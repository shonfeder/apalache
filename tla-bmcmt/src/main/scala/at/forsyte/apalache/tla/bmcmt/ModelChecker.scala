package at.forsyte.apalache.tla.bmcmt

import java.io.{File, PrintWriter, StringWriter}
import java.util.concurrent.TimeUnit

import at.forsyte.apalache.tla.bmcmt.rules.aux.{CherryPick, MockOracle, Oracle}
import at.forsyte.apalache.tla.bmcmt.search._
import at.forsyte.apalache.tla.bmcmt.types._
import at.forsyte.apalache.tla.bmcmt.util.TlaExUtil
import at.forsyte.apalache.tla.imp.src.SourceStore
import at.forsyte.apalache.tla.lir._
import at.forsyte.apalache.tla.lir.storage.{ChangeListener, SourceLocator}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.HashMap

/**
  * <p>A bounded model checker using SMT. The checker implements symbolic breadth-first search with splitting.
  * The TLA+ specification of the search algorithm is available in `./docs/specs/search/ParBMC.tla`.
  * </p>
  *
  * @author Igor Konnov
  */
class ModelChecker(val checkerInput: CheckerInput,
                   val params: ModelCheckerParams,
                   val sharedState: SharedSearchState,
                   var context: WorkerContext,
                   changeListener: ChangeListener,
                   sourceStore: SourceStore)
  extends Checker with LazyLogging {

  import Checker._

  class CancelSearchException(val outcome: Outcome.Value) extends Exception

  /**
    * For how the workers should try to wait for others in the initialization phase.
    */
  val INIT_BARRIER_TIMEOUT_SEC = 10

  /**
    * When a worker thread has nothing to do, it sleep for that number of seconds.
    */
  private val SLEEP_DURATION_MS = 500

  /**
    * Check all executions of a TLA+ specification up to a bounded number of steps.
    *
    * @return a verification outcome
    */
  def run(): Outcome.Value = {
    val initialArena = Arena.create(context.solver)
    val dummyState = new SymbState(initialArena.cellTrue().toNameEx, initialArena, Binding())
    val outcome =
      try {
        logger.info(s"Worker ${context.rank} is initializing")
        if (context.rank == 1) {
          // the leading worker is initializing the shared state
          val initConstState = initializeConstants(dummyState)
          // push state 0 -- the state after initializing the parameters
          val statuses = checkerInput.initTransitions.zipWithIndex map { case (e, i) => (i, (e, NewTransition())) }

          context.activeNode.synchronized {
            context.activeNode = sharedState.searchRoot // start with the root
            context.activeNode.isChecked = true // nothing to check, as Init has not been applied yet
            saveContextInActiveNode(initConstState, new MockOracle(0)) // save for later recovery
            // the root node contains the list of transitions to be explored
            context.activeNode.openTransitions = HashMap(statuses: _*)
            context.activeNode.jailTimeoutSec = params.jailTimeoutMinSec // set the initial timeout
          }
        }

        // register the worker
        sharedState.synchronized {
          context.activeNode = sharedState.searchRoot // start with the root
          context.workerState = IdleState()
          sharedState.workerStates += context.rank -> IdleState() // register as idle
        }

        // wait for the other workers to get initialized
        sharedState.barrier.await(INIT_BARRIER_TIMEOUT_SEC, TimeUnit.SECONDS)
        logger.info(s"Worker ${context.rank} started")

        // run the worker's loop
        searchLoop()
      } catch {
        case _: CancelSearchException =>
          Outcome.Error

        case err: CheckerException =>
          // try to get any info about the problematic source location
          printRewriterSourceLoc()
          throw err
      }
    // flush the logs
    context.rewriter.dispose()
    outcome
  }

  /**
    * Use the provided expression to initialize the constants
    *
    * @param state an initial state
    * @return a new state with the constants initialized
    */
  private def initializeConstants(state: SymbState): SymbState = {
    val newState =
      if (checkerInput.constInitPrimed.isEmpty) {
        logger.info(s"Worker ${context.rank}: No CONSTANT initializer given")
        state
      } else {
        logger.info(s"Worker ${context.rank}: Initializing CONSTANTS with the provided operator")
        checkTypes(checkerInput.constInitPrimed.get)
        val nextState = context.rewriter.rewriteUntilDone(state.setRex(checkerInput.constInitPrimed.get))
        // importantly, assert the constraints that are imposed by the expression
        context.solver.assertGroundExpr(nextState.ex)
        // as the initializer was defined over the primed versions of the constants, shift them back to non-primed
        shiftTypes(Set())
        nextState.setBinding(nextState.binding.shiftBinding(Set()))
      }

    val constants = checkerInput.rootModule.constDeclarations.map(_.name)
    val uninitialized = constants.filter(n => !newState.binding.contains(n))
    if (uninitialized.nonEmpty) {
      logger.error("Worker %d: The following CONSTANTS are not initialized: %s".
        format(context.rank, uninitialized.mkString(", ")))
      throw new CancelSearchException(Checker.Outcome.RuntimeError)
    }
    newState
  }

  /**
    * The search is running in the loop until the worker has reached a final state.
    *
    * @return the outcome of the search
    */
  private def searchLoop(): Outcome.Value = {
    // this is the choice of actions
    def allFinishedOrBuggy(): Boolean = {
      sharedState.synchronized {
        sharedState.workerStates.values.exists(_ == BuggyState()) ||
          sharedState.workerStates.values.forall(_.isFinished)
      }
    }

    while (!allFinishedOrBuggy()) {
      // try to apply one of the actions, similar to what we see in the TLA+ spec
      // TODO: introduce a function that checks the preconditions and selects one action
      // TODO: the current implementation has too many collisions on the locks
      val appliedAction =
      borrowTransition() || checkOneTransition() || proveOneCondition() || increaseDepth() ||
        closeDisabledNode() || switchNode() || finishBugFree()

      if (!appliedAction) {
        // no action applicable, the worker waits and then continues
        Thread.sleep(SLEEP_DURATION_MS)
      }
    }

    // return the result
    sharedState.synchronized {
      val workerState = sharedState.workerStates(context.rank)
      workerState match {
        case BugFreeState() => Outcome.NoError
        case BuggyState() => Outcome.Error
        case _ =>
          if (sharedState.workerStates.values.exists(_ == BuggyState())) {
            Outcome.Error
          } else {
            throw new CheckerException("Worker %d: unexpected state upon finishing: %s".format(context.rank, workerState), NullEx)
          }
      }
    }
  }

  /**
    * Try to borrow a transition for feasibility checking.
    *
    * TODO: merge it with CheckOneTransition?
    */
  private def borrowTransition(): Boolean = {
    if (context.workerState != IdleState()) {
      return false
    }
    context.activeNode.synchronized {
      context.activeNode.openTransitions.find {
        _._2._2 == NewTransition()
      } match {
        case Some((no, (ex, _))) =>
          logger.debug(s"Worker ${context.rank} borrowed transition $no from node ${context.activeNode.id}")
          context.activeNode.openTransitions += (no -> (ex, BorrowedTransition()))
          context.workerState = ExploringState(no, ex)
          sharedState.workerStates += context.rank -> context.workerState
          true

        case None => false
      }
    }
  }

  /**
    * Serialize the context and save it to the file that is named after the active node.
    *
    * This method must be called under a lock on the node!
    */
  private def saveContextInActiveNode(state: SymbState, oracle: Oracle): Unit = {
    assert(context.activeNode.snapshot.isEmpty)
    context.activeNode.snapshot = Some(context.makeSnapshot(state, oracle))
  }

  /**
    * Recover the worker context from a snapshot.
    *
    * @param node the root of the tree to use for recovery
    */
  private def recoverContext(node: HyperNode): Unit = {
    logger.debug(s"Worker ${context.rank} is synchronizing with tree node ${node.id}")
    context.dispose() // free the resources
    // XXX: using a type cast
    context = WorkerContext.recover(context.rank, node, params, context.rewriter.asInstanceOf[SymbStateRewriterImpl])
    //    context = WorkerContext.load(getNodeFile(tree), context.rank)
    context.solver.log(";;;;;;;;;;;;; RECOVERY FROM node %d".format(context.activeNode.id))
  }

  private def checkOneTransition(): Boolean = {
    context.workerState match {
      case ExploringState(trNo, trEx) if !params.stepMatchesFilter(context.stepNo, trNo) =>
        // the transition does not match the filter, skip
        context.activeNode.synchronized {
          context.activeNode.openTransitions -= trNo
          context.activeNode.closedTransitions += trNo -> (trEx, DisabledTransition(1))
        }
        sharedState.synchronized {
          context.workerState = IdleState()
          sharedState.workerStates += context.rank -> context.workerState
        }
        true

      case ExploringState(trNo, trEx) =>
        // check the borrowed transition
        recoverContext(context.activeNode) // recover the context
      // do not push, keep the context offline, recover from the snapshot later
      // If the transition is not in jail, set a small timeout. Otherwise, no timeout.
      val timeout = if (context.activeNode.transition.isJailed) 0 else context.activeNode.jailTimeoutSec
        val (_, status) = applyOneTransition(context.stepNo, context.state, trEx, trNo, timeout, checkForErrors = true)
        context.rewriter.exprCache.disposeActionLevel() // leave only the constants

        status match {
          case TimedOutTransition(durationMs) =>
            // the transition timed out, isolate it in its own group
            recoverContext(context.activeNode) // recover the context, as it was updated by applyTransition
            context.activeNode.synchronized {
              context.activeNode.openTransitions -= trNo
              val transition = HyperTransition(trNo)
              transition.isJailed = true
              val newNode = HyperNode(transition)
              logger.info(s"Worker ${context.rank}: INTRODUCED NODE ${newNode.id} after timeout of $durationMs ms on transition $trNo")
              newNode.openTransitions = Map(trNo -> (trEx, NewTransition()))
              val activeSnapshot = context.activeNode.snapshot.get
              newNode.snapshot = Some(context.makeSnapshot(activeSnapshot.state, activeSnapshot.oracle))
              context.activeNode.parent match {
                case Some(parent) => parent.append(newNode)
                case None => context.activeNode.append(newNode) // this should not happen actually

              }
              logger.debug(s"Worker ${context.rank}: Transition $trNo timed out. Isolated it in node ${newNode.id}")
            }
            sharedState.synchronized {
              context.workerState = IdleState()
              sharedState.workerStates += context.rank -> context.workerState
            }

          case _ =>
            // the transition has been checked
            context.activeNode.synchronized {
              context.activeNode.openTransitions -= trNo
              context.activeNode.closedTransitions += trNo -> (trEx, status)
            }
            sharedState.synchronized {
              context.workerState = IdleState()
              sharedState.workerStates += context.rank -> context.workerState
            }
            logger.debug(s"Worker ${context.rank}: Transition $trNo is $status (elapsed time ${status.elapsedMs} ms).")
        }

        true // the action ran successfully

      case _ => false
    }
  }

  private def proveOneCondition(): Boolean = {
    if (context.workerState != IdleState()) {
      return false
    }
    // find a condition, if there is any
    val (vcNo, vc) =
      context.activeNode.synchronized {
        context.activeNode.unprovenVCs.find {
          case (_, NewVC(_)) => true
          case _ => false
        } match {
          case Some((no, NewVC(vc@InvariantVC(_)))) =>
            logger.debug(s"Worker ${context.rank} started to prove VC $no in node ${context.activeNode.id}")
            context.activeNode.unprovenVCs += (no -> InProgressVC(vc))
            context.workerState = ProvingState(no, vc)
            (no, vc)

          case Some((no, vc@NewVC(_))) =>
            throw new NotImplementedError(s"Unsupported verification condition: $vc")

          case _ =>
            return false
        }
      }

    // tell the others
    sharedState.synchronized {
      sharedState.workerStates += context.rank -> context.workerState
    }

    // check the condition
    val startTimeMs = System.currentTimeMillis()
    recoverContext(context.activeNode) // recover the context
    val result = checkOneInvariant(context.activeNode.depth, context.state, vcNo, vc)
    val diffMs = System.currentTimeMillis() - startTimeMs
    val newState = result match {
      case InvalidVC(_) =>
        logger.info(s"Worker ${context.rank}: VC $vcNo is violated in $diffMs ms. Going to the buggy state.")
        BuggyState()

      case _ =>
        logger.debug(s"Worker ${context.rank}: Proven VC $vcNo at node ${context.activeNode.id} in $diffMs ms ")
        IdleState()
    }

    val activeNode = context.activeNode
    activeNode.synchronized {
      activeNode.provenVCs += (vcNo -> result)
      activeNode.unprovenVCs -= vcNo
      if (activeNode.unprovenVCs.isEmpty) {
        activeNode.isChecked = true
      }
      context.workerState = newState
    }
    sharedState.synchronized {
      sharedState.workerStates += context.rank -> context.workerState
    }
    true
  }

  /**
    * If the current node is completely explored and checked, switch the node.
    *
    * @return true, if successful
    */
  private def switchNode(): Boolean = {
    if (context.workerState != IdleState()) {
      return false
    }

    def isNodeUnexploredOrNotChecked(node: HyperNode): Boolean = {
      val yetToExplore = !node.isExplored && node.openTransitions.values.exists(_._2 == NewTransition())
      val yetToProve = !node.isChecked && node.unprovenVCs.values.exists(_.isInstanceOf[NewVC])
      yetToExplore || yetToProve
    }

    if (isNodeUnexploredOrNotChecked(context.activeNode)) {
      // stay with the current node
      return false
    }

    // pick a tree node that is yet to be explored (in the top-to-bottom, left-to-right order)
    def findUnfinished(node: HyperNode): Boolean = {
      if (isNodeUnexploredOrNotChecked(node)) {
        // lock the node as it may be concurrently updated
        node.synchronized {
          if (isNodeUnexploredOrNotChecked(node)) { // still unexplored or not checked
            context.activeNode = node
          }
        }
        true
      } else {
        node.children.exists(findUnfinished)
      }
    }

    // start with the root
    if (findUnfinished(sharedState.searchRoot)) {
      logger.debug(s"Worker ${context.rank} switched to node ${context.activeNode.id}")
      true
    } else {
      false
    }
  }

  private def closeDisabledNode(): Boolean = {
    if (context.workerState != IdleState() || context.activeNode.isExplored) {
      return false // the worker is busy or the node has been explored, nothing to check
    }
    context.activeNode.synchronized {
      if (context.activeNode.openTransitions.isEmpty) {
        logger.info("Worker %d: CLOSING NODE %d".format(context.rank, context.activeNode.id))
        context.activeNode.isExplored = true
        true
      } else {
        false
      }
    }
  }

  private def increaseDepth(): Boolean = {
    val reachedCeiling = context.stepNo >= params.stepsBound
    var enabled: List[(TlaEx, Int)] = List()

    if (context.workerState != IdleState() || context.activeNode.isExplored) {
      return false // the worker is busy or the node has been explored, nothing to check
    }

    // an arbitrary worker may close its active node, saves the SMT context, and other workers have to catch up
    context.activeNode.synchronized {
      enabled = context.activeNode.closedTransitions.
        collect({ case (trNo, (trEx, EnabledTransition(_, _))) => (trEx, trNo) }).toList
      val allExplored = context.activeNode.openTransitions.isEmpty
      if (context.activeNode.isExplored || enabled.isEmpty || !allExplored) {
        return false
      }
      // close the tree node, nothing to explore in this node
      logger.info("Worker %d: CLOSING NODE %d".format(context.rank, context.activeNode.id))
      recoverContext(context.activeNode)

      // introduce a new node, unless the depth is too high
      val newNode = HyperNode(HyperTransition(enabled.map(_._2): _*))
      // recompute the jail timeout
      val newJailTimeout = 1 + context.activeNode.maxTransitionTimeMs() * params.jailTimeoutFactor / 100000
      newNode.jailTimeoutSec = Math.max(newJailTimeout, params.jailTimeoutMinSec)
      // Find all verification conditions. By converting it to Map, we remove the duplicates
      val allVCsMap = context.activeNode.closedTransitions.
        collect({ case (_, (_, EnabledTransition(_, vcs))) => vcs }).
        flatten.map(p => (p._2, p._1)).
        toMap

      // schedule the verification conditions
      newNode.unprovenVCs = allVCsMap map { case (no, vc) => (no, NewVC(vc)) }
      if (newNode.unprovenVCs.isEmpty) {
        newNode.isChecked = true // nothing to check
      }

      // schedule the transitions to check
      if (!reachedCeiling) {
        val statuses = checkerInput.nextTransitions.zipWithIndex map { case (e, i) => (i, (e, NewTransition())) }
        newNode.openTransitions = HashMap(statuses: _*)
      } else {
        logger.info(s"Worker ${context.rank}: node ${newNode.id} is at max depth, only invariants will be checked")
        newNode.isExplored = true
      }
      newNode.synchronized {
        context.activeNode.append(newNode)
        // apply the transitions and push the new state in the new node
        // TODO: can we do that outside of the critical section?
        val (nextState, nextOracle) = applyEnabledThenPush(context.stepNo, context.state, enabled)
        // mark the node as explored in the end, so the finishing actions do not prematurely terminate
        context.activeNode.isExplored = true
        // switch to the new node
        context.activeNode = newNode
        saveContextInActiveNode(nextState, nextOracle)
      }
      logger.info("Worker %d: INTRODUCED NODE %d with %d open transitions".
        format(context.rank, context.activeNode.id, context.activeNode.openTransitions.size))
    }

    true
  }

  private def finishBugFree(): Boolean = {
    def allExploredAndChecked(node: HyperNode): Boolean = {
      // we do not need a lock here, as isExplored is changing once from false to true (it can be checked again later)
      node.isExplored && node.isChecked && node.children.forall(allExploredAndChecked)
    }

    sharedState.synchronized {
      val allIdle = sharedState.workerStates.values.forall(ws => ws == IdleState() || ws.isFinished)
      if (allIdle && allExploredAndChecked(sharedState.searchRoot)) {
        sharedState.workerStates += context.rank -> BugFreeState()
        true
      } else {
        false
      }
    }
  }

  private def applyEnabledThenPush(stepNo: Int, startingState: SymbState,
                                   transitionsWithIndex: List[(TlaEx, Int)]): (SymbState, Oracle) = {
    // second, apply the enabled transitions and collect their effects
    logger.info("Worker %d: Step %d, level %d: collecting %d enabled transition(s)"
      .format(context.rank, stepNo, context.rewriter.contextLevel, transitionsWithIndex.size))
    assert(transitionsWithIndex.nonEmpty)

    def applyTrans(state: SymbState, ts: List[(TlaEx, Int)]): List[SymbState] =
      ts match {
        case List() =>
          List(state) // the final state may contain additional cells, add it

        case (transition, transitionNo) :: tail =>
          val erased = state.setBinding(state.binding.forgetPrimed)
          // note that the constraints are added at the current level, without an additional push
          val (nextState, _) = applyOneTransition(stepNo, erased, transition, transitionNo, 0, checkForErrors = false)
          context.rewriter.exprCache.disposeActionLevel() // leave only the constants
          // collect the variables of the enabled transition
          nextState +: applyTrans(nextState, tail)
      }

    val producedStates = applyTrans(startingState, transitionsWithIndex)
    // the last state contains the latest arena
    val lastState = producedStates.last
    val statesAfterTransitions = producedStates.slice(0, producedStates.length - 1)

    // pick an index j \in { 0..k } of the fired transition
    val picker = new CherryPick(context.rewriter)
    val (oracleState, oracle) = picker.oracleFactory.newDefaultOracle(lastState, statesAfterTransitions.length)

    if (statesAfterTransitions.isEmpty) {
      throw new IllegalArgumentException("enabled must be non-empty")
    } else if (statesAfterTransitions.lengthCompare(1) == 0) {
      val resultingState = oracleState.setBinding(lastState.binding.shiftBinding(params.consts))
      context.solver.assertGroundExpr(lastState.ex)
      shiftTypes(params.consts)
      (resultingState, oracle)
    } else {
      // if oracle = i, then the ith transition is enabled
      context.solver.assertGroundExpr(oracle.caseAssertions(oracleState, statesAfterTransitions.map(_.ex)))

      // glue the computed states S_0, ..., S_k together:
      // for every variable x', pick c_x from { S_1[x'], ..., S_k[x'] }
      //   and require \A i \in { 0.. k-1}. j = i => c_x = S_i[x']
      // Then, the final state binds x' -> c_x for every x' \in Vars'
      def getAssignedVars(st: SymbState) = st.binding.forgetNonPrimed(Set()).toMap.keySet

      val primedVars = getAssignedVars(statesAfterTransitions.head) // only VARIABLES, not CONSTANTS
      var finalState = oracleState
      if (statesAfterTransitions.exists(getAssignedVars(_) != primedVars)) {
        // TODO: we should not throw an exception here, but ignore the transitions that do not have all assignments.
        // The reason is that the assignment solver guarantees that every transition has the assignments.
        // The only case when this situation may happen is when the rewriter uses short-circuiting logic.
        val index = statesAfterTransitions.indexWhere(getAssignedVars(_) != primedVars)
        val otherSet = getAssignedVars(statesAfterTransitions(index))
        val diff = otherSet.union(primedVars).diff(otherSet.intersect(primedVars))
        val msg =
          "Worker %d: [Step %d] Next states 0 and %d disagree on the set of assigned variables: %s"
            .format(context.rank, stepNo, index, diff.mkString(", "))
        throw new InternalCheckerError(msg, finalState.ex)
      }

      def pickVar(x: String): ArenaCell = {
        val toPickFrom = statesAfterTransitions map (_.binding(x))
        finalState = picker.pickByOracle(finalState,
          oracle, toPickFrom, finalState.arena.cellFalse().toNameEx) // no else case
        finalState.asCell
      }

      val finalVarBinding = Binding(primedVars.toSeq map (n => (n, pickVar(n))): _*) // variables only
      val constBinding = Binding(oracleState.binding.toMap.filter(p => params.consts.contains(p._1)))
      finalState = finalState.setBinding(finalVarBinding ++ constBinding)
      if (params.debug && !context.solver.sat()) {
        throw new InternalCheckerError(s"Error picking next variables (step $stepNo). Report a bug.", finalState.ex)
      }
      // finally, shift the primed variables to non-primed
      finalState = finalState.setBinding(finalState.binding.shiftBinding(params.consts))
      // that is the result of this step
      shiftTypes(params.consts)
      // here we save the transition index, not the oracle, which will be shown to the user
      (finalState, oracle)
    }
  }

  // warning: this method updates the context and does not recover it
  private def applyOneTransition(stepNo: Int,
                                 state: SymbState,
                                 transition: TlaEx,
                                 transitionNo: Int,
                                 timeout: Long,
                                 checkForErrors: Boolean): (SymbState, TransitionStatus) = {
    logger.debug("Worker %d: Step #%d, transition #%d, SMT context level %d, checking error %b."
      .format(context.rank, stepNo, transitionNo, context.rewriter.contextLevel, checkForErrors))
    logger.debug("Worker %d: Finding types of the variables...".format(context.rank))
    checkTypes(transition)
    context.solver.log("; ------- STEP: %d, STACK LEVEL: %d TRANSITION: %d {"
      .format(stepNo, context.rewriter.contextLevel, transitionNo))
    logger.debug("Worker %d: Applying rewriting rules...".format(context.rank))
    val nextState = context.rewriter.rewriteUntilDone(state.setRex(transition))
    context.rewriter.flushStatistics()

    val assignedVars = nextState.binding.toMap.
      collect { case (name, _) if name.endsWith("'") => name.substring(0, name.length - 1)}
    if (assignedVars.toSet != params.vars) {
      logger.debug(s"Worker ${context.rank}: Transition $transitionNo produces partial assignment. Disabled.")
      return (nextState, DisabledTransition(1))
    }

    if (params.lucky) {
      // feeling lucky, do not check whether the transition is enabled
      logger.debug(s"Worker ${context.rank}: Feeling lucky. Transition $transitionNo is enabled.")
      return (nextState, EnabledTransition(1, findTransitionInvariants(stepNo, transitionNo, nextState)))
    }

    if (checkForErrors && params.debug) {
      // This is a debugging feature that allows us to find incorrect rewriting rules.
      // Disable it in production.
      logger.debug("Worker %d: Finished rewriting. Checking satisfiability of the pushed constraints.".
        format(context.rank))
      context.solver.satOrTimeout(timeout) match {
        case Some(false) =>
          // this is a clear sign of a bug in one of the translation rules
          logger.debug("Worker %d: UNSAT after pushing transition constraints".format(context.rank))
          throw new CheckerException("A contradiction introduced in rewriting. Report a bug.", state.ex)

        case Some(true) => () // SAT
          logger.debug("Worker %d: The transition constraints are OK.".format(context.rank))

        case None => // interpret it as sat
          logger.debug("Worker %d: Timeout. Assuming the transition constraints to be OK.".format(context.rank))
      }
    }
    if (!checkForErrors) {
      // just return the state, no verification conditions
      (nextState, EnabledTransition(1, List.empty))
      // LEVEL + 0
    } else {
      // assume the constraint constructed by this transition
      context.solver.assertGroundExpr(nextState.ex)
      // check whether this transition violates some assertions
      logger.debug("Checking transition feasibility.")
      val startTimeMs = System.currentTimeMillis()
      context.solver.satOrTimeout(timeout) match {
        case Some(true) =>
          val durationMs = System.currentTimeMillis() - startTimeMs
          val vcs = findTransitionInvariants(stepNo, transitionNo, nextState)
          context.solver.log("; } ------- STEP: %d, STACK LEVEL: %d".format(stepNo, context.rewriter.contextLevel))
          (nextState, EnabledTransition(durationMs, vcs))

        case r: Option[Boolean] => // unsat or timeout
          val durationMs = System.currentTimeMillis() - startTimeMs
          if (r.isDefined) { // the transition is not feasible in the current symbolic state
            logger.debug("Worker %d: Transition #%d is not feasible.".format(context.rank, transitionNo))
            context.solver.log("; } ------- STEP: %d, STACK LEVEL: %d TRANSITION: %d"
              .format(stepNo, context.rewriter.contextLevel, transitionNo))
            (nextState, DisabledTransition(durationMs))
          } else { // it takes too long to check the transition
            logger.debug("Worker %d: Transition %d is slow. It should be separated.".format(context.rank, transitionNo))
            (nextState, TimedOutTransition(durationMs))
          }
      }
    }
  }

  // schedule the invariants to be checked after the transition was fired
  private def findTransitionInvariants(stepNo: Int,
                                       transitionNo: Int,
                                       nextState: SymbState): List[(VerificationCondition, Int)] = {
    // skip the check if the user told us to skip the invariant at this step
    val matchesInvFilter = params.invFilter == "" || stepNo.toString.matches("^" + params.invFilter + "$")
    if (!matchesInvFilter) {
      return List.empty
    }

    // if the previous step was filtered, we cannot use the unchanged optimization
    val prevMatchesInvFilter =
      params.invFilter == "" || (stepNo - 1).toString.matches("^" + params.invFilter + "$")
    val changedPrimed =
      if (prevMatchesInvFilter) {
        // only check an invariant if it touches the changed variables
        nextState.changed
      } else {
        // check an invariant in any case, as it could be violated at the previous step (the invariant was filtered before)
        nextState.binding.toMap.keySet
      }

    // is the invariant non-trivial, that is, it refers to the changed variables
    def refersToChanged(notInv: TlaEx): Boolean = {
      // add primes as the invariant is referring to the unprimed variables
      val used = TlaExUtil.findUsedNames(notInv).map(_ + "'")
      used.intersect(changedPrimed).nonEmpty
    }

    // keep the invariant negations that refer to the changed variables
    val invNegs = checkerInput.invariantsAndNegations.map(_._2).zipWithIndex.filter(p => refersToChanged(p._1))
    if (invNegs.nonEmpty) {
      logger.debug("Worker %d, step %d, transition %d: scheduled invariants %s"
        .format(context.rank, stepNo, transitionNo, invNegs.map(_._2).mkString(", ")))
      invNegs.map(p => (InvariantVC(p._1), p._2))
    } else {
      logger.debug("Worker %d, step %d, transition %d: no invariants to check"
        .format(context.rank, stepNo, transitionNo))
      List.empty
    }
  }

  private def checkOneInvariant(stepNo: Int,
                                state: SymbState,
                                vcNo: Int,
                                vc: InvariantVC): VCStatus = {
    // check the types first
    checkTypes(vc.notInv)
    val notInvState = context.rewriter.rewriteUntilDone(state.setRex(vc.notInv))
    context.solver.assertGroundExpr(notInvState.ex)
    context.solver.satOrTimeout(params.invariantTimeout) match {
      case Some(true) =>
        // TODO: take a snapshot and return InvalidVC instead?
        val filename = "counterexample.txt"
        context.dumpCounterexample(filename)
        logger.error("Worker %d: Invariant %d is violated at depth %d. Check the counterexample in %s".
          format(context.rank, vcNo, stepNo, filename))
        if (params.debug) {
          logger.warn("Worker %d: Dumping the arena into smt.log. This may take some time...".format(context.rank))
          // dump everything in the log
          val writer = new StringWriter()
          new SymbStateDecoder(context.solver, context.rewriter).dumpArena(notInvState, new PrintWriter(writer))
          context.solver.log(writer.getBuffer.toString)
        }

        InvalidVC(vc)

      case Some(false) =>
        logger.debug(s"Worker ${context.rank}: The invariant $vcNo holds true.")
        ValidVC(vc)

      case None =>
        logger.debug(s"Worker ${context.rank}: Timeout when checking invariant $vcNo.")
        UnknownVC(vc)
    }
  }

  private def checkTypes(expr: TlaEx): Unit = {
    context.typeFinder.inferAndSave(expr)
    if (context.typeFinder.getTypeErrors.nonEmpty) {
      def print_error(e: TypeInferenceError): Unit = {
        val sourceLocator: SourceLocator = SourceLocator(sourceStore.makeSourceMap, changeListener)

        val locInfo =
          sourceLocator.sourceOf(e.origin) match {
            case Some(loc) => loc.toString
            case None => "<unknown origin>"
          }
        val exStr = e.origin match {
          case OperEx(op, _*) => op.name + "(...)"
          case ex@_ => ex.toString()
        }
        logger.error("%s, %s, type error: %s".format(locInfo, exStr, e.explanation))
      }

      context.typeFinder.getTypeErrors foreach print_error
      throw new CancelSearchException(Outcome.Error)
    }
  }

  /**
    * Remove the non-primed variables (except provided constants)
    * and rename the primed variables to their non-primed versions.
    * After that, remove the type finder to contain the new types only.
    */
  private def shiftTypes(constants: Set[String]): Unit = {
    val types = context.typeFinder.getVarTypes
    val nextTypes =
      types.filter(p => p._1.endsWith("'") || constants.contains(p._1))
        .map(p => (p._1.stripSuffix("'"), p._2))
    context.typeFinder.reset(nextTypes)
  }

  private def getNodeFile(tree: HyperNode): File = {
    new File(params.saveDirectory, "%d.ser".format(tree.id))
  }

  private def printRewriterSourceLoc(): Unit = {
    def getSourceLocation(ex: TlaEx) = {
      val sourceLocator: SourceLocator = SourceLocator(
        sourceStore.makeSourceMap,
        changeListener
      )
      sourceLocator.sourceOf(ex)
    }

    context.rewriter.getRewritingStack().find(getSourceLocation(_).isDefined) match {
      case None =>
        logger.error("Unable find the source of the problematic expression")

      case Some(ex) =>
        val loc = getSourceLocation(ex).get
        logger.error(s"The problem occurs around the source location $loc")
    }
  }
}
