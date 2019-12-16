package at.forsyte.apalache.tla.bmcmt

import java.io.{File, PrintWriter, StringWriter}

import at.forsyte.apalache.tla.bmcmt.rules.aux.{CherryPick, MockOracle}
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
    * Check all executions of a TLA+ specification up to a bounded number of steps.
    *
    * @return a verification outcome
    */
  def run(): Outcome.Value = {
    val initialArena = Arena.create(context.solver)
    val dummyState = new SymbState(initialArena.cellTrue().toNameEx, CellTheory(), initialArena, Binding())
    val outcome =
      try {
        val initConstState = initializeConstants(dummyState)
        // push state 0 -- the state after initializing the parameters
        context.push(initConstState, new MockOracle(0), context.typeFinder.getVarTypes)
        val statuses = checkerInput.initTransitions.zipWithIndex map { case (e, i) => (i, (e, NewTransition())) }

        if (context.rank == 1) {
          // the leading worker is initializing the shared state
          sharedState.synchronized {
            sharedState.transitions = HashMap(statuses: _*)
          }
        }

        // register the worker
        sharedState.synchronized {
          sharedState.workerStates += context.rank -> IdleState()
        }

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
    def allFinished(): Boolean = {
      sharedState.synchronized {
        sharedState.workerStates(context.rank).isFinished
      }
    }

    while (!allFinished()) {
      // try to apply one of the actions, similar to what we see in the TLA+ spec
      val appliedAction = borrowTransition() || checkOneTransition() || increaseDepth() || finishBugFree()
      if (!appliedAction) {
        // no action applicable, the worker waits and then continues
        Thread.sleep(100)
      }
    }

    // return the result
    sharedState.synchronized {
      val workerState = sharedState.workerStates(context.rank)
      workerState match {
        case BugFreeState() => Outcome.NoError
        case BuggyState() => Outcome.Error
        case _ =>
          throw new CheckerException("Worker %d: unexpected state upon finishing: %s".format(context.rank, workerState), NullEx)
      }
    }
  }

  /**
    * Try to borrow a transition for feasibility checking.
    */
  private def borrowTransition(): Boolean = {
    sharedState.synchronized {
      if (sharedState.workerStates(context.rank) != IdleState()) {
        return false
      }
      sharedState.transitions.find {
        _._2._2 == NewTransition()
      } match {
        case Some((no, (ex, _))) =>
          sharedState.transitions += (no -> (ex, BorrowedTransition()))
          sharedState.workerStates += context.rank -> ExploringState(no, ex)
          true

        case None => false
      }
    }
  }

  private def checkOneTransition(): Boolean = {
    val workerState = sharedState.synchronized {
      sharedState.workerStates(context.rank)
    }
    workerState match {
      case ExploringState(trNo, trEx) if !params.stepMatchesFilter(context.stepNo, trNo) =>
        // the transition does not match the filter, skip
        sharedState.synchronized {
          sharedState.transitions += trNo -> (trEx, DisabledTransition())
          sharedState.workerStates += context.rank -> IdleState()
        }
        true

      case ExploringState(trNo, trEx) =>
        // check the borrowed transition
        val state = context.state
        context.typeFinder.reset(context.types) // set the variable type as they should be at this step
      val erased = state.setBinding(state.binding.forgetPrimed)
        context.rewriter.push() // LEVEL + 1
      val (_, isEnabled) = applyOneTransition(context.stepNo, erased, trEx, trNo, checkForErrors = true)
        context.rewriter.exprCache.disposeActionLevel() // leave only the constants
        context.rewriter.pop() // forget all the constraints that were generated by the transition, LEVEL + 0
        context.typeFinder.reset(context.types)
        // TODO: handle timeouts!
        val newStatus = if (isEnabled) EnabledTransition() else DisabledTransition()
        sharedState.transitions.synchronized {
          sharedState.transitions += trNo -> (trEx, newStatus)
          sharedState.workerStates += context.rank -> IdleState()
        }
        // TODO: schedule invariant checking (happening in applyTransition now...)
        true // the action ran successfully

      case _ => false
    }
  }

  private def increaseDepth(): Boolean = {
    val reachedCeiling = context.stepNo >= params.stepsBound
    var enabled: List[(TlaEx, Int)] = List()
    if (context.rank == 1) { // only the leader may increase the depth
      sharedState.synchronized {
        enabled = sharedState.transitions.
          collect({ case (trNo, (trEx, EnabledTransition())) => (trEx, trNo) }).toList
        val allUnexplored = sharedState.transitions forall {
          _._2._2.isExplored
        }
        if (reachedCeiling || enabled.isEmpty || !allUnexplored) {
          return false
        }
        // TODO: handle slow prefixes
        // add next transitions to explore
        val statuses = checkerInput.nextTransitions.zipWithIndex map { case (e, i) => (i, (e, NewTransition())) }
        sharedState.transitions = HashMap(statuses: _*)
        sharedState.extendActiveNode(HyperTransition(enabled.map(_._2) :_*))
      }

      // the leader advances the search tree, saves the SMT context, and other workers have to catch up
      val state = context.state
      context.typeFinder.reset(context.types) // set the variable type as they should be at this step
      val stateWithNoPrimes = state.setBinding(state.binding.forgetPrimed)
      val result = applyEnabled(context.stepNo, stateWithNoPrimes, enabled)
      // NOTE: applyEnabled has pushed the state and types on the stack, so we do not have to do it
      val savefile = new File(params.saveDirectory, "%d.ser".format(context.activeNode.id))
      context.saveToFile(savefile)
      // recover the context from the file, as it will smash the SMT context
      // TODO: uncomment!
//      context = WorkerContext.load(savefile, context.rank)
      assert(result.isDefined)
      true
    } else {
      false
    }
  }

  private def finishBugFree(): Boolean = {
    sharedState.synchronized {
      val allIdle = sharedState.workerStates.values.forall(_ == IdleState())
      val allDisabled = sharedState.transitions.valuesIterator.forall(_._2 == DisabledTransition())
      val allEnabledOrDisabled =
        sharedState.transitions.valuesIterator.forall {
          case (_, EnabledTransition()) | (_, DisabledTransition()) => true
          case _ => false
        }
      // TODO: check unsafePrefixes
      // TODO: check slowPrefixes
      if (allIdle && (allDisabled || (allEnabledOrDisabled && context.stepNo == params.stepsBound))) {
        sharedState.workerStates += context.rank -> BugFreeState()
        true
      } else {
        false
      }
    }
  }

  // TODO: add StartProving
  // TODO: add ProveInvariant
  // TODO: add FindSlowPrefixes
  // TODO: add WhenAllDisabledButNotFinished

  private def applyEnabled(stepNo: Int, startingState: SymbState,
                           transitionsWithIndex: List[(TlaEx, Int)]): Option[SymbState] = {
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
          val (nextState, _) = applyOneTransition(stepNo, erased, transition, transitionNo, checkForErrors = false)
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
      val resultingState = oracleState.setBinding(lastState.binding.shiftBinding(params.constants))
      context.solver.assertGroundExpr(lastState.ex)
      shiftTypes(params.constants)
      context.push(resultingState, oracle, context.typeFinder.getVarTypes)
      Some(resultingState)
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
        // TODO: we should not throw an exception here, but ignore the transitions that does not have all assignments.
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
      val constBinding = Binding(oracleState.binding.toMap.filter(p => params.constants.contains(p._1)))
      finalState = finalState.setBinding(finalVarBinding ++ constBinding)
      if (params.debug && !context.solver.sat()) {
        throw new InternalCheckerError(s"Error picking next variables (step $stepNo). Report a bug.", finalState.ex)
      }
      // finally, shift the primed variables to non-primed
      finalState = finalState.setBinding(finalState.binding.shiftBinding(params.constants))
      // that is the result of this step
      shiftTypes(params.constants)
      // here we save the transition index, not the oracle, which will be shown to the user
      context.push(finalState, oracle, context.typeFinder.getVarTypes)
      Some(finalState)
    }
  }

  // This method adds constraints right in the current context, without doing push
  private def applyOneTransition(stepNo: Int, state: SymbState, transition: TlaEx,
                                 transitionNo: Int, checkForErrors: Boolean): (SymbState, Boolean) = {
    logger.debug("Worker %d: Step #%d, transition #%d, SMT context level %d, checking error %b."
      .format(context.rank, stepNo, transitionNo, context.rewriter.contextLevel, checkForErrors))
    logger.debug("Worker %d: Finding types of the variables...".format(context.rank))
    checkTypes(transition)
    context.solver.log("; ------- STEP: %d, STACK LEVEL: %d TRANSITION: %d {"
      .format(stepNo, context.rewriter.contextLevel, transitionNo))
    logger.debug("Worker %d: Applying rewriting rules...".format(context.rank))
    var nextState = context.rewriter.rewriteUntilDone(state.setTheory(CellTheory()).setRex(transition))
    context.rewriter.flushStatistics()
    if (checkForErrors && params.debug) {
      // This is a debugging feature that allows us to find incorrect rewriting rules.
      // Disable it in production.
      logger.debug("Worker %d: Finished rewriting. Checking satisfiability of the pushed constraints.".
        format(context.rank))
      context.solver.satOrTimeout(params.transitionTimeout) match {
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
      // just return the state
      (nextState, true)
      // LEVEL + 0
    } else {
      context.rewriter.push() // LEVEL + 1
      // assume the constraint constructed by this transition
      context.solver.assertGroundExpr(nextState.ex)
      // check whether this transition violates some assertions
      logger.debug("Checking transition feasibility.")
      context.solver.satOrTimeout(params.transitionTimeout) match {
        case Some(true) =>
          // TODO: this should be a separate step
          // check the invariant as soon as one transition has been applied
          checkAllInvariants(stepNo, transitionNo, nextState)
          // and then forget all these constraints!
          context.rewriter.pop() // LEVEL + 0
          context.solver.log("; } ------- STEP: %d, STACK LEVEL: %d".format(stepNo, context.rewriter.contextLevel))
          (nextState, true)
        // LEVEL + 0

        case r: Option[Boolean] => // unsat or timeout
          // the current symbolic state is not feasible
          if (r.isDefined) {
            logger.debug("Worker %d: Transition #%d is not feasible.".format(context.rank, transitionNo))
          } else {
            logger.debug("Worker %d: Timed out when checking feasibility of transition %d. Assuming it is infeasible.".
              format(context.rank, transitionNo))
          }
          context.rewriter.pop() // LEVEL + 0
          context.solver.log("; } ------- STEP: %d, STACK LEVEL: %d TRANSITION: %d"
            .format(stepNo, context.rewriter.contextLevel, transitionNo))
          (nextState, false)
        // LEVEL + 0
      }
    }
  }

  private def checkAllInvariants(stepNo: Int, transitionNo: Int, nextState: SymbState): Unit = {
    val matchesInvFilter = params.invFilter == "" || stepNo.toString.matches("^" + params.invFilter + "$")
    if (!matchesInvFilter) {
      return // skip the check if this transition should not be checked
    }

    // if the previous step was filtered, we cannot use the unchanged optimization
    val prevMatchesInvFilter = params.invFilter == "" ||
      (stepNo - 1).toString.matches("^" + params.invFilter + "$")

    val invNegs = checkerInput.invariantsAndNegations.map(_._2)
    for ((notInv, invNo) <- invNegs.zipWithIndex) {
      logger.debug("Worker %d: Checking the invariant %d".format(context.rank, invNo))
      val changedPrimed =
        if (prevMatchesInvFilter) {
          nextState.changed // only check the invariant if it touches the changed variables
        } else {
          nextState.binding.toMap.keySet // check the invariant in any case, as it could be violated at the previous step
        }
      val savedTypes = context.rewriter.typeFinder.getVarTypes
      // rename x' to x, so we are reasoning about the non-primed variables
      shiftTypes(params.constants)
      val shiftedState = nextState.setBinding(nextState.binding.shiftBinding(params.constants))
      context.rewriter.exprCache.disposeActionLevel() // renaming x' to x makes the cache inconsistent, so clean it
      // check the types and the invariant
      checkTypes(notInv)
      checkOneInvariant(stepNo, transitionNo, shiftedState, changedPrimed, notInv)
      context.rewriter.typeFinder.reset(savedTypes) // forget about the types that were used to check the invariant
    }
  }

  private def checkOneInvariant(stepNo: Int, transitionNo: Int, nextState: SymbState, changedPrimed: Set[String], notInv: TlaEx): Unit = {
    val used = TlaExUtil.findUsedNames(notInv).map(_ + "'") // add primes as the invariant is referring to non-primed variables
    if (used.intersect(changedPrimed).isEmpty) {
      logger.debug(s"Worker %d: The invariant is referring only to the UNCHANGED variables. Skipped.".
        format(context.rank))
    } else {
      context.rewriter.push()
      val notInvState = context.rewriter.rewriteUntilDone(nextState
        .setTheory(CellTheory())
        .setRex(notInv))
      context.solver.assertGroundExpr(notInvState.ex)
      context.solver.satOrTimeout(params.invariantTimeout) match {
        case Some(true) =>
          // introduce a dummy oracle to hold the transition index, we need it for the counterexample
          val oracle = new MockOracle(transitionNo)
          context.push(notInvState, oracle, context.typeFinder.getVarTypes)
          val filename = "counterexample.txt"
          context.dumpCounterexample(filename)
          logger.error("Worker %d: Invariant is violated at depth %d. Check the counterexample in %s".
            format(context.rank, stepNo, filename))
          if (params.debug) {
            logger.warn("Worker %d: Dumping the arena into smt.log. This may take some time...".format(context.rank))
            // dump everything in the log
            val writer = new StringWriter()
            new SymbStateDecoder(context.solver, context.rewriter).dumpArena(notInvState, new PrintWriter(writer))
            context.solver.log(writer.getBuffer.toString)
          }
          // cancel the search
          throw new CancelSearchException(Outcome.Error)

        case Some(false) =>
          logger.debug("Worker %d: The invariant holds true.".format(context.rank))

        case None =>
          logger.debug("Worker %d: Timeout. Assuming that the invariant holds true.".format(context.rank))
      }
      context.rewriter.pop()
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
