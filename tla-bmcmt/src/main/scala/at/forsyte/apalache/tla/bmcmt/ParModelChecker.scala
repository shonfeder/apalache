package at.forsyte.apalache.tla.bmcmt

import java.io.File
import java.util.concurrent.TimeUnit

import at.forsyte.apalache.tla.bmcmt.search._
import at.forsyte.apalache.tla.lir._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.HashMap

/**
  * <p>A bounded model checker with SMT. The checker implements parallel symbolic breadth-first search with splitting.
  * The TLA+ specification of the search algorithm is available in `./docs/specs/search/ParBMC.tla`.
  * </p>
  *
  * <p>TODO: The current implementation uses lots of locks.</p>
  *
  * @author Igor Konnov
  */
class ParModelChecker(val checkerInput: CheckerInput,
                      val params: ModelCheckerParams,
                      val sharedState: SharedSearchState,
                      var context: WorkerContext)
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
    * When a worker has been sleeping for DEADLOCK_TIMEOUT_MS and other workers are idle,
    * report a deadlock in the model checker.
    */
  private val DEADLOCK_TIMEOUT_MS = 30000

  /**
    * Check all executions of a TLA+ specification up to a bounded number of steps.
    *
    * @return a verification outcome
    */
  def run(): Outcome.Value = {
    val outcome =
      try {
        logger.info(s"Worker ${context.rank} is initializing")
        if (context.rank == 1) {
          // the leading worker is initializing the shared state
          if (checkerInput.constInitPrimed.isDefined) {
            // initialize CONSTANTS
            context.trex.initializeConstants(checkerInput.constInitPrimed.get)
          }
          // push state 0 -- the state after initializing the parameters
          context.activeNode.synchronized {
            context.activeNode = sharedState.searchRoot // start with the root
            context.activeNode.isChecked = true // nothing to check, as Init has not been applied yet
            context.saveSnapshotToActiveNode()
            // the root node contains the list of transitions to be explored
            val statuses = checkerInput.initTransitions.zipWithIndex map { case (e, i) => (i, (e, NewTransition())) }
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

        case _: InterruptedException =>
          Outcome.Interrupted
      }

    // flush the logs
    context.dispose()
    outcome
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
      borrowTransition() || checkOneTransition || splitNodeOnLowLoad() ||
        proveOneCondition() || increaseDepth() || closeDisabledNode() || switchNode() || finishBugFree()

      val jsonFile: File = new File("search-tree.json")
      if (appliedAction) {
        sharedState.searchRoot.printJsonToFile(jsonFile)
      } else {
        // no action applicable, the worker waits and then continues
        Thread.sleep(SLEEP_DURATION_MS)
        if (context.workerState.timeSinceStartMs() >= DEADLOCK_TIMEOUT_MS) {
          sharedState.synchronized {
            val allDeadlocked: Boolean = sharedState.workerStates.values.forall {
              s => s == IdleState() && s.timeSinceStartMs() >= DEADLOCK_TIMEOUT_MS
            }
            if (allDeadlocked) {
              logger.error("Worker %d has been idle for %d ms and other workers are idle too. Deadlock? Check search-tree.json"
                .format(context.rank, context.workerState.timeSinceStartMs()))
              sharedState.searchRoot.printJsonToFile(jsonFile)
              throw new IllegalStateException("Detected deadlock")
            }
          }
        }
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
    */
  private def borrowTransition(): Boolean = {
    if (context.workerState != IdleState()) {
      return false
    }
    context.activeNode.synchronized {
      context.activeNode.openTransitions.find {
        _._2._2 == NewTransition()
      } match {
        case Some((trNo, (trEx, _))) =>
          logger.debug(s"Worker ${context.rank} borrowed transition $trNo from node ${context.activeNode.id}")
          val timeoutMs = context.activeNode.jailTimeoutSec * 1000
          val borrowed = BorrowedTransition(trNo, trEx, timeoutMs)
          context.activeNode.openTransitions += (trNo -> (trEx, borrowed))
          context.workerState = ExploringState(borrowed)
          sharedState.workerStates += context.rank -> context.workerState
          true

        case None => false
      }
    }

    // Check immediately. It might fail due to concurrency.
    checkOneTransition()
  }

  private def jailBorrowedTransitionIfSlow(borrowed: BorrowedTransition,
                                           newStatus: TransitionStatus): HyperNode = {
    val durationMs = System.currentTimeMillis() - borrowed.startTimeMs
    if (context.activeNode.parent.isEmpty || borrowed.timeoutMs > durationMs) {
      // either activeNode is the root, or the transition was fast
      context.activeNode
    } else {
      borrowed.durationMs = durationMs
      context.activeNode.synchronized { // to prevent checkOneTransition updating concurrently
        // remove the borrowed transition from the active node
        context.activeNode.openTransitions -= borrowed.trNo
        context.activeNode.slowTransitions -= borrowed.trNo
        // and add the transition to the new node
        val transition = HyperTransition(borrowed.trNo)
        transition.isJailed = true
        val newNode = HyperNode(transition)
        val elapsedMs = System.currentTimeMillis() - borrowed.startTimeMs
        logger.info("Worker %d: INTRODUCED JAIL NODE %d after timeout of %d ms on transition %d".
          format(context.rank, newNode.id, elapsedMs, borrowed.trNo))
        // introduce the borrowed transition in the new node
        newNode.closedTransitions = Map(borrowed.trNo -> (borrowed.trEx, newStatus))
        newNode.snapshot = context.activeNode.snapshot // copy the context
        val parent = context.activeNode.parent.get // it should work due to the condition
        parent.synchronized {
          parent.append(newNode)
        }
        logger.debug(s"Worker ${context.rank}: Isolated transition ${borrowed.trNo} in node ${newNode.id}")
        newNode
      }
    }
  }

  private def checkOneTransition(): Boolean = {
    context.workerState match {
      case ExploringState(borrowed) =>
        // check the borrowed transition
        context.recoverFromNodeAndActivate(context.activeNode)

        val trex = context.trex

        val startTimeMs = System.currentTimeMillis()

        // translate the transition with the transition executor
        val translatedOk = trex.prepareTransition(borrowed.trNo, borrowed.trEx)
        val isEnabled =
          if (!translatedOk) {
            logger.debug(s"Step %d: Transition #%d is syntactically disabled".format(trex.stepNo, borrowed.trNo))
            false
          } else {
            if (params.pruneDisabled) {
              // check, whether the transition is enabled in SMT
              // assume that the transition is fired and check, whether the constraints are satisfiable
              trex.assumeTransition(borrowed.trNo)
              trex.sat(params.smtTimeoutSec) match {
                case Some(true) =>
                  true // keep the transition and collect the invariants

                case None => // UNKNOWN or timeout
                  logger.debug(s"Step %d: Transition #%d timed out. Keeping enabled.".format(trex.stepNo, borrowed.trNo))
                  true

                case Some(false) =>
                  logger.debug(s"Step %d: Transition #%d is disabled".format(trex.stepNo, borrowed.trNo))
                  false // recover the transition before the transition was prepared
              }
            } else {
              logger.debug(s"Step %d: Transition #%d added unconditionally (pruneDisabled = false)"
                .format(trex.stepNo, borrowed.trNo))
              true // consider it enabled without checking
            }
          }

        // compute the new transition status
        val durationMs = System.currentTimeMillis() - startTimeMs
        val newStatus =
          if (!isEnabled) {
            DisabledTransition(durationMs)
          } else {
            // find the invariants that may changed after the enabled transition has been fired
            val invs = checkerInput.invariantsAndNegations.map(_._2)
            val vcsWithIndex = invs.zipWithIndex.filter(p => trex.mayChangeAssertion(borrowed.trNo, p._1))
            val timeMs = 0
            EnabledTransition(timeMs, vcsWithIndex.map(p => (InvariantVC(p._1), p._2)))
          }

        // if it took us too long to check the transition, move it to a separate node
        val newNode =
          newStatus match {
            case EnabledTransition(_, _) => jailBorrowedTransitionIfSlow(borrowed, newStatus)
            case _ => context.activeNode // DisabledTransition
          }

        // close the transition
        newNode.synchronized {
          newNode.openTransitions -= borrowed.trNo
          newNode.closedTransitions += borrowed.trNo -> (borrowed.trEx, newStatus)
        }
        sharedState.synchronized {
          context.workerState = IdleState()
          sharedState.workerStates += context.rank -> context.workerState
        }
        logger.debug("Worker %d: Transition %d is %s (elapsed time %d ms).".
          format(context.rank, borrowed.trNo, newStatus, newStatus.elapsedMs))

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
    context.recoverFromNodeAndActivate(context.activeNode)

    // check the invariant
    context.trex.assertState(vc.notInv)

    val newStatus =
      context.trex.sat(params.smtTimeoutSec) match {
        case Some(true) | None =>
          // treat timeout as a violation
          // TODO: add a more precise treatment
          val filenames = context.dumpCounterexample(checkerInput.rootModule, vc.notInv)
          logger.error("Invariant %s violated. Check the counterexample in: %s"
            .format(vcNo, filenames.mkString(", ")))
          InvalidVC(vc)

        case Some(false) =>
          ValidVC(vc) // the invariant holds true
      }

    val diffMs = System.currentTimeMillis() - startTimeMs
    val newState = newStatus match {
      case InvalidVC(_) =>
        logger.info(s"Worker ${context.rank}: VC $vcNo is violated in $diffMs ms. Going to the buggy state.")
        BuggyState()

      case _ =>
        logger.debug(s"Worker ${context.rank}: Proven VC $vcNo at node ${context.activeNode.id} in $diffMs ms ")
        IdleState()
    }

    val activeNode = context.activeNode
    activeNode.synchronized {
      activeNode.provenVCs += (vcNo -> newStatus)
      activeNode.unprovenVCs -= vcNo
      activeNode.isChecked = activeNode.unprovenVCs.isEmpty
      context.workerState = newState
    }
    sharedState.synchronized {
      sharedState.workerStates += context.rank -> context.workerState
    }
    true
  }

  /**
    * If the current node is completely explored and checked,
    * then switch to an unexplored or unchecked node with the minimal depth.
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
      val yetToClose = (!node.isExplored && node.openTransitions.isEmpty) &&
        (!node.isChecked && node.unprovenVCs.isEmpty)
      yetToExplore || yetToProve || yetToClose
    }

    if (isNodeUnexploredOrNotChecked(context.activeNode)) {
      // stay with the current node
      return false
    }

    // pick a tree node that is yet to be explored (in the breadth-first order)
    def findMinUnfinishedDepth(node: HyperNode): Option[Int] = {
      if (isNodeUnexploredOrNotChecked(node)) {
        Some(node.depth)
      } else {
        def min(that: Option[Int], other: Option[Int]): Option[Int] = {
          (that, other) match {
            case (Some(a), Some(b)) => Some(Math.min(a, b))
            case (Some(a), _) => Some(a)
            case (_, Some(b)) => Some(b)
            case (None, None) => None
          }
        }

        val depths = node.children.map(findMinUnfinishedDepth)
        depths.foldLeft(None: Option[Int])(min)
      }
    }

    def findNodeByDepth(depth: Int, node: HyperNode): Boolean = {
      if (isNodeUnexploredOrNotChecked(node) && node.depth == depth) {
        node.synchronized {
          // lock the node as it may be concurrently updated
          if (isNodeUnexploredOrNotChecked(node)) { // still unexplored or not checked
            context.activeNode = node
          }
          true
        }
      } else {
        node.children.exists(n => findNodeByDepth(depth, n))
      }
    }

    // change the state, as it might take some time
    val oldState = context.workerState
    context.workerState = HouseKeepingState()
    sharedState.synchronized {
      sharedState.workerStates += context.rank -> context.workerState
    }

    val result =
      findMinUnfinishedDepth(sharedState.searchRoot) match {
        case Some(depth) =>
          if (findNodeByDepth(depth, sharedState.searchRoot)) {
            logger.debug(s"Worker ${context.rank} switched to node ${context.activeNode.id}")
            true
          } else {
            false
          }

        case None => false
      }

    context.workerState = if (result) IdleState() else oldState
    sharedState.synchronized {
      sharedState.workerStates += context.rank -> context.workerState
    }
    result
  }

  private def splitNodeOnLowLoad(): Boolean = {
    if (context.activeNode.isExplored || context.activeNode.parent.isEmpty) {
      return false // the root node, or the active node has been explored; nothing to check
    }

    context.workerState match {
      case is: IdleState if is.timeSinceStartMs() >= params.idleTimeoutMs =>
        // the current worker has been idle for some time => split the active node into two
        context.activeNode.synchronized {
          val activeNode = context.activeNode
          if (activeNode.closedTransitions.isEmpty || activeNode.openTransitions.isEmpty) {
            false
          } else {
            val newNode = HyperNode(activeNode.transition)
            // move the closed transitions to the new node, so it can make progress
            newNode.closedTransitions = activeNode.closedTransitions
            newNode.snapshot = activeNode.snapshot
            newNode.isChecked = true // the invariants will be checked at activeNode
            activeNode.closedTransitions = Map()

            val parent = activeNode.parent.get
            parent.synchronized {
              parent.append(newNode)
            }

            context.activeNode = newNode

            logger.info("Idle worker %d branched node %d from node %d"
              .format(context.rank, activeNode.id, newNode.id))
            true
          }
        }

      case is: IdleState =>
        //        logger.debug("Idle worker since %d ms".format(is.timeSinceStartMs()))
        false

      case _ =>
        false
    }
  }

  private def closeDisabledNode(): Boolean = {
    if (context.workerState != IdleState() || context.activeNode.isExplored) {
      return false // the worker is busy or the node has been explored, nothing to check
    }

    context.activeNode.synchronized {
      if (context.activeNode.openTransitions.isEmpty) {
        val noOpen = context.activeNode.openTransitions.isEmpty
        val allDisabled = context.activeNode.closedTransitions.
          forall(_._2._2.isInstanceOf[DisabledTransition])
        if (noOpen && allDisabled) {
          logger.info("Worker %d: CLOSING NODE %d".format(context.rank, context.activeNode.id))
          context.activeNode.isExplored = true
          context.activeNode.isChecked = true // there is nothing to check
          true
        } else {
          false
        }
      } else {
        false
      }
    }
  }

  private def increaseDepth(): Boolean = {
    val reachedCeiling = context.stepNo >= params.stepsBound // the previous step is the last one
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

      context.workerState = HouseKeepingState()
      sharedState.synchronized {
        sharedState.workerStates += context.rank -> context.workerState
      }

      // close the tree node, nothing to explore in this node
      logger.info("Worker %d: CLOSING NODE %d".format(context.rank, context.activeNode.id))
      context.recoverFromNodeAndActivate(context.activeNode)

      // introduce a new node, unless the depth is too high
      val newNode = HyperNode(HyperTransition(enabled.map(_._2): _*))
      // recompute the jail timeout
      val newJailTimeout = 1 + context.activeNode.maxTransitionTimeSec() * params.jailTimeoutFactor / 100
      newNode.jailTimeoutSec = Math.max(newJailTimeout, params.jailTimeoutMinSec)
      // Find all verification conditions. By converting it to Map, we remove the duplicates
      val allVCsMap = context.activeNode.closedTransitions.
        collect({ case (_, (_, EnabledTransition(_, vcs))) => vcs }).
        flatten.map(p => (p._2, p._1)).
        toMap

      // schedule the verification conditions
      newNode.unprovenVCs = allVCsMap map { case (no, vc) => (no, NewVC(vc)) }
      newNode.isChecked = newNode.unprovenVCs.isEmpty

      // schedule the transitions to check
      if (!reachedCeiling) {
        val statuses = checkerInput.nextTransitions.zipWithIndex map { case (e, i) => (i, (e, NewTransition())) }
        newNode.openTransitions = HashMap(statuses: _*)
      } else {
        logger.info(s"Worker ${context.rank}: node ${newNode.id} is at max depth, only invariants will be checked")
        newNode.isExplored = true
      }
      // apply all enabled transitions
      newNode.synchronized {
        context.activeNode.append(newNode)
        // TODO: can we do that outside of the critical section?
        // apply the enabled transitions and switch to the next state
        enabled.foreach(t => context.trex.prepareTransition(t._2, t._1))
        context.trex.pickTransition()
        context.trex.nextState()
        // mark the node as explored in the end, so the finishing actions do not prematurely terminate
        context.activeNode.isExplored = true
        if (context.activeNode.unprovenVCs.isEmpty) {
          newNode.isChecked = true // nothing to check
        }
        // switch to the new node
        context.activeNode = newNode
        assert(context.activeNode.snapshot.isEmpty)
        context.saveSnapshotToActiveNode()
      }
      logger.info("Worker %d: INTRODUCED NODE %d with %d open transitions".
        format(context.rank, context.activeNode.id, context.activeNode.openTransitions.size))
    }

    context.workerState = IdleState()
    sharedState.synchronized {
      sharedState.workerStates += context.rank -> context.workerState
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
}
