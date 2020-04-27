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
    * The name of the status file
    */
  private val JSON_FILE = new File("search-tree.json")

  /**
    * An action by a node in the parallel exploration. An action has a pre-condition (called isEnabled),
    * which consists of fast checks on the shared state and the worker's state. If the pre-condition holds true,
    * the action may be executed with apply. However, the action has to check once again inside the criticial section,
    * whether it is still enabled (by calling isEnabled). Only if the action is still enabled, it should be applied.
    * Hence, tryApply returns true if and only if the action has been successfully applied.
    */
  private abstract class Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    def isEnabled: Boolean

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    def tryExecute(): Boolean
  }

  // initialize a new node with the transitions
  private def openNode(node: HyperNode, transitions: Seq[TlaEx]): Unit = {
    if (params.pruneDisabled) {
      // schedule transitions to be explored
      val newTransitions = transitions.zipWithIndex map { case (e, i) => (i, (e, NewTransition())) }
      node.openTransitions = HashMap(newTransitions: _*)
      node.closedTransitions = HashMap.empty
    } else {
      // all transitions are unconditionally enabled, so they can change all invariants
      val vcs = checkerInput.invariantsAndNegations.map(p => InvariantVC(p._2)).zipWithIndex
      val enabledTransitions = transitions.zipWithIndex map { case (e, i) => (i, (e, EnabledTransition(1, vcs))) }
      node.openTransitions = HashMap.empty
      node.closedTransitions = HashMap(enabledTransitions: _*)
    }
  }

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
            openNode(context.activeNode, checkerInput.initTransitions)
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
    val actions = Seq(new BorrowTransition(),
      new CheckOneTransition(), new SplitNodeOnLowLoad(), new ProveOneCondition(),
      new IncreaseDepth(), new CloseDisabledNode(), new FinishBugFree(), new SwitchNode())

    def allFinishedOrBuggy(): Boolean = {
      sharedState.synchronized {
        sharedState.workerStates.values.exists(_ == BuggyState()) ||
          sharedState.workerStates.values.forall(_.isFinished)
      }
    }

    while (!allFinishedOrBuggy()) {
      // try to apply one of the actions, similar to what we see in the TLA+ spec
      val hasFired = actions.exists(a => a.isEnabled && a.tryExecute())

      if (hasFired) {
        sharedState.searchRoot.printJsonToFile(JSON_FILE)
      } else {
        // no action applicable, the worker waits and then continues
        Thread.sleep(SLEEP_DURATION_MS)
        // check the internal state for deadlocks
        if (context.workerState.timeSinceStartMs() >= DEADLOCK_TIMEOUT_MS) {
          sharedState.synchronized {
            val allDeadlocked: Boolean = sharedState.workerStates.values.forall {
              s => s == IdleState() && s.timeSinceStartMs() >= DEADLOCK_TIMEOUT_MS
            }
            if (allDeadlocked) {
              logger.error("Worker %d has been idle for %d ms and other workers are idle too. Deadlock? Check search-tree.json"
                .format(context.rank, context.workerState.timeSinceStartMs()))
              sharedState.searchRoot.printJsonToFile(JSON_FILE)
              throw new IllegalStateException("Detected deadlock in the model checker (not your spec!)")
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

  private class BorrowTransition extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      context.workerState == IdleState() &&
        context.activeNode.openTransitions.exists(_._2._2 == NewTransition())
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
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
      new CheckOneTransition().tryExecute()
    }
  }

  private class CheckOneTransition extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      context.workerState match {
        case ExploringState(_) => true
        case _ => false
      }
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
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
  }

  private class ProveOneCondition extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      context.workerState == IdleState() &&
        context.activeNode.unprovenVCs.exists {
          case (_, NewVC(_)) => true
          case _ => false
        }
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
      // find a condition, if there is any
      val (vcNo, vc) =
        context.activeNode.synchronized {
          if (!isEnabled) {
            return false
          }

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
      }
      sharedState.synchronized {
        sharedState.workerStates += context.rank -> newState
        context.workerState = newState
      }
      true
    }
  }

  /**
    * If the current node is completely explored and checked, then switch to an unexplored or unchecked node
    * with the minimal depth. This action is not locking the nodes, so it may sometimes switch to a node that has
    * been explored. This should be no problem, as it will switch to the right node later.
    */
  private class SwitchNode extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      context.workerState == IdleState() &&
        !isNodeUnexploredOrNotChecked(context.activeNode)
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
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

    private def isNodeUnexploredOrNotChecked(node: HyperNode): Boolean = {
      val yetToExplore = !node.isExplored && node.openTransitions.values.exists(_._2 == NewTransition())
      val yetToProve = !node.isChecked && node.unprovenVCs.values.exists(_.isInstanceOf[NewVC])
      val yetToClose = (!node.isExplored && node.openTransitions.isEmpty) &&
        (!node.isChecked && node.unprovenVCs.isEmpty)
      yetToExplore || yetToProve || yetToClose
    }

    // pick a tree node that is yet to be explored (in the breadth-first order)
    private def findMinUnfinishedDepth(node: HyperNode): Option[Int] = {
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

    private def findNodeByDepth(depth: Int, node: HyperNode): Boolean = {
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
  }

  private class SplitNodeOnLowLoad extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      !context.activeNode.isExplored &&
        context.activeNode.parent.isDefined &&
        context.activeNode.closedTransitions.nonEmpty &&
        context.activeNode.openTransitions.nonEmpty &&
        (context.workerState match {
          case is: IdleState if is.timeSinceStartMs() >= params.idleTimeoutMs => true
          case _ => false
        })
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
      context.activeNode.synchronized {
        if (!isEnabled) {
          return false
        }

        // the current worker has been idle for some time => split the active node into two
        val activeNode = context.activeNode
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
  }

  private class CloseDisabledNode extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      val noOpen = context.activeNode.openTransitions.isEmpty
      val allDisabled = context.activeNode.closedTransitions.
        forall(_._2._2.isInstanceOf[DisabledTransition])

      context.workerState == IdleState() &&
        !context.activeNode.isExplored &&
        noOpen &&
        allDisabled
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
      context.activeNode.synchronized {
        if (!isEnabled) {
          false
        } else {
          logger.info("Worker %d: EXPLORED NODE %d".format(context.rank, context.activeNode.id))
          context.activeNode.isExplored = true
          context.activeNode.isChecked = true // there is nothing to check
          true
        }
      }
    }
  }

  private class FinishBugFree extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      def allExploredAndChecked(node: HyperNode): Boolean = {
        // we do not need a lock here, as isExplored is changing once from false to true, and it can be checked later again
        node.isExplored && node.isChecked && node.children.forall(allExploredAndChecked)
      }

      val allIdle = sharedState.workerStates.values.forall(ws => ws == IdleState() || ws.isFinished)
      allIdle && allExploredAndChecked(sharedState.searchRoot)
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
      sharedState.synchronized {
        if (isEnabled) {
          sharedState.workerStates += context.rank -> BugFreeState()
          true
        } else {
          false
        }
      }
    }
  }

  private class IncreaseDepth extends Action {
    /**
      * Is an action enabled? By the time, `tryApply` is called, `isEnabled` may expire, so `tryApply` should call
      * `isEnabled` once again inside its critical section, which is protected by a lock.
      *
      * @return true if it is
      */
    override def isEnabled: Boolean = {
      val allExplored = context.activeNode.openTransitions.isEmpty

      context.workerState == IdleState() &&
        !context.activeNode.isExplored &&
        allExplored &&
        closedButEnabledTransitions.nonEmpty
    }

    /**
      * Try to apply the action.
      *
      * @return true, if the action has been successfully applied
      */
    override def tryExecute(): Boolean = {
      context.activeNode.synchronized {
        if (!isEnabled) {
          false
        } else {
          context.workerState = HouseKeepingState()
          sharedState.synchronized {
            sharedState.workerStates += context.rank -> context.workerState
          }

          // close the tree node, nothing to explore in this node
          logger.info("Worker %d: EXPLORED NODE %d".format(context.rank, context.activeNode.id))
          // Synchronization may take tens of seconds. However, there is nothing to do about the node, but close it
          context.recoverFromNodeAndActivate(context.activeNode)
          // TODO: can we do that outside of the critical section?
          // apply the enabled transitions and switch to the next state
          val enabled = closedButEnabledTransitions
          val reachedCeiling = context.stepNo >= params.stepsBound // the previous step is the last one
          enabled.foreach(t => context.trex.prepareTransition(t._2, t._1))
          context.trex.pickTransition()
          context.trex.nextState()

          // introduce a new node
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

          if (!reachedCeiling) {
            // schedule the transitions to check
            openNode(newNode, checkerInput.nextTransitions)
          } else {
            // we need the new node to check the invariants at the state that results from taking a transition
            logger.info(s"Worker ${context.rank}: node ${newNode.id} is at max depth. Only checking invariants")
            newNode.isExplored = true
          }
          // apply all enabled transitions
          newNode.synchronized {
            if (newNode.unprovenVCs.isEmpty) {
              newNode.isChecked = true // nothing to check
            }
            context.activeNode.append(newNode)
            // mark the node as explored in the end, so the finishing actions do not prematurely terminate
            context.activeNode.isExplored = true
            // switch to the new node
            context.activeNode = newNode
            assert(context.activeNode.snapshot.isEmpty)
            context.saveSnapshotToActiveNode()
          }
          logger.info("Worker %d: INTRODUCED NODE %d, %d open and %d closed transitions, %d verification conditions".
            format(context.rank, newNode.id,
                   newNode.openTransitions.size, newNode.closedTransitions.size, newNode.unprovenVCs.size))

          context.workerState = IdleState()
          sharedState.synchronized {
            sharedState.workerStates += context.rank -> context.workerState
          }

          true
        }
      }
    }

    private def closedButEnabledTransitions = {
      context.activeNode.closedTransitions.
        collect({ case (trNo, (trEx, EnabledTransition(_, _))) => (trEx, trNo) }).toList
    }
  }

}
