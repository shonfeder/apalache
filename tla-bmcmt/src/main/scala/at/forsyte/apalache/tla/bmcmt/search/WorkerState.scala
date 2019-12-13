package at.forsyte.apalache.tla.bmcmt.search

import at.forsyte.apalache.tla.lir.TlaEx

/**
  * A state of a worker thread.
  */
sealed abstract class WorkerState {
  def isFinished: Boolean = this match {
    case BuggyState() | BugFreeState() => true
    case _ => false
  }
}

/**
  * The worker is looking for a job.
  */
case class IdleState() extends WorkerState

/**
  * The worker is checking feasibility of a transition.
  *
  * @param trNo transition number
  * @param trEx transition expression
  */
case class ExploringState(trNo: Int, trEx: TlaEx) extends WorkerState

/**
  * The worker is proving an invariant.
  */
case class ProvingState() extends WorkerState

/**
  * The worker has found a bug.
  */
case class BuggyState() extends WorkerState

/**
  * The worker has reported that the search is over, no bugs found.
  */
case class BugFreeState() extends WorkerState

