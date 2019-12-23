package at.forsyte.apalache.tla.bmcmt.search

/**
  * A state of a worker thread.
  */
sealed abstract class WorkerState {
  /**
    * The time when the state was created. This is useful for deadlock detection.
    * This field is not used in equals.
    */
  val startTimeMs: Long = System.currentTimeMillis()

  def timeSinceStart(): Long = System.currentTimeMillis() - startTimeMs

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
  * The worker is checking, whether a transition is enabled.
  *
  * @param borrowed the state of the borrowed transition
  */
case class ExploringState(borrowed: BorrowedTransition) extends WorkerState

/**
  * The worker is proving a verification condition.
  */
case class ProvingState(vcNo: Int, vc: VerificationCondition) extends WorkerState

/**
  * The worker is doing some processing that is different from ExploringState and ProvingState.
  */
case class HouseKeepingState() extends WorkerState

/**
  * The worker has found a bug.
  */
case class BuggyState() extends WorkerState

/**
  * The worker has reported that the search is over, no bugs found.
  */
case class BugFreeState() extends WorkerState

