package at.forsyte.apalache.tla.bmcmt.search

import at.forsyte.apalache.tla.lir.TlaEx

/**
  * A status of evaluating a symbolic transition when extending a hyperpath.
  */
sealed abstract class TransitionStatus {
  def isClosed: Boolean = this match {
    case EnabledTransition(_, _) | DisabledTransition(_) | TimedOutTransition(_) => true
    case _ => false
  }

  def elapsedMs: Long
}

/**
  * The transition has been introduced but not checked.
  */
case class NewTransition() extends TransitionStatus {
  override def elapsedMs: Long = 0

  override def toString: String = "new"
}

/**
  * The transition has been borrowed by a worker thread for checking. Importantly, the node may be changed later,
  * if it takes too long to check, whether the transition is enabled. In that case, the transition will migrate
  * to a fresh node.
  *
  * @param trNo transition number
  * @param trEx transition expression
  * @param timeoutMs timeout in seconds, after which the node is going to migrate to a distinguished node.
  */
case class BorrowedTransition(trNo: Int, trEx: TlaEx, timeoutMs: Long) extends TransitionStatus {
  val startTimeMs: Long = System.currentTimeMillis()
  var durationMs: Long = 0

  override def elapsedMs: Long = durationMs

  override def toString: String = "borrowed"
}

/**
  * The transition was found to be enabled.
  *
  * @param feasibilityMs how long it took to check transition feasibility (in milliseconds)
  * @param vcsWithIndex the verification conditions that must be proven after the transition has been fired,
  *                     along with their indices.
  */
case class EnabledTransition(feasibilityMs: Long,
                             vcsWithIndex: List[(VerificationCondition, Int)]) extends TransitionStatus {
  override def elapsedMs: Long = feasibilityMs

  override def toString: String = "enabled"
}

/**
  * The transition was found to be disabled.
  *
  * @param feasibilityMs how long it took to check transition feasibility (in milliseconds)
  */
case class DisabledTransition(feasibilityMs: Long) extends TransitionStatus {
  override def elapsedMs: Long = feasibilityMs

  override def toString: String = "disabled"
}

/**
  * The solver timed out when checking the transition.
  *
  * @param timeoutMs the number of milliseconds it took before timeout
  */
case class TimedOutTransition(timeoutMs: Long) extends TransitionStatus {
  override def elapsedMs: Long = timeoutMs

  override def toString: String = "timeout"
}
