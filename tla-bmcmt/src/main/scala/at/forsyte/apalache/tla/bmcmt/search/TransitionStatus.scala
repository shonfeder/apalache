package at.forsyte.apalache.tla.bmcmt.search

/**
  * A status of evaluating a symbolic transition when extending a hyperpath.
  */
sealed abstract class TransitionStatus {
  def isClosed: Boolean = this match {
    case EnabledTransition(_) | DisabledTransition() | TimedOutTransition() => true
    case _ => false
  }
}

/**
  * The transition has been introduced but not checked.
  */
case class NewTransition() extends TransitionStatus

/**
  * The transition has been borrowed by a worker thread for checking.
  */
case class BorrowedTransition() extends TransitionStatus

/**
  * The transition was found to be enabled.
  *
  * @param vcsWithIndex the verification conditions that must be proven after the transition has been fired,
  *                     along with their indices.
  */
case class EnabledTransition(vcsWithIndex: List[(VerificationCondition, Int)]) extends TransitionStatus

/**
  * The transition was found to be disabled.
  */
case class DisabledTransition() extends TransitionStatus

/**
  * The solver timed out when checking the transition.
  */
case class TimedOutTransition() extends TransitionStatus

/**
  * A bug (e.g., invariant violation) was found after applying the transition.
  */
case class BuggyTransition() extends TransitionStatus
