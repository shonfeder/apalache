package at.forsyte.apalache.tla.bmcmt.search

/**
  * A status of evaluating a symbolic transition when extending a hyperpath.
  */
sealed abstract class TransitionStatus {
  def isExplored: Boolean = this match {
    case EnabledTransition() | DisabledTransition() | TimedOutTransition() => true
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
  */
case class EnabledTransition() extends TransitionStatus

/**
  * The transition was found to be disabled.
  */
case class DisabledTransition() extends TransitionStatus

/**
  * The solver timed out when checking the transition.
  */
case class TimedOutTransition() extends TransitionStatus
