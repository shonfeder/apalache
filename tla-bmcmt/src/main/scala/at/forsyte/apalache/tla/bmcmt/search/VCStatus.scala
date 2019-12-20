package at.forsyte.apalache.tla.bmcmt.search

import at.forsyte.apalache.tla.lir.TlaEx

/**
  * Verification condition.
  */
sealed abstract class VerificationCondition

/**
  * An invariant to be proved.
  *
  * @param notInv negated invariant
  */
case class InvariantVC(notInv: TlaEx) extends VerificationCondition

/**
  * The status of a verification condition in a certain state, e.g., whether an invariant has been proved or disproved.
  *
  * @author Igor Konnov
  */
sealed abstract class VCStatus

/**
  * A verification condition to be proved.
  *
  * @param vc a verification condition
  */
case class NewVC(vc: VerificationCondition) extends VCStatus

/**
  * A verification condition that is being proved by a worker.
  *
  * @param vc a verification condition
  */
case class InProgressVC(vc: VerificationCondition) extends VCStatus

/**
  * A verification condition that has been proven to be valid, that is, its negation is unsatisfiable.
  * @param vc a verification condition
  */
case class ValidVC(vc: VerificationCondition) extends VCStatus

/**
  * A verification condition that has been proven to be invalid, that is, its negation is satisfiable.
  * @param vc a verification condition
  */
case class InvalidVC(vc: VerificationCondition) extends VCStatus

/**
  * A verification condition whose status is unknown, as the SMT solver has reported.
  * @param vc a verification condition
  */
case class UnknownVC(vc: VerificationCondition) extends VCStatus
