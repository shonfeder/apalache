package at.forsyte.apalache.tla.pp

import at.forsyte.apalache.tla.lir.{TlaDecl, TlaOperDecl}

/**
  * Normalized names for various operator definitions that are produced by pre-processing.
  */
object NormalizedNames {
  /**
    * The initialization operators that stem from Init
    */
  val INIT_PREFIX = "Init$"
  /**
    * The transition operators that stem from Next
    */
  val NEXT_PREFIX = "Next$"
  /**
    * The parameter initialization constraints that stem from ConstInit
    */
  val CONST_INIT = "CInit$0"
  /**
    * The initialization operators that stem from TypeOK
    */
  val TYPE_INIT_PREFIX = "TypeInit$"
  /**
    * The invariant operators that stem from the invariant under verification
    */
  val VC_INV_PREFIX = "VCInv$"
  /**
    * The negated invariant operators
    */
  val VC_NOT_INV_PREFIX = "VCNotInv$"
  /**
    * The prefix of user-defined predicates that are needed for Boolean abstraction
    */
  val PRED_PREFIX = "ABS_PRED_"

  /**
    * Has been an operator declaration produced by the VCGenerator
    * @param decl an operator declaration
    * @return true, if the operator name matches the VC pattern
    */
  def isVC(decl: TlaDecl): Boolean = {
    decl.isInstanceOf[TlaOperDecl] &&
      decl.asInstanceOf[TlaOperDecl].formalParams.isEmpty &&
      (decl.name.startsWith(VC_INV_PREFIX) || decl.name.startsWith(VC_NOT_INV_PREFIX))
  }

  /**
    * Is it a predicate for predicate abstraction
    * @param decl an operator declaration
    * @return true, if the operator name matches the VC pattern
    */
  def isPred(decl: TlaDecl): Boolean = {
    decl.isInstanceOf[TlaOperDecl] &&
      decl.asInstanceOf[TlaOperDecl].formalParams.isEmpty &&
      decl.name.startsWith(PRED_PREFIX)
  }

  /**
    * Does an operator define the constant initializer (there is only one).
    *
    * @param decl an operator declaration
    * @return true, if the operator is a constant initializer
    */
  def isConstInit(decl: TlaDecl): Boolean = {
    decl.isInstanceOf[TlaOperDecl] &&
      decl.asInstanceOf[TlaOperDecl].formalParams.isEmpty &&
      decl.name == CONST_INIT
  }

  /**
    * Does an operator define an init predicate (there may be several).
    *
    * @param decl an operator declaration
    * @return true, if the operator is a state initializer
    */
  def isInit(decl: TlaDecl): Boolean = {
    decl.isInstanceOf[TlaOperDecl] &&
      decl.asInstanceOf[TlaOperDecl].formalParams.isEmpty &&
      decl.name.startsWith(INIT_PREFIX)
  }

  /**
    * Does an operator define a transition predicate (there may be several).
    *
    * @param decl an operator declaration
    * @return true, if the operator is a transition predicate
    */
  def isNext(decl: TlaOperDecl): Boolean = {
    decl.isInstanceOf[TlaOperDecl] &&
      decl.asInstanceOf[TlaOperDecl].formalParams.isEmpty &&
      decl.name.startsWith(NEXT_PREFIX)
  }
}
