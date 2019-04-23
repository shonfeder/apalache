package at.forsyte.apalache.tla.assignments

import at.forsyte.apalache.tla.lir.{OperEx, TlaEx}
import at.forsyte.apalache.tla.lir.actions.TlaActionOper
import at.forsyte.apalache.tla.lir.db.BodyDB
import at.forsyte.apalache.tla.lir.oper.TlaBoolOper
import at.forsyte.apalache.tla.lir.temporal.TlaTempOper

class PropertyExtractor(val bodyDB: BodyDB) {

  private val ENABLED_PREFIX = "ENABLED"
  private val PREFIX_LENGTH = 8

  def extractLivenessProperty(specification: TlaEx): Option[TlaEx] = specification match {
    case OperEx(TlaBoolOper.implies, _, livenessPlaceholder) =>
      val liveness = bodyDB.get(livenessPlaceholder.toString.dropRight(2))
      if (liveness.isDefined) {
        liveness.get._2 match {
          case OperEx(TlaTempOper.diamond, OperEx(TlaTempOper.box, OperEx(TlaActionOper.stutter, formula, _))) => Some(formula)
          case _ => None
        }
      } else {
        None
      }
    case _ => None
  }

  def extractActionHintTuples(temporalSpecName: String): (List[(TlaEx, TlaEx)], List[(TlaEx, TlaEx)]) = {

    val temporalSpec = bodyDB.get(temporalSpecName).get._2

    def string2formula(name: String): Option[TlaEx] = {
      val optionalTuple = bodyDB.get(name)
      if (optionalTuple.isEmpty) None else Some(optionalTuple.get._2)
    }

    def findWeakFairnessNames(specification: TlaEx): List[String] = specification match {
      case OperEx(TlaBoolOper.implies, OperEx(TlaTempOper.diamond, OperEx(TlaTempOper.box, arg)), _) =>
        findWeakFairnessNames(arg)
      case OperEx(TlaBoolOper.implies, OperEx(TlaTempOper.box, OperEx(TlaTempOper.diamond, arg)), _) =>
        List()
      case OperEx(TlaBoolOper.implies, specificationPlaceholder, _) =>
        val optionalFormula = string2formula(specificationPlaceholder.toString.dropRight(2))
        if (optionalFormula.isEmpty) {
          List()
        } else {
          findWeakFairnessNames(optionalFormula.get)
        }
      case OperEx(TlaBoolOper.and, args@_*) =>
        args.map(findWeakFairnessNames).reduce((accum, list) => accum ::: list)
      case OperEx(TlaActionOper.stutter, formula, _) =>
        List(formula.toString.dropRight(2))
      case _ =>
        List()
    }

    def findStrongFairnessNames(specification: TlaEx): List[String] = specification match {
      case OperEx(TlaBoolOper.implies, OperEx(TlaTempOper.box, OperEx(TlaTempOper.diamond, arg)), _) =>
        findStrongFairnessNames(arg)
      case OperEx(TlaBoolOper.implies, OperEx(TlaTempOper.diamond, OperEx(TlaTempOper.box, arg)), _) =>
        List()
      case OperEx(TlaBoolOper.implies, specificationPlaceholder, _) =>
        val optionalFormula = string2formula(specificationPlaceholder.toString.dropRight(2))
        if (optionalFormula.isEmpty) {
          List()
        } else {
          findStrongFairnessNames(optionalFormula.get)
        }
      case OperEx(TlaBoolOper.and, args@_*) =>
        args.map(findStrongFairnessNames).reduce((accum, list) => accum ::: list)
      case OperEx(TlaActionOper.nostutter, formula, _) =>
        List(formula.toString.dropRight(2))
      case _ =>
        List()
    }

    val weakFairnessNames = findWeakFairnessNames(temporalSpec)
    val strongFairnessNames = findStrongFairnessNames(temporalSpec)

    (
      bodyDB.fullScan
            .filter { _._1.startsWith(ENABLED_PREFIX) }
            .filter { it => weakFairnessNames.contains(it._1.substring(PREFIX_LENGTH)) }
            .map { it => (bodyDB.get(it._1.substring(PREFIX_LENGTH)).get._2, it._2._2) },
      bodyDB.fullScan
            .filter { it => it._1.startsWith(ENABLED_PREFIX) }
            .filter { it => strongFairnessNames.contains(it._1.substring(PREFIX_LENGTH)) }
            .map { it => (bodyDB.get(it._1.substring(PREFIX_LENGTH)).get._2, it._2._2) }
    )
  }
}
