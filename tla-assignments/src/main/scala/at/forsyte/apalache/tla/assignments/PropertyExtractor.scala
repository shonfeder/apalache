package at.forsyte.apalache.tla.assignments

import at.forsyte.apalache.tla.lir.{OperEx, TlaEx}
import at.forsyte.apalache.tla.lir.actions.TlaActionOper
import at.forsyte.apalache.tla.lir.db.BodyDB
import at.forsyte.apalache.tla.lir.oper.TlaBoolOper
import at.forsyte.apalache.tla.lir.temporal.TlaTempOper

import scala.collection.mutable.ListBuffer

class PropertyExtractor(val bodyDB: BodyDB) {
  val ENABLED_PREFIX = "ENABLED"

  def extractLivenessProperty(specification: TlaEx): Option[TlaEx] = specification match {
    case OperEx(TlaBoolOper.implies, _, args@_*) =>
      args.map(extractLivenessProperty)
          .find(it => it.isDefined)
          .map(it => it.get)
    case OperEx(TlaBoolOper.and, args@_*) =>
      args.map(extractLivenessProperty)
          .find(it => it.isDefined)
          .map(it => it.get)
    case OperEx(TlaTempOper.diamond, OperEx(TlaTempOper.box, OperEx(TlaActionOper.stutter, arg1, _))) =>
      Some(arg1)
    case _ =>
      None

  }

  def extractActionHintTuples(specification: TlaEx): (List[(TlaEx, TlaEx)], List[(TlaEx, TlaEx)]) = {
    val actionsWithWeakFairness = ListBuffer[String]()
    val actionsWithStrongFairness = ListBuffer[String]()

    def findFairnessConditions(specification: TlaEx): Unit = specification match {
      case OperEx(TlaBoolOper.and, args@_*) => args.foreach(findFairnessConditions)
      case ex => throw new RuntimeException("Unhandled pattern: " + ex)
    }

    findFairnessConditions(specification)
    (
      bodyDB.fullScan
            .filter { it => it._1.startsWith(ENABLED_PREFIX) }
            .filter { it => actionsWithWeakFairness.contains(it._1.substring(8)) }
            .map { it => (bodyDB.get(it._1).get._2, it._2._2) },
      bodyDB.fullScan
            .filter { it => it._1.startsWith(ENABLED_PREFIX) }
            .filter { it => actionsWithStrongFairness.contains(it._1.substring(8)) }
            .map { it => (bodyDB.get(it._1).get._2, it._2._2) }
    )
  }
}
