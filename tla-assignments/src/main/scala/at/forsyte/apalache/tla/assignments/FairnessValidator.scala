package at.forsyte.apalache.tla.assignments

import at.forsyte.apalache.tla.lir.{OperEx, TlaEx}
import at.forsyte.apalache.tla.lir.actions.TlaActionOper
import at.forsyte.apalache.tla.lir.oper.{TlaBoolOper, TlaOper}
import at.forsyte.apalache.tla.lir.temporal.TlaTempOper
import com.google.inject.Singleton

import scala.collection.mutable

@Singleton
class FairnessValidator {

  /**
    * Checks if temporal property of the system, satisfies weak fairness condition.
    *
    * @param expression temporal expression
    * @return
    */
  def validateWF(expression: TlaEx): Boolean = {
    val enabledActions: mutable.Set[TlaEx] = new mutable.HashSet[TlaEx]()
    val nextActions: mutable.Set[TlaEx] = new mutable.HashSet[TlaEx]()


    //TODO: think about it in the mean time, the must be better solution
    def findEnabledAndNextActions(expression: TlaEx): Unit = expression match {
      case OperEx(TlaBoolOper.implies, OperEx(TlaBoolOper.or, args @ _*), _) =>
        args.foreach(it => collectEnabledActions(it))
      case OperEx(TlaBoolOper.implies, OperEx(TlaTempOper.diamond, arg), _) =>
        collectEnabledActions(arg)
      case OperEx(TlaTempOper.box, arg) =>
        findEnabledAndNextActions(arg)
      case OperEx(TlaBoolOper.and, args @ _*) =>
        args.foreach(it => findEnabledAndNextActions(it))
      case OperEx(TlaOper.eq, _*) => Unit
      case OperEx(TlaActionOper.stutter, OperEx(TlaBoolOper.or, args @ _*), _) =>
        args.foreach(it => nextActions += it)
      case ex@OperEx(_, _*) =>
        throw new RuntimeException("Unexpected pattern: " + ex)
    }

    //TODO: think about possible bugs due to <>[] in other places except WF condition
    def collectEnabledActions(actionEnabledLiteral: TlaEx): Unit = actionEnabledLiteral match {
      case OperEx(TlaTempOper.diamond, OperEx(TlaTempOper.box, enabled)) =>
        enabledActions += enabled
      case OperEx(TlaTempOper.box, enabled) =>
        collectEnabledActions(enabled)
      case OperEx(TlaActionOper.stutter, left, _) =>
        enabledActions += left
      case ex@OperEx(_, _*) =>
        throw new RuntimeException("Unexpected pattern: " + ex)
    }

    findEnabledAndNextActions(expression)
    nextActions.exists(it => enabledActions.contains(it))
  }
}
