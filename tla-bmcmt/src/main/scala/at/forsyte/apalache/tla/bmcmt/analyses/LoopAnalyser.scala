package at.forsyte.apalache.tla.bmcmt.analyses

import at.forsyte.apalache.tla.bmcmt.{ArenaCell, Binding, CellTheory, SolverContext, SymbState, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.{NameEx, OperEx, TlaEx, ValEx}
import at.forsyte.apalache.tla.lir.actions.TlaActionOper
import at.forsyte.apalache.tla.lir.oper.{TlaArithOper, TlaBoolOper, TlaOper, TlaSetOper}
import at.forsyte.apalache.tla.lir.convenience.tla

import scala.collection.mutable.ListBuffer

class LoopAnalyser(val nextTransitions: List[TlaEx],
                   val loopInvariant: TlaEx,
                   var stateStack: List[(SymbState, ArenaCell)],
                   val rewriter: SymbStateRewriterImpl,
                   val solverContext: SolverContext) {

  //TODO (Viktor): write unit-tests
  //TODO (Viktor): check with multiple variables
  def findAllLoops: List[(Int, Int)] = {
    val next = nextTransitions.map { it => convertToEquality(it)}

    val transitionLoopStartIndexTuples = ListBuffer[(Int, Int)]()
    for (i <- next.indices) {
      val last = setActionForLastState(next(i))

      // we ignore first and last states
      //first - add variable 'x'' and check for the presence of the loop
      //last - add variable 'x'' and check for the presence of the loop
      //TODO (Viktor): finish it
      for (j <- 1 until stateStack.size) {
        if (checkForLoopWithAction(j, last)) {
          val tuple = (i, j)
          transitionLoopStartIndexTuples += tuple
        }
      }
    }

    transitionLoopStartIndexTuples.toList
  }

  private def convertToEquality(ex: TlaEx): TlaEx = ex match {
    case OperEx(TlaSetOper.in, arg1, OperEx(TlaSetOper.enumSet, arg)) =>
      OperEx (TlaOper.eq, arg1, arg)
    case _ => throw new RuntimeException("In NEXT statement only assignments are supposed to be.")
  }

  private def checkForLoopWithAction(stateNumber: Int, lastState: SymbState): Boolean = {
    var selected = makeAllPrimed(stateStack(stateNumber))
    stateStack = (stateStack.slice(0, stateNumber) :+ selected) ++ stateStack.slice(stateNumber + 1, stateStack.size)

    //TODO (Viktor): basically, the number of invocations of push/pop methods should depend on the number of variables
    // ask Igor about it
    rewriter.push()
    rewriter.push()

    val ex = rewriter.rewriteUntilDone(lastState.setTheory(CellTheory())).ex
    solverContext.assertGroundExpr(ex)

    val result = solverContext.sat()

    rewriter.pop(2)
    selected = makeAllUnprimed(stateStack(stateNumber))
    stateStack = (stateStack.slice(0, stateNumber) :+ selected) ++ stateStack.slice(stateNumber + 1, stateStack.size)

    result
  }

  private def setActionForLastState(action: TlaEx): SymbState = {
    var last = stateStack.head
    val state = last._1.setRex(action)
    last = (state, last._2)
    stateStack = last :: stateStack.tail

    state
  }

  private def makeAllPrimed(selected: (SymbState, ArenaCell)): (SymbState, ArenaCell) = {
    val state = selected._1
    val binding = state.binding
    val withNewBinding = state.setBinding(Binding(binding.map{ t => (t._1 + "'", t._2) }))
    withNewBinding.binding.foreach { it => rewriter.exprCache.put(OperEx(TlaActionOper.prime, NameEx(it._1.dropRight(1))), (it._2.toNameEx, ExprGrade.Constant)) }

    (withNewBinding, selected._2)
  }

  private def makeAllUnprimed(selected: (SymbState, ArenaCell)): (SymbState, ArenaCell) = {
    val state = selected._1
    val binding = state.binding
    val withNewBinding = state.setBinding(Binding(binding.map{ t => (t._1.dropRight(1), t._2) }))
    //TODO (Viktor): clear cache

    (withNewBinding, selected._2)
  }

  def validateLoopInvariant(transitionLoopStartIndexTuples: List[(Int, Int)]): Boolean = {
    val notLoopInvariant = tla.not(loopInvariant)
    for (i <- transitionLoopStartIndexTuples.indices) {
      //TODO (Viktor): ignored last state, add it into consideration
      var j = stateStack.size - 1
      while (j >= transitionLoopStartIndexTuples(i)._2) {
        rewriter.push()
        val ex = replaceVariable(notLoopInvariant, stateStack(j - 1)._1.binding)
        solverContext.assertGroundExpr(ex)
        j -= 1
      }
      if (solverContext.sat()) {
        return false
      }
      solverContext.pop(stateStack.size - j)
    }

    return true
  }

  //TODO (Viktor): cover all cases
  def replaceVariable(expression: TlaEx, binding: Binding): TlaEx = expression match {
    case NameEx(name) =>
      binding(name).toNameEx
    case ValEx(value) => ValEx(value)
    case OperEx(TlaBoolOper.not, arg) =>
      OperEx(TlaBoolOper.not, replaceVariable(arg, binding))
    case OperEx(TlaArithOper.ge, arg1, arg2) =>
      OperEx(TlaArithOper.ge, replaceVariable(arg1, binding), replaceVariable(arg2, binding))
    case OperEx(TlaArithOper.plus, arg1, arg2) =>
      OperEx(TlaArithOper.plus, replaceVariable(arg1, binding), replaceVariable(arg2, binding))
    case ex@OperEx(_, _*) =>
      throw new RuntimeException("Unexpected pattern: " + ex)
  }
}