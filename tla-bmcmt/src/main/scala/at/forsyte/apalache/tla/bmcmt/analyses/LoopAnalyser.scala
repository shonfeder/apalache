package at.forsyte.apalache.tla.bmcmt.analyses

import at.forsyte.apalache.tla.bmcmt.{ArenaCell, Binding, CellTheory, SolverContext, SymbState, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.{OperEx, TlaEx}
import at.forsyte.apalache.tla.lir.oper.{TlaOper, TlaSetOper}
import at.forsyte.apalache.tla.lir.convenience.tla

import scala.collection.mutable.ListBuffer

//TODO (Viktor): write unit-tests
class LoopAnalyser(val nextTransitions: List[TlaEx],
                   val loopInvariant: TlaEx,
                   var stateStack: List[(SymbState, ArenaCell)],
                   val rewriter: SymbStateRewriterImpl,
                   val solverContext: SolverContext) {

  def findAllLoops: List[Int] = {
    def setActionForLastState(action: TlaEx): SymbState = {
      var last = stateStack.head
      val state = last._1.setRex(action)
      last = (state, last._2)
      stateStack = last :: stateStack.tail

      state
    }

    def convertToEquality(ex: TlaEx): TlaEx = ex match {
      case OperEx(TlaSetOper.in, arg1, OperEx(TlaSetOper.enumSet, arg)) =>
        OperEx (TlaOper.eq, arg1, arg)
      case _ => throw new RuntimeException("In NEXT statement only assignments are supposed to be.")
    }

    def checkForLoopWithAction(stateNumber: Int, lastState: SymbState): Boolean = {
      def addPrimedBinding(last: SymbState, selected: SymbState): SymbState = {
        val selectedBinding = selected.binding
        val lastBinding = last.binding
        val lastWithBinding = last.setBinding(Binding(lastBinding.merged(selectedBinding.map { t => (t._1 + "'", t._2)})((k, _) => k)))

        lastWithBinding
      }

      def checkLoopTransition(last: SymbState) = {
        rewriter.push()
        val ex = rewriter.rewriteUntilDone(last.setTheory(CellTheory())).ex
        solverContext.assertGroundExpr(ex)
        val result = solverContext.sat()
        rewriter.pop()
        result
      }

      val lastWithPrimedBinding = addPrimedBinding(lastState, stateStack(stateNumber)._1)
      checkLoopTransition(lastWithPrimedBinding)
    }

    val next = nextTransitions.map { it => convertToEquality(it)}

    val loopStartIndeces = ListBuffer[Int]()
    for (i <- next.indices) {
      val last = setActionForLastState(next(i))

      for (j <- 0 until stateStack.size - 1) {
        if (checkForLoopWithAction(j, last)) {
          val tuple = j
          loopStartIndeces += tuple
        }
      }
    }

    loopStartIndeces.toList
  }

  def validateLiveness(loopStartIndexes: List[Int]): Boolean = {
    val notLoopInvariant = tla.not(loopInvariant)

    for (i <- loopStartIndexes.indices) {
      val startIndex = loopStartIndexes(i)

      rewriter.push()

      var j = 0
      var lastState = stateStack.head._1
      while(j <= startIndex) {
        val requiredBinding = stateStack(j)._1.binding
        val state = lastState.setBinding(requiredBinding).setRex(notLoopInvariant)
        lastState = rewriter.rewriteUntilDone(state.setTheory(CellTheory()))
        solverContext.assertGroundExpr(lastState.ex)

        j += 1
      }
      val result = solverContext.sat()

      if (result) {
        return false
      }

      rewriter.pop()
    }

    true
  }
}