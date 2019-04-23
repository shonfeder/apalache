package at.forsyte.apalache.tla.bmcmt.analyses

import at.forsyte.apalache.tla.bmcmt.{ArenaCell, Binding, CellTheory, CheckerInput, SolverContext, SymbState, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.{OperEx, TlaEx}
import at.forsyte.apalache.tla.lir.oper.{TlaOper, TlaSetOper}
import at.forsyte.apalache.tla.lir.convenience.tla

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//TODO (Viktor): write unit-tests
class LoopAnalyser(val checkerInput: CheckerInput,
                   var stateStack: List[(SymbState, ArenaCell)],
                   val rewriter: SymbStateRewriterImpl,
                   val solverContext: SolverContext) {

  def findAllLoops: List[(Int, TlaEx)] = {
    val loopStartIndexes = mutable.Set[(Int, TlaEx)]()

    val next = checkerInput.nextTransitions.map(convertToEquality)
    for (action <- next) {
      loopStartIndexes ++= findLoopsForAction(action)
    }

    loopStartIndexes.toList
  }

  private def findLoopsForAction(action: TlaEx): ListBuffer[(Int, TlaEx)] = {
    val loopStartIndexes = ListBuffer[(Int, TlaEx)]()

    val lastState = setActionForLastState(action)
    for (loopStartIndex <- 0 until stateStack.size - 1) {
      val loopStartState = stateStack(loopStartIndex)._1
      if (checkForLoopWithAction(loopStartState, lastState)) {
        val validTuple = (loopStartIndex, action)
        loopStartIndexes += validTuple
      }
    }

    loopStartIndexes
  }

  private def setActionForLastState(action: TlaEx): SymbState = {
    var lastTuple = stateStack.head
    val state = lastTuple._1.setRex(action)
    lastTuple = (state, lastTuple._2)
    stateStack = lastTuple :: stateStack.tail

    state
  }

  private def checkForLoopWithAction(loopStartState: SymbState, lastState: SymbState): Boolean = {
    def checkLoopTransition(last: SymbState): Boolean = {
      rewriter.push()
      val ex = rewriter.rewriteUntilDone(last.setTheory(CellTheory())).ex
      solverContext.assertGroundExpr(ex)
      val result = solverContext.sat()
      rewriter.pop()
      result
    }

    val lastWithPrimedBinding = addPrimedBinding(lastState, loopStartState)
    checkLoopTransition(lastWithPrimedBinding)
  }

  private def convertToEquality(ex: TlaEx): TlaEx = ex match {
    case OperEx(TlaSetOper.in, arg1, OperEx(TlaSetOper.enumSet, arg)) =>
      OperEx(TlaOper.eq, arg1, arg)
    case OperEx(operator, args@_*) => OperEx(operator, args.map(convertToEquality): _*)
    case it => it
  }

  def checkLiveness(loopStartIndexWithActionTuples: List[(Int, TlaEx)]): List[(Int, TlaEx)] = {
    def checkIfLivenessEventNeverHappened(loopIndexWithActionTuple: (Int, TlaEx), notLiveness: TlaEx): Boolean = {
      rewriter.push()

      var lastState = stateStack.head._1
      lastState = rewriter.rewriteUntilDone(addPrimedBinding(lastState, stateStack(loopIndexWithActionTuple._1)._1).setTheory(CellTheory()))
      solverContext.assertGroundExpr(lastState.ex)

      for (j <- 0 to loopIndexWithActionTuple._1) {
        lastState = setBindingAndActionForLastState(lastState, stateStack(j)._1, notLiveness)
        solverContext.assertGroundExpr(lastState.ex)
      }
      val result = solverContext.sat()

      rewriter.pop()

      result
    }

    val notLiveness = tla.not(checkerInput.liveness.get)

    val counterExamples = ListBuffer[(Int, TlaEx)]()
    for (loopStartIndex <- loopStartIndexWithActionTuples) {
      if (checkIfLivenessEventNeverHappened(loopStartIndex, notLiveness)) {
        counterExamples += loopStartIndex
      }
    }

    counterExamples.toList
  }

  private def setBindingAndActionForLastState(lastState: SymbState, selectedState: SymbState, action: TlaEx): SymbState = {
    val requiredBinding = selectedState.binding
    val state = lastState.setBinding(requiredBinding).setRex(action)

    rewriter.rewriteUntilDone(state.setTheory(CellTheory()))
  }

  def checkFairnessOfCounterExamples(counterExampleLoopStartIndexWithActionTuples: List[(Int, TlaEx)]): Boolean = {
    val notLiveness = tla.not(checkerInput.liveness.get)

    def doesFairnessHold(loopStartIndexWithActionTuple: (Int, TlaEx), weakFairnessConjunction: TlaEx, actions: List[TlaEx]): Boolean = {
      rewriter.push()

      var j = 0
      var lastState = stateStack.head._1
      lastState = rewriter.rewriteUntilDone(addPrimedBinding(lastState, stateStack(loopStartIndexWithActionTuple._1)._1).setTheory(CellTheory()))
      solverContext.assertGroundExpr(lastState.ex)

      while (j <= loopStartIndexWithActionTuple._1) {
        lastState = setBindingAndActionForLastState(lastState, stateStack(j)._1, weakFairnessConjunction)
        solverContext.assertGroundExpr(lastState.ex)

        lastState = setBindingAndActionForLastState(lastState, stateStack(j)._1, notLiveness)
        solverContext.assertGroundExpr(lastState.ex)

        j += 1
      }

      def isTaken(action: TlaEx, loopStartIndex: Int): Boolean = {
        var i = 0
        while (i <= loopStartIndex) {
          rewriter.push()

          val requiredBinding = stateStack(i)._1.binding
          val state = addPrimedBinding(
            lastState.setBinding(requiredBinding),
            stateStack(if (i - 1 >= 0) i - 1 else loopStartIndex)._1
            ).setRex(action)
          lastState = rewriter.rewriteUntilDone(state.setTheory(CellTheory()))
          solverContext.assertGroundExpr(lastState.ex)
          i += 1

          val taken = solverContext.sat()

          rewriter.pop()

          if (taken) {
            return true
          }
        }

        false
      }

      val result = actions.forall(it => isTaken(it, loopStartIndexWithActionTuple._1))

      rewriter.pop()

      result
    }

    val weakFairnessConjunction = tla.and(checkerInput.enabledActionWeakFairnessHintTuples.get.map(it => it._2): _*)
    val actions = checkerInput.enabledActionWeakFairnessHintTuples.get.map(it => it._1).map(it => convertToEquality(it))

    counterExampleLoopStartIndexWithActionTuples.exists(doesFairnessHold(_, weakFairnessConjunction, actions))
  }

  private def addPrimedBinding(state: SymbState, selected: SymbState): SymbState = {
    val selectedBinding = selected.binding
    val stateBinding = state.binding
    val stateWithBinding = state.setBinding(
      Binding(stateBinding.merged(selectedBinding.map { t => (t._1 + "'", t._2) })((k, _) => k))
    )

    stateWithBinding
  }
}