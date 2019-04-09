package at.forsyte.apalache.tla.bmcmt.analyses

import at.forsyte.apalache.tla.bmcmt.{ArenaCell, Binding, CellTheory, SolverContext, SymbState, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.{OperEx, TlaEx}
import at.forsyte.apalache.tla.lir.oper.{TlaOper, TlaSetOper}
import at.forsyte.apalache.tla.lir.convenience.tla

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//TODO (Viktor): write unit-tests
class LoopAnalyser(val nextTransitions: List[TlaEx],
                   val liveness: TlaEx,
                   val enabledActionHintTuples: List[(TlaEx, TlaEx)],
                   var stateStack: List[(SymbState, ArenaCell)],
                   val rewriter: SymbStateRewriterImpl,
                   val solverContext: SolverContext) {

  def findAllLoops: List[Int] = {
    val loopStartIndexes = mutable.Set[Int]()

    val next = nextTransitions.map(convertToEquality)
    for (action <- next) {
      loopStartIndexes ++= findLoopsForAction(action)
    }

    loopStartIndexes.toList
  }

  private def findLoopsForAction(action: TlaEx): ListBuffer[Int] = {
    val loopStartIndexes = ListBuffer[Int]()

    val lastState = setActionForLastState(action)
    for (loopStartIndex <- 0 until stateStack.size - 1) {
      val loopStartState = stateStack(loopStartIndex)._1
      if (checkForLoopWithAction(loopStartState, lastState)) {
        loopStartIndexes += loopStartIndex
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

  def checkLiveness(loopStartIndexActionTuples: List[Int]): List[Int] = {
    val notLiveness = tla.not(liveness)

    val counterExamples = ListBuffer[Int]()
    for (loopStartIndex <- loopStartIndexActionTuples) {
      if (checkIfLivenessEventNeverHappened(loopStartIndex, notLiveness)) {
        counterExamples += loopStartIndex
      }
    }

    counterExamples.toList
  }

  private def checkIfLivenessEventNeverHappened(loopStartIndex: Int, notLiveness: TlaEx): Boolean = {
    rewriter.push()

    var lastState = stateStack.head._1
    for (j <- 0 to loopStartIndex) {
      lastState = setBindingAndActionForLastState(lastState, stateStack(j)._1, notLiveness)
      solverContext.assertGroundExpr(lastState.ex)
    }
    val result = solverContext.sat()

    rewriter.pop()

    result
  }

  private def setBindingAndActionForLastState(lastState: SymbState, selectedState: SymbState, action: TlaEx): SymbState = {
    val requiredBinding = selectedState.binding
    val state = lastState.setBinding(requiredBinding).setRex(action)

    rewriter.rewriteUntilDone(state.setTheory(CellTheory()))
  }

  def checkFairnessOfCounterExamples(counterExampleLoopStartIndexes: List[Int]): List[Int] = {
    def areActionsEnabled(loopStartIndex: Int, weakFairnessConjunction: TlaEx): Boolean = {
      rewriter.push()

      var j = 0
      var lastState = stateStack.head._1
      while (j <= loopStartIndex) {
        lastState = setBindingAndActionForLastState(lastState, stateStack(j)._1, weakFairnessConjunction)
        solverContext.assertGroundExpr(lastState.ex)

        j += 1
      }
      val result = solverContext.sat()

      rewriter.pop()

      result
    }

    def areActionsTaken(loopStartIndex: Int, actions: List[TlaEx]): Boolean = {
      def isTaken(action: TlaEx, loopStartIndex: Int): Boolean = {
        var i = 0
        var lastState = stateStack.head._1
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

      actions.forall(it => isTaken(it, loopStartIndex))
    }

    val weakFairnessConjunction = tla.and(enabledActionHintTuples.map(it => it._2): _*)
    val actions = enabledActionHintTuples.map(it => it._1).map( it => convertToEquality(it))

    val temp = counterExampleLoopStartIndexes.filter(it => areActionsEnabled(it, weakFairnessConjunction))
    temp.filter(it => areActionsTaken(it, actions))
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