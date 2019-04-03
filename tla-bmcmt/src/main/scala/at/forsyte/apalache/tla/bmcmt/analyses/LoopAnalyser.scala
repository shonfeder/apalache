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

  def findAllLoops: List[(Int, TlaEx)] = {
    def setActionForLastState(action: TlaEx): SymbState = {
      var last = stateStack.head
      val state = last._1.setRex(action)
      last = (state, last._2)
      stateStack = last :: stateStack.tail

      state
    }

    def checkForLoopWithAction(stateNumber: Int, lastState: SymbState): Boolean = {
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

    val next = nextTransitions.map { it => convertToEquality(it) }

    val loopStartIndexes = ListBuffer[(Int, TlaEx)]()
    for (i <- next) {
      val last = setActionForLastState(i)

      for (j <- 0 until stateStack.size - 1) {
        if (checkForLoopWithAction(j, last)) {
          val tuple = (j, i)
          loopStartIndexes += tuple
        }
      }
    }

    loopStartIndexes.toList
  }

  private def convertToEquality(ex: TlaEx): TlaEx = ex match {
    case OperEx(TlaSetOper.in, arg1, OperEx(TlaSetOper.enumSet, arg)) =>
      OperEx(TlaOper.eq, arg1, arg)
    case OperEx(oper, args@_*) => OperEx(oper, args.map(it => convertToEquality(it)): _*)
    case it => it
  }

  def validateLiveness(loopStartIndexActionTuples: List[(Int, TlaEx)]): List[(Int, TlaEx)] = {
    val notLoopInvariant = tla.not(liveness)

    val counterExamples = ListBuffer[(Int, TlaEx)]()

    for (startIndex <- loopStartIndexActionTuples) {

      rewriter.push()

      var j = 0
      var lastState = stateStack.head._1
      while (j <= startIndex._1) {
        val requiredBinding = stateStack(j)._1.binding
        val state = lastState.setBinding(requiredBinding).setRex(notLoopInvariant)
        lastState = rewriter.rewriteUntilDone(state.setTheory(CellTheory()))
        solverContext.assertGroundExpr(lastState.ex)

        j += 1
      }
      val result = solverContext.sat()

      if (result) {
        counterExamples += startIndex
      }

      rewriter.pop()
    }

    counterExamples.toList
  }

  def checkFairnessOfCounterExamples(counterExampleLoopStartIndexes: List[(Int, TlaEx)]): List[(Int, TlaEx)] = {
    def filterByEnabledActions(counterExampleLoopStartIndexActionTuples: List[(Int, TlaEx)]): List[(Int, TlaEx)] = {
      val filteredCounterExamples = ListBuffer[(Int, TlaEx)]()

      val weakFairnessConjunction = tla.and(enabledActionHintTuples.map(it => it._2): _*)
      for (startIndex <- counterExampleLoopStartIndexActionTuples) {
        rewriter.push()

        var j = 0
        var lastState = stateStack.head._1
        while (j <= startIndex._1) {
          val requiredBinding = stateStack(j)._1.binding
          val state = lastState.setBinding(requiredBinding).setRex(weakFairnessConjunction)
          lastState = rewriter.rewriteUntilDone(state.setTheory(CellTheory()))
          solverContext.assertGroundExpr(lastState.ex)

          j += 1
        }
        val result = solverContext.sat()
        if (result) {
          filteredCounterExamples += startIndex
        }

        rewriter.pop()
      }

      filteredCounterExamples.toList
    }

    def filterByTakenActions(counterExampleLoopStartIndexActionTuples: List[(Int, TlaEx)]): List[(Int, TlaEx)] = {

      def collectTakenActions(actions: List[TlaEx], loopStartIndexActionTuple: (Int, TlaEx)): List[TlaEx] = {
        val takenActions = ListBuffer[TlaEx]()
        takenActions += loopStartIndexActionTuple._2

        var j = 0
        var lastState = stateStack.head._1
        var taken = false
        while (j <= loopStartIndexActionTuple._1 && !taken) {
          taken = false
          for (toBeTakenAction <- actions) {
            rewriter.push()

            val requiredBinding = stateStack(j)._1.binding
            val state = addPrimedBinding(
              lastState.setBinding(requiredBinding),
              stateStack(if (j - 1 >= 0) j - 1 else 0)._1
              ).setRex(toBeTakenAction)
            lastState = rewriter.rewriteUntilDone(state.setTheory(CellTheory()))
            solverContext.assertGroundExpr(lastState.ex)
            j += 1

            taken = solverContext.sat()

            if (taken) {
              takenActions += toBeTakenAction
            }

            rewriter.pop()
          }
        }

        takenActions.toList
      }

      val filteredCounterExamples = ListBuffer[(Int, TlaEx)]()

      val actions = enabledActionHintTuples.map(it => it._1).map( it => convertToEquality(it))


      var takenActions = List[TlaEx]()
      for (startIndex <- counterExampleLoopStartIndexActionTuples) {

        takenActions = collectTakenActions(actions, startIndex)

        if (actions.forall(takenActions.contains)) {
          filteredCounterExamples += startIndex
        }
      }

      filteredCounterExamples.toList
    }

    val filteredByEnabled = filterByEnabledActions(counterExampleLoopStartIndexes)
    filterByTakenActions(filteredByEnabled)
  }

  def addPrimedBinding(state: SymbState, selected: SymbState): SymbState = {
    val selectedBinding = selected.binding
    val stateBinding = state.binding
    val stateWithBinding = state
                           .setBinding(Binding(stateBinding
                                               .merged(selectedBinding.map { t => (t._1 + "'", t._2) })((k, _) => k)))

    stateWithBinding
  }
}