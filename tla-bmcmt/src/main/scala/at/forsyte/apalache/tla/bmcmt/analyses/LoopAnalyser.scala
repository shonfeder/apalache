package at.forsyte.apalache.tla.bmcmt.analyses

import at.forsyte.apalache.tla.bmcmt.{Arena, ArenaCell, Binding, CellTheory, CheckerInput, SolverContext, SymbState, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.{OperEx, TlaEx}
import at.forsyte.apalache.tla.lir.oper.{TlaBoolOper, TlaOper, TlaSetOper}
import at.forsyte.apalache.tla.lir.convenience.tla

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

//TODO (Viktor): write unit-tests
class LoopAnalyser(val checkerInput: CheckerInput,
                   var stateStack: List[(SymbState, ArenaCell)],
                   val rewriter: SymbStateRewriterImpl,
                   val solverContext: SolverContext) {

  private var actualCellArena: Arena = stateStack.head._1.arena
  private val notLiveness: TlaEx = tla.not(checkerInput.liveness.get)

  def findAllLoopStartStateIndexes: List[(Int, TlaEx)] = {
    val loopStartIndexWithActionTuples = mutable.Set[(Int, TlaEx)]()

    val next = checkerInput.nextTransitions.map(convertToEquality)
    for (action <- next) {
      loopStartIndexWithActionTuples ++= findLoopsForAction(action)
    }

    loopStartIndexWithActionTuples.toList
  }

  /**
    * Find all loops for current configuration
    */

  private def convertToEquality(ex: TlaEx): TlaEx = ex match {
    case OperEx(TlaSetOper.in, arg1, OperEx(TlaSetOper.enumSet, arg)) =>
      OperEx(TlaOper.eq, arg1, arg)
    case OperEx(operator, args@_*) => OperEx(operator, args.map(convertToEquality): _*)
    case it => it
  }

  private def findLoopsForAction(action: TlaEx): ListBuffer[(Int, TlaEx)] = {
    val loopStartStateIndexWithActionTuples = ListBuffer[(Int, TlaEx)]()

    val lastState = setActionForLastState(action)
    for (loopStartStateIndex <- 0 until stateStack.size - 1) {
      val loopStartState = stateStack(loopStartStateIndex)._1
      if (checkForLoopWithAction(loopStartState, lastState)) {
        val validTuple = (loopStartStateIndex, action)
        loopStartStateIndexWithActionTuples += validTuple
      }
    }

    loopStartStateIndexWithActionTuples
  }

  private def setActionForLastState(action: TlaEx): SymbState = {
    var lastTuple = stateStack.head
    val state = lastTuple._1.setRex(action)
    lastTuple = (state, lastTuple._2)
    stateStack = lastTuple :: stateStack.tail

    state
  }

  private def checkForLoopWithAction(loopStartState: SymbState, lastState: SymbState): Boolean = {
    val lastWithPrimedBinding = addPrimedTargetBinding(lastState, loopStartState)

    checkLoopTransition(lastWithPrimedBinding)
  }

  private def addPrimedTargetBinding(source: SymbState, target: SymbState): SymbState = {
    val selectedBinding = target.binding
    val stateBinding = source.binding
    val stateWithBinding = source.setBinding(
      Binding(stateBinding.merged(selectedBinding.map { t => (t._1 + "'", t._2) })((k, _) => k))
      )

    stateWithBinding
  }

  private def checkLoopTransition(last: SymbState): Boolean = {
    rewriter.push()
    val ex = rewriter.rewriteUntilDone(last.setTheory(CellTheory())).ex
    solverContext.assertGroundExpr(ex)
    val result = solverContext.sat()
    rewriter.pop()

    result
  }

  /*
   * Find at least liveness fair counter-example
   */
  def checkFairLiveness(loopStartStateIndexWithActionTuples: List[(Int, TlaEx)]): Boolean = {
    val result = loopStartStateIndexWithActionTuples.exists(checkFairNotLiveness)
    result
  }

  private def checkFairNotLiveness(loopStartStateIndexWithAction: (Int, TlaEx)): Boolean = {
    rewriter.push()
    actualCellArena = stateStack.head._1.arena
    val conditionExpression = buildConditionExpression(loopStartStateIndexWithAction)
    val rewrittenState = rewriter.rewriteUntilDone(stateStack.head._1.setArena(actualCellArena).setRex(conditionExpression).setTheory(CellTheory()))
    actualCellArena = rewrittenState.arena
    solverContext.assertGroundExpr(rewrittenState.ex)
    val result = solverContext.sat()
    rewriter.pop()

    result
  }

  private def buildConditionExpression(loopStartStateIndexWithAction: (Int, TlaEx)): TlaEx = {
    val loopStartStateIndex = loopStartStateIndexWithAction._1

    val loopCondition = buildLoopCondition(loopStartStateIndexWithAction)
    val notLivenessCondition = buildNotLivenessCondition(loopStartStateIndex)
    val weakFairnessCondition = buildWeakFairnessCondition(loopStartStateIndex)
    val strongFairnessCondition = buildStrongFairnessCondition(loopStartStateIndex)

    OperEx(TlaBoolOper.and, loopCondition, notLivenessCondition, weakFairnessCondition, strongFairnessCondition)
  }

  private def buildLoopCondition(loopStartStateIndexAndAction: (Int, TlaEx)): TlaEx = {
    var lastState = addPrimedTargetBinding(stateStack.head._1, stateStack(loopStartStateIndexAndAction._1)._1)
    lastState = lastState.setRex(loopStartStateIndexAndAction._2)

    val rewrittenState = rewriter.rewriteUntilDone(lastState.setTheory(CellTheory()))
    actualCellArena = rewrittenState.arena
    rewrittenState.ex
  }

  private def buildNotLivenessCondition(loopStartStateIndex: Int): TlaEx = {
    val notLivenessStateConditions = ListBuffer[TlaEx]()

    for (i <- 0 to loopStartStateIndex) {
      val consideringState = stateStack(i)._1.setArena(actualCellArena).setRex(notLiveness)
      val rewrittenState = rewriter.rewriteUntilDone(consideringState.setTheory(CellTheory()))
      actualCellArena = rewrittenState.arena
      notLivenessStateConditions += rewrittenState.ex
    }

    OperEx(TlaBoolOper.and, notLivenessStateConditions:_*)
  }

  private def buildWeakFairnessCondition(loopStartStateIndex: Int): TlaEx = {
    val weakFairnessActionWithHintTuples = checkerInput.enabledActionWeakFairnessHintTuples.get
    val weakFairnessConditions = weakFairnessActionWithHintTuples.map(it => buildWeakFairnessConditionForAction(loopStartStateIndex, it))

    OperEx(TlaBoolOper.and, weakFairnessConditions:_*)
  }

  private def buildWeakFairnessConditionForAction(loopStartStateIndex: Int, actionAndHint: (TlaEx, TlaEx)): TlaEx = {
    val enabledCondition = buildWeaklyEnabledCondition(loopStartStateIndex, actionAndHint._2)
    val takenCondition = buildTakenCondition(loopStartStateIndex, actionAndHint._1)

    OperEx(TlaBoolOper.implies, enabledCondition, takenCondition)
  }

  private def buildWeaklyEnabledCondition(loopStartStateIndex: Int, hint: TlaEx): TlaEx = {
    val enabledStateConditions = generateEnabledConditions(loopStartStateIndex, hint)

    OperEx(TlaBoolOper.and, enabledStateConditions:_*)
  }

  private def generateEnabledConditions(loopStartStateIndex: Int, hint: TlaEx): ListBuffer[TlaEx] = {
    val enabledHintsConditions = ListBuffer[TlaEx]()

    for (i <- 0 to loopStartStateIndex) {
      val enabledHint = applyHintOnState(hint, stateStack(i)._1)
      enabledHintsConditions += enabledHint
    }

    enabledHintsConditions
  }

  private def applyHintOnState(hint: TlaEx, state: SymbState): TlaEx = {
    val stateWithHint = state.setArena(actualCellArena).setRex(hint)
    val rewrittenState = rewriter.rewriteUntilDone(stateWithHint)
    actualCellArena = rewrittenState.arena
    rewrittenState.ex
  }

  private def buildTakenCondition(loopStartStateIndex: Int, action: TlaEx): TlaEx = {
    val takenStateConditions = ListBuffer[TlaEx]()

    for (i <- 0 to loopStartStateIndex) {
      val targetState = stateStack(i)._1
      val sourceIndex = if (i == loopStartStateIndex) 0 else i + 1
      val sourceState = stateStack(sourceIndex)._1
      val sourceWithPrimedBinding = addPrimedTargetBinding(sourceState, targetState).setArena(actualCellArena).setRex(action)
      val rewrittenState = rewriter.rewriteUntilDone(sourceWithPrimedBinding.setTheory(CellTheory()))
      actualCellArena = rewrittenState.arena
      takenStateConditions += rewrittenState.ex
    }

    OperEx(TlaBoolOper.or, takenStateConditions:_*)
  }

  def buildStrongFairnessCondition(loopStartStateIndex: Int): TlaEx = {
    val strongFairnessActionWithHintTuples = checkerInput.enabledActionStrongFairnessHintTuples.get
    val strongFairnessConditions = strongFairnessActionWithHintTuples.map(it => buildStrongFairnessConditionForAction(loopStartStateIndex, it))

    OperEx(TlaBoolOper.and, strongFairnessConditions:_*)
  }

  private def buildStrongFairnessConditionForAction(loopStartStateIndex: Int, actionAndHint: (TlaEx, TlaEx)): TlaEx = {
    val enabledCondition = buildStronglyEnabledCondition(actionAndHint._2)
    val takenCondition = buildTakenCondition(loopStartStateIndex, actionAndHint._1)

    OperEx(TlaBoolOper.implies, enabledCondition, takenCondition)
  }

  private def buildStronglyEnabledCondition(hint: TlaEx): TlaEx = {
    val enabledStateConditions = generateEnabledConditions(stateStack.size - 1, hint)

    OperEx(TlaBoolOper.or, enabledStateConditions:_*)
  }
}