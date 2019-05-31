package at.forsyte.apalache.tla.bmcmt.analyses

import at.forsyte.apalache.tla.bmcmt.types.IntT
import at.forsyte.apalache.tla.bmcmt.{Arena, ArenaCell, Binding, CellTheory, CheckerInput, SolverContext, SymbState, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.actions.TlaActionOper
import at.forsyte.apalache.tla.lir.convenience.tla
import at.forsyte.apalache.tla.lir.{NameEx, OperEx, TlaEx, ValEx}
import at.forsyte.apalache.tla.lir.oper.{TlaArithOper, TlaBoolOper, TlaOper, TlaSetOper}
import at.forsyte.apalache.tla.lir.temporal.TlaTempOper
import at.forsyte.apalache.tla.lir.values.TlaInt

import scala.collection.mutable.ListBuffer


class SymbolicLoopAnalyzer(val checkerInput: CheckerInput,
                           var stateStack: List[(SymbState, ArenaCell)],
                           val rewriter: SymbStateRewriterImpl,
                           val solverContext: SolverContext) extends LoopAnalyser {

  private var actualCellArena: Arena = _
  private var lambda: ArenaCell = _
  private val notLiveness: TlaEx = negateLiveness(checkerInput.liveness.get)

  private def negateLiveness(liveness: TlaEx): TlaEx = liveness match {
    case OperEx(TlaTempOper.diamond, arg) =>
      OperEx(TlaTempOper.box, negateLiveness(arg))
    case OperEx(TlaTempOper.box, arg) =>
      OperEx(TlaTempOper.diamond, negateLiveness(arg))
    case OperEx(TlaBoolOper.implies, left, right) =>
      OperEx(TlaBoolOper.and, negateLiveness(left), negateLiveness(right))
    case OperEx(TlaBoolOper.and, args@_*) =>
      OperEx(TlaBoolOper.or, args.map(negateLiveness):_*)
    case OperEx(TlaBoolOper.or, args@_*) =>
      OperEx(TlaBoolOper.and, args.map(negateLiveness):_*)
    case OperEx(operator: TlaArithOper, args@_*) =>
      tla.not(OperEx(operator, args:_*))
    case OperEx(TlaActionOper.nostutter, formula, _) =>
      negateLiveness(formula)
    case OperEx(TlaTempOper.leadsTo, left, right) =>
      OperEx(TlaTempOper.diamond, OperEx(TlaBoolOper.and, left, OperEx(TlaTempOper.box, OperEx(TlaBoolOper.not, right))))
    case OperEx(TlaOper.eq, args@_*) =>
      OperEx(TlaOper.ne, args:_*)
    case OperEx(TlaBoolOper.forall, value, set, arg) =>
      OperEx(TlaBoolOper.exists, value, set, negateLiveness(arg))
    case OperEx(TlaOper.ne, left, right) =>
      OperEx(TlaOper.eq, left, right)
    case OperEx(TlaBoolOper.not, arg) =>
      arg
    case OperEx(TlaBoolOper.exists, value, set, arg) =>
      OperEx(TlaBoolOper.forall, value, set, negateLiveness(arg))
    case OperEx(TlaSetOper.in, value, set) =>
      OperEx(TlaSetOper.notin, value, set)
    case _ =>
      throw new RuntimeException("Unhandled pattern")
  }

  def checkNotLiveness: Boolean = {
    rewriter.push()
    appendLoopStartCell()

    val fairNotLivenessExpression = buildFairNotLivenessExpression
    val rewrittenState = rewriter.rewriteUntilDone(stateStack.head._1.setArena(actualCellArena).setRex(fairNotLivenessExpression).setTheory(CellTheory()))
    actualCellArena = rewrittenState.arena
    solverContext.assertGroundExpr(rewrittenState.ex)
    val result = solverContext.sat()
    rewriter.pop()

    result
  }

  private def appendLoopStartCell(): Unit = {
    var lastState = stateStack.head._1
    actualCellArena = lastState.arena.appendCell(IntT())
    lambda = actualCellArena.topCell

    lastState = lastState.setArena(actualCellArena)
    val tuple = (lastState, stateStack.head._2)
    stateStack = tuple :: stateStack.tail
  }

  private def buildFairNotLivenessExpression: TlaEx = {
    val loopConstraint = buildLoopConstraint
    val notLivenessConstraint = buildNotLivenessConstraint()
    val weakFairnessConstraint = buildWeakFairnessCondition()
    val strongFairnessConstraint = buildStrongFairnessCondition()
    //TODO: WF, SF

    OperEx(TlaBoolOper.and, loopConstraint, notLivenessConstraint, weakFairnessConstraint, )
  }

  private def buildLoopConstraint: TlaEx = {
    val loopImplications = buildLoopImplications
    OperEx(TlaBoolOper.and, loopImplications:_*)
  }

  private def buildLoopImplications: List[TlaEx] = {
    val loopImplications = ListBuffer[TlaEx]()

    var lastState = stateStack.head._1
    lastState = lastState.setBinding(lastState.binding.merged(Binding(("lambda", lambda)))((k, _) => k))

    for (action <- checkerInput.nextTransitions) {
      loopImplications ++= buildLoopImplicationsForAction(action, lastState)
    }

    loopImplications.toList
  }

  private def buildLoopImplicationsForAction(action: TlaEx, lastState: SymbState): List[TlaEx] = {
    val loopImplications = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 1) {
      val lastWithImplication = lastState.setRex(buildLoopImplication(i, lastState, action))
      val rewrittenState = rewriter.rewriteUntilDone(lastWithImplication.setArena(actualCellArena).setTheory(CellTheory()))
      actualCellArena = rewrittenState.arena
      loopImplications += rewrittenState.ex
    }

    loopImplications.toList
  }

  private def buildLoopImplication(index: Int, lastState: SymbState, action: TlaEx): TlaEx = {
    val equality = OperEx(TlaOper.eq, ValEx(TlaInt(index)), NameEx("lambda"))
    val transitionCondition = buildTransitionCondition(action, lastState, index)

    OperEx(TlaBoolOper.implies, equality, transitionCondition)
  }

  private def buildTransitionCondition(action: TlaEx, lastState: SymbState, index: Int): TlaEx = {
    val lastWithAction = setActionForLastState(action, lastState)
    val lastWithPrimedBinding = addPrimedTargetBinding(lastWithAction, stateStack(index)._1).setArena(actualCellArena)
    val rewrittenState = rewriter.rewriteUntilDone(lastWithPrimedBinding.setTheory(CellTheory()))
    actualCellArena = rewrittenState.arena
    rewrittenState.ex
  }

  private def addPrimedTargetBinding(source: SymbState, target: SymbState): SymbState = {
    val selectedBinding = target.binding
    val stateBinding = source.binding
    val stateWithBinding = source.setBinding(
      Binding(stateBinding.merged(selectedBinding.map { t => (t._1 + "'", t._2) })((k, _) => k))
      )

    stateWithBinding
  }

  private def setActionForLastState(action: TlaEx, lastState: SymbState): SymbState = lastState.setRex(action)

  private def buildNotLivenessConstraint(): TlaEx = notLiveness match {
    case OperEx(TlaTempOper.diamond, OperEx(TlaTempOper.box, arg)) =>
      OperEx(TlaBoolOper.and, buildNotLivenessConditionsForStates(arg):_*)
    case OperEx(TlaTempOper.box, OperEx(TlaTempOper.diamond, arg)) =>
      OperEx(TlaBoolOper.or, buildNotLivenessConditionsForStates(arg):_*)
    case OperEx(TlaTempOper.diamond, OperEx(TlaBoolOper.and, left, OperEx(TlaTempOper.box, right))) =>
      OperEx(
        TlaBoolOper.or,
        stateStack.indices
        .map(index => OperEx(
          TlaBoolOper.and,
          Seq(
            buildNotLivenessConditionForState(index, left),
            OperEx(
              TlaBoolOper.and,
              buildNotLivenessConditionsForStates(index, stateStack.size - 1, right):_*
              )
            ):_*
          )
             ):_*
        )
    case OperEx(TlaTempOper.box, arg) =>
      OperEx(TlaBoolOper.and, buildNotLivenessConditionsForStates(0, stateStack.size - 1, arg):_*)
    case _ =>
      throw new RuntimeException("Unhandled pattern")
  }

  private def buildNotLivenessConditionsForStates(notLiveness: TlaEx): List[TlaEx] = {
    val notLivenessStateConditions = ListBuffer[TlaEx]()

    var lastState = stateStack.head._1
    lastState = lastState.setBinding(lastState.binding.merged(Binding(("lambda", lambda)))((k, _) => k))

    for (i <- 0 until stateStack.size - 1) {
      notLivenessStateConditions += buildNotLivenessConditionForState(i, notLiveness, lastState)
    }

    notLivenessStateConditions.toList
  }

  private def buildNotLivenessConditionsForStates(firstState: Int, lastState: Int, notLiveness: TlaEx): List[TlaEx] =  {
    val notLivenessStateConditions = ListBuffer[TlaEx]()

    for (i <- firstState to lastState) {
      notLivenessStateConditions += buildNotLivenessConditionForState(i, notLiveness)
    }

    notLivenessStateConditions.toList
  }

  private def buildNotLivenessConditionForState(index: Int, notLiveness: TlaEx): TlaEx = {
    val equality = OperEx(TlaArithOper.le, ValEx(TlaInt(index)), NameEx("lambda"))

    var consideringState = stateStack(index)._1.setArena(actualCellArena).setRex(notLiveness)
    consideringState = consideringState.setBinding(consideringState.binding.merged(Binding(("lambda", lambda)))((k, _) => k))
    val rewrittenState = rewriter.rewriteUntilDone(consideringState.setTheory(CellTheory()))
    actualCellArena = rewrittenState.arena
    val notLivenessForState = rewrittenState.ex

    val lastWithNotLiveness = consideringState.setRex(OperEx(TlaBoolOper.and, equality, notLivenessForState))
    val rewrittenStateWithNotLiveness = rewriter.rewriteUntilDone(lastWithNotLiveness.setArena(actualCellArena).setTheory(CellTheory()))
    actualCellArena = rewrittenState.arena
    rewrittenStateWithNotLiveness.ex
  }

  private def buildNotLivenessConditionForState(index: Int, notLiveness: TlaEx, lastState: SymbState): TlaEx = {
    val equality = OperEx(TlaArithOper.le, ValEx(TlaInt(index)), NameEx("lambda"))
    val notLivenessCondition = buildNotLivenessConditionForState(notLiveness, lastState)

    val lastWithNotLiveness = lastState.setRex(OperEx(TlaBoolOper.and, equality, notLivenessCondition))
    val rewrittenState = rewriter.rewriteUntilDone(lastWithNotLiveness.setArena(actualCellArena).setTheory(CellTheory()))
    actualCellArena = rewrittenState.arena
    rewrittenState.ex
  }

  private def buildNotLivenessConditionForState(notLiveness: TlaEx, lastState: SymbState): TlaEx = {
    val rewrittenState = rewriter.rewriteUntilDone(lastState.setArena(actualCellArena).setRex(notLiveness))
    actualCellArena = rewrittenState.arena
    rewrittenState.ex
  }

  private def buildWeakFairnessCondition(): TlaEx = {
    val weakFairnessActionWithHintTuples = checkerInput.enabledActionWeakFairnessHintTuples.get
    val weakFairnessConditions = weakFairnessActionWithHintTuples.map(it => buildWeakFairnessConditionForAction(it))

    OperEx(TlaBoolOper.and, weakFairnessConditions:_*)
  }

  private def buildWeakFairnessConditionForAction(actionAndHint: (TlaEx, TlaEx)): TlaEx = {
    val enabledCondition = buildWeaklyEnabledCondition(actionAndHint._2)
    val takenCondition = buildTakenCondition(actionAndHint._1)

    OperEx(TlaBoolOper.implies, enabledCondition, takenCondition)
  }

  private def buildWeaklyEnabledCondition(hint: TlaEx): TlaEx = {
    val enabledStateConditions = generateEnabledConditions(hint)

    OperEx(TlaBoolOper.and, enabledStateConditions:_*)
  }

  private def generateEnabledConditions(hint: TlaEx): ListBuffer[TlaEx] = {
    val enabledHintsConditions = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 1) {
      val enabledHint = applyHintOnState(hint, i)
      enabledHintsConditions += enabledHint
    }

    enabledHintsConditions
  }

  private def applyHintOnState(hint: TlaEx, index: Int): TlaEx = {
    val equality = OperEx(TlaArithOper.le, ValEx(TlaInt(index)), NameEx("lambda"))

    var stateWithHint = stateStack(index)._1.setArena(actualCellArena).setRex(hint)
    stateWithHint = stateWithHint.setBinding(stateWithHint.binding.merged(Binding(("lambda", lambda)))((k, _) => k))
    val rewrittenState = rewriter.rewriteUntilDone(stateWithHint)
    actualCellArena = rewrittenState.arena
    val hintExpression = rewrittenState.ex

    val lastWithNotLiveness = rewrittenState.setRex(OperEx(TlaBoolOper.and, equality, hintExpression))
    val rewrittenStateWithNotLiveness = rewriter.rewriteUntilDone(lastWithNotLiveness.setArena(actualCellArena).setTheory(CellTheory()))
    actualCellArena = rewrittenStateWithNotLiveness.arena
    rewrittenStateWithNotLiveness.ex
  }

  private def buildTakenCondition(action: TlaEx): TlaEx = {
    val takenStateConditions = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 1) {
      val equality = OperEx(TlaArithOper.le, ValEx(TlaInt(i)), NameEx("lambda"))

      val targetState = stateStack(i)._1
      val sourceIndex = i + 1
      var sourceState = stateStack(sourceIndex)._1
      sourceState = sourceState.setBinding(sourceState.binding.merged(Binding(("lambda", lambda)))((k, _) => k))
      val sourceWithPrimedBinding = addPrimedTargetBinding(sourceState, targetState).setArena(actualCellArena).setRex(action)
      val rewrittenState = rewriter.rewriteUntilDone(sourceWithPrimedBinding.setTheory(CellTheory()))
      actualCellArena = rewrittenState.arena
      val takenExpr = rewrittenState.ex

      val lastWithNotLiveness = rewrittenState.setRex(OperEx(TlaBoolOper.and, equality, takenExpr))
      val rewrittenStateWithNotLiveness = rewriter.rewriteUntilDone(lastWithNotLiveness.setArena(actualCellArena).setTheory(CellTheory()))
      actualCellArena = rewrittenStateWithNotLiveness.arena

      takenStateConditions += rewrittenStateWithNotLiveness.ex
    }

    OperEx(TlaBoolOper.or, takenStateConditions:_*)
  }

  def buildStrongFairnessCondition(): TlaEx = {
    val strongFairnessActionWithHintTuples = checkerInput.enabledActionStrongFairnessHintTuples.get
    val strongFairnessConditions = strongFairnessActionWithHintTuples.map(it => buildStrongFairnessConditionForAction(it))

    OperEx(TlaBoolOper.and, strongFairnessConditions:_*)
  }

  private def buildStrongFairnessConditionForAction(loopStartStateIndex: Int, actionAndHint: (TlaEx, TlaEx)): TlaEx = {
    val enabledCondition = buildStronglyEnabledCondition(actionAndHint._2)
    val takenCondition = buildTakenCondition(actionAndHint._1)

    OperEx(TlaBoolOper.implies, enabledCondition, takenCondition)
  }

  private def buildStronglyEnabledCondition(hint: TlaEx): TlaEx = {
    val enabledStateConditions = generateEnabledConditions(hint)

    OperEx(TlaBoolOper.or, enabledStateConditions:_*)
  }
}
