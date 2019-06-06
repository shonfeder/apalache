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

  def checkNotLiveness(): Boolean = {
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
    lastState = lastState.setBinding(lastState.binding.merged(Binding(("lambda", lambda)))((k, _) => k))
    val tuple = (lastState, stateStack.head._2)
    stateStack = tuple :: stateStack.tail
  }

  private def buildFairNotLivenessExpression: TlaEx = {
    val loopConstraint = buildLoopConstraint()
    val notLivenessConstraint = buildNotLivenessConstraint()
    val weakFairnessConstraint = buildWeakFairnessConstraint()
    val strongFairnessConstraint = buildStrongFairnessConstraint()

    OperEx(TlaBoolOper.and, loopConstraint, notLivenessConstraint, weakFairnessConstraint, strongFairnessConstraint)
  }

  private def buildLoopConstraint(): TlaEx = {
    val loopImplications = buildLoopImplications()
    OperEx(TlaBoolOper.and, loopImplications:_*)
  }

  private def buildLoopImplications(): List[TlaEx] = {
    val loopImplications = ListBuffer[TlaEx]()

    for (action <- checkerInput.nextTransitions) {
      loopImplications ++= buildLoopImplicationsForAction(action)
    }

    loopImplications.toList
  }

  private def buildLoopImplicationsForAction(action: TlaEx): List[TlaEx] = {
    val loopImplications = ListBuffer[TlaEx]()

    val lastState = stateStack.head._1

    for (i <- 0 until stateStack.size - 1) {
      val lastWithPrimed = addPrimedTargetBinding(lastState, stateStack(i)._1)
      val lastWithImplication = lastWithPrimed.setRex(OperEx(TlaBoolOper.implies, getLoopCondition(i), action))
      loopImplications += rewrite(lastWithImplication)
    }

    loopImplications.toList
  }

  private def getLoopCondition(i: Int): TlaEx = OperEx(TlaOper.eq, ValEx(TlaInt(i)), NameEx("lambda"))

  private def rewrite(state: SymbState): TlaEx = {
    val rewrittenState = rewriter.rewriteUntilDone(state.setArena(actualCellArena).setTheory(CellTheory()))
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

  private def buildNotLivenessConstraint(): TlaEx = notLiveness match {
    case OperEx(TlaTempOper.diamond, OperEx(TlaTempOper.box, arg)) =>
      OperEx(TlaBoolOper.and, buildNotLivenessConditionsForStates(arg):_*)
    case OperEx(TlaTempOper.box, OperEx(TlaTempOper.diamond, arg)) =>
      OperEx(TlaBoolOper.or, buildNotLivenessConditionsForStates(arg):_*)
    case OperEx(TlaTempOper.diamond, OperEx(TlaBoolOper.and, left, OperEx(TlaTempOper.box, right))) =>
      OperEx(TlaBoolOper.or, buildLeadsToCondition(left, right):_*)
    case OperEx(TlaTempOper.box, arg) =>
      OperEx(TlaBoolOper.and, buildNotLivenessConditionsForStates(arg):_*)
    case _ =>
      throw new RuntimeException("Unhandled pattern")
  }

  private def buildNotLivenessConditionsForStates(notLiveness: TlaEx): List[TlaEx] = {
    val notLivenessStateConditions = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 1) {
      val state = stateStack(i)._1
      val stateWithLambda = addLambdaBinding(state)
      val stateWithExpression = stateWithLambda.setRex(wrapWithInLoopCondition(i, notLiveness))
      val rewrittenExpression = rewrite(stateWithExpression)
      notLivenessStateConditions += rewrittenExpression
    }

    notLivenessStateConditions.toList
  }

  private def wrapWithInLoopCondition(index: Int, notLiveness: TlaEx): TlaEx = {
    OperEx(TlaBoolOper.and, OperEx(TlaArithOper.ge, ValEx(TlaInt(index)), NameEx("lambda")), notLiveness)
  }

  private def addLambdaBinding(state: SymbState): SymbState = {
    state.setBinding(state.binding.merged(Binding(("lambda", lambda)))((k, _) => k))
  }

  private def buildLeadsToCondition(left: TlaEx, right: TlaEx): List[TlaEx] = {
    val notLivenessStateConditions = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 1) {
      val lastState = stateStack.head._1
      val notLiveness = OperEx(TlaBoolOper.or, buildLeadsToNotLiveness(i, left, right):_*)
      val stateWithExpression = lastState.setRex(wrapWithInLoopCondition(i, notLiveness))
      val rewrittenExpression = rewrite(stateWithExpression)
      notLivenessStateConditions += rewrittenExpression
    }

    notLivenessStateConditions.toList
  }

  private def buildLeadsToNotLiveness(loopStartIndex: Int, left: TlaEx, right: TlaEx): List[TlaEx] = {
    val notLivenessStateConditions = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 1) {
      val state = stateStack(i)._1
      val stateWithExpression = state.setRex(OperEx(TlaBoolOper.and, left :: buildActionsForStates(right, i):_*))
      val rewrittenExpression = rewrite(stateWithExpression)
      notLivenessStateConditions += rewrittenExpression
    }

    notLivenessStateConditions.toList
  }

  private def buildActionsForStates(action: TlaEx, start: Int): List[TlaEx] = {
    val actions = ListBuffer[TlaEx]()

    for (i <- 0 to start) {
      val state = stateStack(i)._1
      val stateWithAction = state.setRex(action)
      val rewrittenExpression = rewrite(stateWithAction)
      actions += rewrittenExpression
    }

    actions.toList
  }

  private def buildWeakFairnessConstraint(): TlaEx = {
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
    OperEx(TlaBoolOper.and, generateEnabledConditions(hint, wrapWithInLoopCondition):_*)
  }

  private def generateEnabledConditions(hint: TlaEx, transform: (Int, TlaEx) => TlaEx = (_, it) => it): ListBuffer[TlaEx] = {
    val enabledHintsConditions = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 1) {
      val state = stateStack(i)._1
      val stateWithLambda = addLambdaBinding(state)
      val stateWithHint = stateWithLambda.setRex(transform(i, hint))
      val rewrittenExpression = rewrite(stateWithHint)
      enabledHintsConditions += rewrittenExpression
    }

    enabledHintsConditions
  }

  private def buildTakenCondition(action: TlaEx): TlaEx = {
    val takenStateConditions = ListBuffer[TlaEx]()

    for (i <- 0 until stateStack.size - 2) {
      val targetState = stateStack(i)._1
      val sourceState = stateStack(i + 1)._1
      val stateWithLambda = addLambdaBinding(sourceState)
      val sourceWithAction = addPrimedTargetBinding(stateWithLambda, targetState).setRex(wrapWithInLoopCondition(i, action))
      val rewrittenExpression = rewrite(sourceWithAction)
      takenStateConditions += rewrittenExpression
    }

    for (i <- 0 until stateStack.size - 1) {
      val targetState = stateStack(i)._1
      val sourceState = stateStack.head._1
      val sourceWithAction = addPrimedTargetBinding(sourceState, targetState).setRex(OperEx(TlaBoolOper.and, getLoopStartsCondition(i), action))
      val rewrittenExpression = rewrite(sourceWithAction)
      takenStateConditions += rewrittenExpression
    }

    OperEx(TlaBoolOper.or, takenStateConditions:_*)
  }

  private def getLoopStartsCondition(index: Int): TlaEx = {
    OperEx(TlaOper.eq, ValEx(TlaInt(index)), NameEx("lambda"))
  }

  private def buildStronglyEnabledCondition(hint: TlaEx): TlaEx = {
    OperEx(TlaBoolOper.or, generateEnabledConditions(hint):_*)
  }

  private def buildStrongFairnessConstraint(): TlaEx = {
    val strongFairnessActionWithHintTuples = checkerInput.enabledActionWeakFairnessHintTuples.get
    val strongFairnessConditions = strongFairnessActionWithHintTuples.map(it => buildStrongFairnessConditionForAction(it))

    OperEx(TlaBoolOper.and, strongFairnessConditions:_*)
  }

  private def buildStrongFairnessConditionForAction(actionAndHint: (TlaEx, TlaEx)): TlaEx = {
    val enabledCondition = buildStronglyEnabledCondition(actionAndHint._2)
    val takenCondition = buildTakenCondition(actionAndHint._1)

    OperEx(TlaBoolOper.implies, enabledCondition, takenCondition)
  }
}
