package at.forsyte.apalache.tla.boolka

import at.forsyte.apalache.tla.bmcmt.CheckerInput
import at.forsyte.apalache.tla.bmcmt.smt.SolverContext
import at.forsyte.apalache.tla.bmcmt.trex.TransitionExecutor
import com.typesafe.scalalogging.LazyLogging

/**
  * Abstractor computes the predicate abstraction of a TLA+ formula (either a state formula, or an action formula).
  *
  * @author Igor Konnov
  */
class Boolifier[ExecutorContextT](val checkerInput: CheckerInput,
                                  val boolifierInput: BoolifierInput,
                                  solver: SolverContext,
                                  trex: TransitionExecutor[ExecutorContextT]) extends LazyLogging {
  def compute(): BoolSys = {
    // initialize CONSTANTS
    if (checkerInput.constInitPrimed.isDefined) {
      trex.initializeConstants(checkerInput.constInitPrimed.get)
    }
    // construct the Boolean abstraction
    val sys = new BoolSys(boolifierInput.preds.size)
    logger.debug(s"Constructing predicate abstraction of Init with %d predicates".format(boolifierInput.preds.size))
    sys.init = abstractInit
    logger.debug(s"Constructing predicate abstraction of (TypeOK; Next) with %d predicates"
      .format(2 * boolifierInput.preds.size))
    applyTypeOk()
    sys.next = abstractNext
    logger.debug(s"Constructing predicate abstraction of ~Inv with %d predicates".format(boolifierInput.preds.size))
    sys.notInv = abstractNotInv()
    sys
  }

  private def abstractInit: List[Cube] = {
    var cubes = List[Cube]()
    for ((tr, no) <- checkerInput.initTransitions.zipWithIndex) {
      val snapshot = trex.snapshot()
      val translatedOk = trex.prepareTransition(no, tr)
      if (translatedOk) {
        trex.assumeTransition(no)
        trex.nextState()
        // introduce predicates as cells
        val predNames = boolifierInput.preds.map(trex.translateStateExpr)
        // enumerate all cubes
        val transitionCubes = new CubeFinder(solver, predNames).allCubes()
        logger.debug(s"Transition $no introduced ${transitionCubes.length} cubes")
        cubes = cubes ++ transitionCubes
      }
      trex.recover(snapshot)
    }
    cubes
  }

  private def abstractNotInv(): List[Cube] = {
    var cubes = List[Cube]()
    for (((_, notInv), no) <- checkerInput.invariantsAndNegations.zipWithIndex) {
      val snapshot = trex.snapshot()
      trex.assertState(notInv)
      // introduce predicates as cells
      val predNames = boolifierInput.preds.map(trex.translateStateExpr)
      // enumerate all cubes
      val notInvCubes = new CubeFinder(solver, predNames).allCubes()
      logger.debug(s"Transition $no introduced ${notInvCubes.length} cubes")
      cubes = cubes ++ notInvCubes
      trex.recover(snapshot)
    }

    cubes
  }

  private def applyTypeOk(): Unit = {
    // translate TypeOK and collect the predicates after the transition has happened
    for ((tr, no) <- boolifierInput.typeOkPrimed.zipWithIndex) {
      val translatedOk = trex.prepareTransition(no, tr)
      if (!translatedOk) {
        throw new AbstractionException(s"TypeOK does not encode a valid initialization predicate (transition $no)")
      }
    }

    trex.pickTransition()
    trex.nextState()
  }

  private def abstractNext: List[Cube] = {
    val mainSnapshot = trex.snapshot()
    val prevPreds = boolifierInput.preds.map(trex.translateStateExpr)
    var cubes = List[Cube]()
    for ((tr, no) <- checkerInput.nextTransitions.zipWithIndex) {
      val snapshot = trex.snapshot()
      val translatedOk = trex.prepareTransition(no, tr)
      if (translatedOk) {
        trex.assumeTransition(no)
        trex.nextState()
        // introduce predicates as cells
        val nextPreds = boolifierInput.preds.map(trex.translateStateExpr)
        // enumerate all cubes
        val transitionCubes = new CubeFinder(solver, prevPreds ++ nextPreds).allCubes()
        logger.debug(s"Transition $no introduced ${transitionCubes.length} cubes")
        cubes = cubes ++ transitionCubes
      }
      trex.recover(snapshot)
    }
    trex.recover(mainSnapshot)
    cubes
  }
}
