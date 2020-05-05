package at.forsyte.apalache.tla.boolka

import java.io.File

import at.forsyte.apalache.tla.bmcmt.analyses.ExprGradeStoreImpl
import at.forsyte.apalache.tla.bmcmt.search.ModelCheckerParams
import at.forsyte.apalache.tla.bmcmt.smt.{SolverContext, Z3SolverContext}
import at.forsyte.apalache.tla.bmcmt.trex.{IncrementalExecutorContext, IncrementalSnapshot, TransitionExecutorImpl}
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.bmcmt.{CheckerInput, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.convenience.tla
import at.forsyte.apalache.tla.lir.oper.BmcOper
import at.forsyte.apalache.tla.lir.{OperEx, TlaEx, TlaModule, TlaVarDecl}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Outcome, fixture}

import scala.collection.immutable.BitSet

@RunWith(classOf[JUnitRunner])
class TestBoolifier extends fixture.FunSuite {
  type ExecutorContextT = IncrementalExecutorContext
  type FixtureParam = (SolverContext, ExecutorContextT)

  override protected def withFixture(test: OneArgTest): Outcome = {
    val typeFinder = new TrivialTypeFinder()
    val solver = new Z3SolverContext(debug = true)
    val rewriter = new SymbStateRewriterImpl(solver, typeFinder, new ExprGradeStoreImpl())
    val exeCtx = new IncrementalExecutorContext(rewriter)
    try {
      test((solver, exeCtx))
    } finally {
      rewriter.dispose()
    }
  }

  test("test two predicates") { contexts: (SolverContext, ExecutorContextT) =>
    val solver = contexts._1
    val exeCtx = contexts._2

    val emptySet = tla.withType(tla.enumSet(), tla.enumSet(tla.intSet()))
    // Init == S = 1..5 /\ T = {}
    val initTrans = List(
      tla.and(
        tla.assignPrime(tla.name("S"), tla.dotdot(tla.int(1), tla.int(5))),
        tla.assignPrime(tla.name("T"), emptySet)
      )) ///

    def skolemize(ex: TlaEx) = OperEx(BmcOper.`skolem`, ex)

    val minus =
      tla.filter(
        tla.name("z"),
        tla.name("S"),
        tla.not(tla.eql(tla.name("z"), tla.name("x"))))
    // Next == \E x \in S: S' = S \ {x} /\ T' = T \\union {x}
    val nextTrans =
      List(
        skolemize(tla.exists(
          tla.name("x"),
          tla.name("S"),
          tla.and(
            tla.assignPrime(tla.name("S"), minus),
            tla.assignPrime(tla.name("T"),
              tla.cup(tla.name("T"), tla.enumSet(tla.name("x"))))
          )))) ///

    ///////////////////
    val modSandT = new TlaModule("root", List(TlaVarDecl("S"), TlaVarDecl("T")))
    val fiveInS = tla.in(tla.int(5), tla.name("S"))
    val fiveInT = tla.in(tla.int(5), tla.name("T"))
    val inv = tla.or(fiveInS, fiveInT)
    val notInv = tla.and(tla.not(fiveInS), tla.not(fiveInT))
    val checkerInput = new CheckerInput(modSandT, initTrans, nextTrans, None, List((inv, notInv)))
    val params = new ModelCheckerParams(checkerInput, stepsBound = 0, new File("."), Map(), false)
    val trex = new TransitionExecutorImpl[IncrementalSnapshot](params.consts, params.vars, exeCtx)
    val powset = OperEx(BmcOper.expand, tla.powSet(tla.dotdot(tla.int(1), tla.int(5))))

    // TypeOK as an init constraint

    val typeOkPrimed =
      skolemize(tla.exists(tla.name("t_1"),
        powset,
        skolemize(tla.exists(tla.name("t_2"),
          powset,
          tla.and(
            tla.assignPrime(tla.name("S"), tla.name("t_1")),
            tla.assignPrime(tla.name("T"), tla.name("t_2"))
          ))))) ///

    // two predicates
    val p1 = tla.in(tla.int(5), tla.name("S"))
    val p2 = tla.in(tla.int(5), tla.name("T"))
    val abstractorInput = new BoolifierInput(List(typeOkPrimed), List(p1, p2))
    val boolSys = new Boolifier[IncrementalSnapshot](checkerInput, abstractorInput, solver, trex).compute()
    // the abstraction of Init gives us one cube: p1 /\ ~p2
    assert(List(Cube(2, BitSet(0), BitSet(0, 1))) == boolSys.init)
    val mask = BitSet(0, 1, 2, 3)
    // the abstraction of Next gives us p1 /\ ~p2 \/ ~p1 /\ p2
    val nextCubes = List(
      Cube(4, BitSet(), mask),
      Cube(4, BitSet(0, 1, 3), mask),
      Cube(4, BitSet(1, 3), mask),
      Cube(4, BitSet(0, 2), mask),
      Cube(4, BitSet(0, 3), mask),
      Cube(4, BitSet(0, 1, 2, 3), mask)
    ) ///

    assert(nextCubes.toSet == boolSys.next.toSet)

    // the abstraction of Inv gives us ~p1 /\ ~p2
    assert(List(Cube(2, BitSet(), BitSet(0, 1))) == boolSys.notInv)
  }
}
