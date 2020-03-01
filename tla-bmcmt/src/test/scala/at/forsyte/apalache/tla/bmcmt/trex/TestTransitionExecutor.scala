package at.forsyte.apalache.tla.bmcmt.trex

import at.forsyte.apalache.tla.bmcmt.SymbStateRewriterImpl
import at.forsyte.apalache.tla.bmcmt.analyses._
import at.forsyte.apalache.tla.bmcmt.smt.Z3SolverContext
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.lir._
import at.forsyte.apalache.tla.lir.convenience.tla
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfter, FunSuite}

@RunWith(classOf[JUnitRunner])
class TestTransitionExecutor extends FunSuite with BeforeAndAfter {
  private var typeFinder: TrivialTypeFinder = new TrivialTypeFinder()
  private var solver: Z3SolverContext = new Z3SolverContext()
  private var rewriter = new SymbStateRewriterImpl(solver, typeFinder, new ExprGradeStoreImpl())
  private var execCtx = new IncrementalExecutorContext(rewriter, typeFinder)

  before {
    // initialize the executor context
    typeFinder = new TrivialTypeFinder()
    solver = new Z3SolverContext()
    rewriter = new SymbStateRewriterImpl(solver, typeFinder, new ExprGradeStoreImpl())
    execCtx = new IncrementalExecutorContext(rewriter, typeFinder)
  }

  test("push 1 transition") {
    // y' <- 1 /\ x' <- 1
    val init = tla.and(mkAssign("y", 1), mkAssign("x", 1))
    val trex = new TransitionExecutor(Set.empty, Set("x", "y"), execCtx)
    trex.debug = true
    assert(trex.stepNo == 0)
    // init is a potential transition with index 3 (the index is defined by the input spec)
    trex.prepareTransition(3, init)
    // assume that one of the prepared transitions fires
    trex.pickTransition()
    // advance the computation: forget the non-primed variables, rename primed to non-primed
    trex.nextState()
    assert(trex.stepNo == 1)
    // assert something about the current state
    trex.assertState(tla.eql(tla.name("y"), tla.int(1)))
    assert(trex.sat(60).contains(true))
  }

  test("check enabled and discard") {
    // an obviously disabled transition: y' <- 1 /\ y' <- 2
    val init = tla.and(
      mkAssign("y", 1),
      tla.eql(tla.prime(tla.name("y")), tla.int(2)),
      mkAssign("x", 3))
    val trex = new TransitionExecutor(Set.empty, Set("x", "y"), execCtx)
    trex.debug = true
    assert(trex.stepNo == 0)
    // init is a potential transition with index 3 (the index is defined by the input spec)
    trex.prepareTransition(1, init)
    // check, whether the transition is enabled
    trex.assumeTransition(1)
    assert(trex.sat(60).contains(false))
  }

  test("check an invariant after transition") {
    // y' <- 1 /\ x' <- 1
    val init = tla.and(mkAssign("y", 1), mkAssign("x", 1))
    val trex = new TransitionExecutor(Set.empty, Set("x", "y"), execCtx)
    trex.debug = true
    assert(trex.stepNo == 0)
    // init is a potential transition with index 3 (the index is defined by the input spec)
    trex.prepareTransition(3, init)
    // assume that the transition has fired
    trex.assumeTransition(3)
    // create a snapshot for a later rollback
    val snapshot = trex.snapshot()
    // assert invariant violation and check it
    val notInv = tla.not(tla.eql(tla.prime(tla.name("y")), tla.prime(tla.name("x"))))
    trex.assertState(notInv)
    assert(trex.sat(60).contains(false))
    // rollback the snapshot
    trex.recover(snapshot)
    // now the context is satisfiable again
    assert(trex.sat(60).contains(true))
  }

  test("push 3 steps") {
    // x' <- 1 /\ y' <- 1
    val init = tla.and(mkAssign("y", 1), mkAssign("x", 1))
    // x' <- y /\ y' <- x + y
    val nextTrans = tla.and(
      mkAssign("y", tla.plus(tla.name("y"), tla.int(1))),
      mkAssign("x", tla.plus(tla.name("x"), tla.name("y"))))
    // 3 = x /\ 3 = y
    val inv = tla.ge(tla.name("y"), tla.name("x"))
    val trex = new TransitionExecutor(Set.empty, Set("x", "y"), execCtx)
    trex.prepareTransition(1, init)
    trex.pickTransition()
    trex.nextState()
    trex.prepareTransition(1, nextTrans)
    trex.pickTransition()
    trex.nextState()
    trex.prepareTransition(1, nextTrans)
    trex.pickTransition()
    trex.nextState()
    trex.prepareTransition(1, nextTrans)
    trex.pickTransition()
    trex.nextState()
  }

  private def mkAssign(name: String, value: Int) =
    tla.assignPrime(tla.name(name), tla.int(value))

  private def mkAssign(name: String, rhs: TlaEx) =
    tla.assignPrime(tla.name(name), rhs)
}
