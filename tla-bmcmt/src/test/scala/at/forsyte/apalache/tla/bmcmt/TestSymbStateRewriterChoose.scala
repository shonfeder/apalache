package at.forsyte.apalache.tla.bmcmt

import at.forsyte.apalache.tla.lir.TestingPredefs
import at.forsyte.apalache.tla.lir.convenience.tla
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSymbStateRewriterChoose extends RewriterBase with TestingPredefs {
  test("""SE-CHOOSE: CHOOSE x \in {1, 2, 3}: x > 1 ~~> $B$k""") {
    val ex = tla.choose(tla.name("x"),
      tla.enumSet(tla.int(1), tla.int(2), tla.int(3)),
      tla.gt(tla.name("x"), tla.int(1)))
    val state = new SymbState(ex, CellTheory(), arena, new Binding)
    val rewriter = create()
    val nextState = rewriter.rewriteUntilDone(state)
    assert(solverContext.sat())
    def assertEq(i: Int): SymbState = {
      val ns = rewriter.rewriteUntilDone(nextState.setRex(tla.eql(nextState.ex, tla.int(i))))
      solverContext.assertGroundExpr(ns.ex)
      ns
    }

    rewriter.push()
    assertEq(3)
    assert(solverContext.sat())
    rewriter.pop()
    rewriter.push()
    assertEq(2)
    assert(solverContext.sat())
    rewriter.pop()
    rewriter.push()
    val ns = assertEq(1)
    assertUnsatOrExplain(rewriter, ns)
  }
}