package at.forsyte.apalache.tla.bmcmt

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import at.forsyte.apalache.tla.bmcmt.analyses.{ExprGradeStore, ExprGradeStoreImpl}
import at.forsyte.apalache.tla.bmcmt.search.ModelCheckerContext
import at.forsyte.apalache.tla.bmcmt.smt.RecordingZ3SolverContext
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.bmcmt.types.{IntT, TypeFinder}
import at.forsyte.apalache.tla.lir.convenience.tla
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TestModelCheckerContext extends FunSuite {
  test("write and read") {
    val solver = new RecordingZ3SolverContext(false, false)
    val typeFinder = new TrivialTypeFinder()
    val rewriter = new SymbStateRewriterImpl(solver, typeFinder, new ExprGradeStoreImpl)
    val mcContext = new ModelCheckerContext(typeFinder, solver, rewriter)
    var arena = Arena.create(solver).appendCell(IntT())
    val x = arena.topCell
    var state = new SymbState(tla.eql(x.toNameEx, tla.int(42)), CellTheory(), arena, Binding())
    state = rewriter.rewriteUntilDone(state.setRex(tla.eql(x.toNameEx, tla.int(42))))
    solver.assertGroundExpr(state.ex)
    assert(solver.sat())
    assert(solver.evalGroundExpr(x.toNameEx) == tla.int(42))

    // save the object
    val arrayStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(arrayStream)
    oos.writeObject(mcContext)
    oos.close()
    // update the context
    solver.assertGroundExpr(tla.gt(x.toNameEx, tla.int(1000)))
    assert(!solver.sat())
    val inputStream = new ObjectInputStream(new ByteArrayInputStream(arrayStream.toByteArray))

    // read the object
    val restoredMcContext = inputStream.readObject().asInstanceOf[ModelCheckerContext]
    // the restored context should be satisfiable
    assert(restoredMcContext.solver.sat())
    assert(restoredMcContext.solver.evalGroundExpr(x.toNameEx) == tla.int(42))
  }
}
