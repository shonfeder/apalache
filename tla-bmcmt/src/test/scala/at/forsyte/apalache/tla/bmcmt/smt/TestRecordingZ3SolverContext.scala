package at.forsyte.apalache.tla.bmcmt.smt

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import at.forsyte.apalache.tla.bmcmt.Arena
import at.forsyte.apalache.tla.bmcmt.types.IntT
import at.forsyte.apalache.tla.lir.convenience.tla
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TestRecordingZ3SolverContext extends FunSuite {
  test("operations proxied") {
    val solver = RecordingZ3SolverContext(None, false, false)
    var arena = Arena.create(solver).appendCell(IntT())
    val x = arena.topCell
    solver.assertGroundExpr(tla.eql(x.toNameEx, tla.int(42)))
    assert(solver.sat())
    assert(solver.evalGroundExpr(x.toNameEx) == tla.int(42))
  }

  test("write and read") {
    val solver = RecordingZ3SolverContext(None, false, false)
    var arena = Arena.create(solver).appendCell(IntT())
    val x = arena.topCell
    solver.assertGroundExpr(tla.eql(x.toNameEx, tla.int(42)))
    assert(solver.sat())
    assert(solver.evalGroundExpr(x.toNameEx) == tla.int(42))
    // save the log
    val log = solver.extractLog()
    // update the context
    solver.assertGroundExpr(tla.gt(x.toNameEx, tla.int(1000)))
    assert(!solver.sat())
    // restore the context
    val restoredSolver = RecordingZ3SolverContext(Some(log), false, false)
    // the restored context should be satisfiable
    assert(restoredSolver.sat())
    assert(restoredSolver.evalGroundExpr(x.toNameEx) == tla.int(42))
  }
}
