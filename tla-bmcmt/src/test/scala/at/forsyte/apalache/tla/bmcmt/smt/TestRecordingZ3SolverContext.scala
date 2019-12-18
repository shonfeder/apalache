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
    // save the object
    val arrayStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(arrayStream)
    oos.writeObject(solver)
    oos.close()
    // update the context
    solver.assertGroundExpr(tla.gt(x.toNameEx, tla.int(1000)))
    assert(!solver.sat())
    val inputStream = new ObjectInputStream(new ByteArrayInputStream(arrayStream.toByteArray))
    // read the object
    val restoredContext = inputStream.readObject().asInstanceOf[RecordingZ3SolverContext]
    // the restored context should be satisfiable
    assert(restoredContext.sat())
    assert(restoredContext.evalGroundExpr(x.toNameEx) == tla.int(42))
  }
}
