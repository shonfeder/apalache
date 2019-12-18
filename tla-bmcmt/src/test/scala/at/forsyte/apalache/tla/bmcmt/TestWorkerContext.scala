package at.forsyte.apalache.tla.bmcmt

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import at.forsyte.apalache.tla.bmcmt.analyses.ExprGradeStoreImpl
import at.forsyte.apalache.tla.bmcmt.search.{HyperTransition, HyperTree, WorkerContext}
import at.forsyte.apalache.tla.bmcmt.smt.RecordingZ3SolverContext
import at.forsyte.apalache.tla.bmcmt.types.IntT
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.lir.convenience.tla
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.SortedMap

@RunWith(classOf[JUnitRunner])
class TestWorkerContext extends FunSuite {

  // irrelevant as of now
  ignore("write and read") {
    val solver = RecordingZ3SolverContext(None, false, false)
    val typeFinder = new TrivialTypeFinder()
    val rewriter = new SymbStateRewriterImpl(solver, typeFinder, new ExprGradeStoreImpl)
    val workerContext = new WorkerContext(rank = 1, HyperTree(HyperTransition(0)), solver, rewriter, typeFinder)
    var arena = Arena.create(solver).appendCell(IntT())
    val x = arena.topCell
    var state = new SymbState(tla.eql(x.toNameEx, tla.int(42)), arena, Binding())
    state = rewriter.rewriteUntilDone(state.setRex(tla.eql(x.toNameEx, tla.int(42))))
    solver.assertGroundExpr(state.ex)
    assert(solver.sat())
    assert(solver.evalGroundExpr(x.toNameEx) == tla.int(42))

//    workerContext.push(state, new MockOracle(0), SortedMap())

    // save the object
    val arrayStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(arrayStream)
    oos.writeObject(workerContext)
    oos.close()
    // update the context
    solver.assertGroundExpr(tla.gt(x.toNameEx, tla.int(1000)))
    assert(!solver.sat())
    val inputStream = new ObjectInputStream(new ByteArrayInputStream(arrayStream.toByteArray))

    // read the object
    val restoredContext = inputStream.readObject().asInstanceOf[WorkerContext]
    // the restored context should be satisfiable
    assert(restoredContext.solver.sat())
    assert(restoredContext.solver.evalGroundExpr(x.toNameEx) == tla.int(42))
    state = restoredContext.state
    state = restoredContext.rewriter.rewriteUntilDone(state.setRex(tla.not(tla.eql(x.toNameEx, tla.int(42)))))
    restoredContext.solver.assertGroundExpr(state.ex)
    assert(!restoredContext.solver.sat())
  }
}
