package at.forsyte.apalache.tla.boolka.io

import java.io.{PrintWriter, StringWriter}

import at.forsyte.apalache.tla.boolka.{BoolSys, Cube}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Outcome, fixture}

import scala.collection.immutable.BitSet

@RunWith(classOf[JUnitRunner])
class TestBoolSysSmvWriter extends fixture.FunSuite {
  type FixtureParam = (StringWriter, PrintWriter)

  override protected def withFixture(test: OneArgTest): Outcome = {
    val stringWriter = new StringWriter()
    val printWriter = new PrintWriter(stringWriter)
    test((stringWriter, printWriter))
  }

  test("test empty") { param =>
    val (stringWriter, printWriter) = param
    val writer = new BoolSysSmvWriter(printWriter, 80)
    val sys = new BoolSys(2)
    writer.write(sys)
    printWriter.flush()
    val expected =
      """MODULE main
        |VAR
        |  p0: boolean;
        |  p1: boolean;
        |INIT
        |  (TRUE)
        |TRANS
        |  (TRUE)
        |SPEC AG !(TRUE)""".stripMargin
    assert(expected == stringWriter.toString)
  }

  test("test INIT") { param =>
    val (stringWriter, printWriter) = param
    val writer = new BoolSysSmvWriter(printWriter, 80)
    val sys = new BoolSys(2)
    sys.init = List(Cube(2, BitSet(0), BitSet(0, 1)), Cube(2, BitSet(1), BitSet(0, 1)))
    writer.write(sys)
    printWriter.flush()
    val expected =
      """MODULE main
        |VAR
        |  p0: boolean;
        |  p1: boolean;
        |INIT
        |  ((p0 & !p1) | (!p0 & p1))
        |TRANS
        |  (TRUE)
        |SPEC AG !(TRUE)""".stripMargin
    assert(expected == stringWriter.toString)
  }

  test("test TRANS") { param =>
    val (stringWriter, printWriter) = param
    val writer = new BoolSysSmvWriter(printWriter, 80)
    val sys = new BoolSys(2)
    sys.next = List(Cube(4, BitSet(0, 2), BitSet(0, 1, 2, 3)), Cube(4, BitSet(1, 3), BitSet(0, 1, 2, 3)))
    writer.write(sys)
    printWriter.flush()
    val expected =
      """MODULE main
        |VAR
        |  p0: boolean;
        |  p1: boolean;
        |INIT
        |  (TRUE)
        |TRANS
        |  ((p0 & !p1 & next(p0) & !next(p1)) | (!p0 & p1 & !next(p0) & next(p1)))
        |SPEC AG !(TRUE)""".stripMargin
    assert(expected == stringWriter.toString)
  }

  test("test invariant") { param =>
    val (stringWriter, printWriter) = param
    val writer = new BoolSysSmvWriter(printWriter, 80)
    val sys = new BoolSys(2)
    sys.notInv = List(Cube(2, BitSet(0), BitSet(0, 1)), Cube(2, BitSet(1), BitSet(0, 1)))
    writer.write(sys)
    printWriter.flush()
    val expected =
      """MODULE main
        |VAR
        |  p0: boolean;
        |  p1: boolean;
        |INIT
        |  (TRUE)
        |TRANS
        |  (TRUE)
        |SPEC AG !((p0 & !p1) | (!p0 & p1))""".stripMargin
    assert(expected == stringWriter.toString)
  }
}
