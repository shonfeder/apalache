package at.forsyte.apalache.tla.boolka.cube

import at.forsyte.apalache.tla.lir.NameEx
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.easymock.EasyMockSugar
import org.scalatest.junit.JUnitRunner

import scala.collection.immutable.BitSet

@RunWith(classOf[JUnitRunner])
class TestCubeBddZipper extends FunSuite with EasyMockSugar {
  test("test compaction of a Boolean formula") {
    val finderMock = mock[CubeFinder]
    expecting {
      finderMock.preds.andReturn(List(NameEx("p0"), NameEx("p1"), NameEx("p2")))
      // p0 /\ p1 /\ ~p2
      finderMock.next().andReturn(Some(Cube(3, BitSet(0, 1), BitSet(0, 1, 2))))
      // p1
      finderMock.next().andReturn(Some(Cube(3, BitSet(1), BitSet(1))))
      // None
      finderMock.next().andReturn(None)
    }

    val zipper = new CubeBddZipper(finderMock)
    whenExecuting(finderMock) {
      val cubes = zipper.allCubes()
      assert(List(Cube(3, BitSet(1), BitSet(1))) == cubes)
    }
  }
}
