package at.forsyte.apalache.tla.boolka

import at.forsyte.apalache.tla.bmcmt.{Arena, ArenaCell}
import at.forsyte.apalache.tla.bmcmt.smt.{SolverContext, Z3SolverContext}
import at.forsyte.apalache.tla.bmcmt.trex.IncrementalExecutorContext
import at.forsyte.apalache.tla.bmcmt.types.{BoolT, CellT, IntT}
import at.forsyte.apalache.tla.lir.convenience.tla
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Outcome, fixture}

import scala.collection.immutable.BitSet

@RunWith(classOf[JUnitRunner])
class TestCubeFinder extends fixture.FunSuite {
  type ExecutorContextT = IncrementalExecutorContext
  type FixtureParam = SolverContext

  override protected def withFixture(test: OneArgTest): Outcome = {
    val solver = new Z3SolverContext()
    test(solver)
  }

  test("test abstraction of a Boolean formula") { solver: SolverContext =>
    var arena = Arena.create(solver) // create a default arena to get a few constants

    def mkBool(): ArenaCell = {
      arena = arena.appendCell(BoolT())
      arena.topCell
    }

    val p0 = mkBool()
    val p1 = mkBool()
    val p2 = mkBool()
    // the Boolean formula we like to abstract
    solver.assertGroundExpr(
      tla.or(
        tla.and(p0.toNameEx, tla.not(p1.toNameEx), p2.toNameEx),
        tla.and(tla.not(p0.toNameEx), p1.toNameEx)
      ))
    // abstract it using p0 and p2
    val finder = new CubeFinder(solver, List(p0, p2).map(_.toNameEx))
    val cubes = finder.allCubes()
    // the current naive implementation only computes minterms, so we will have 6 of them
    assert(3 == cubes.size)
    val cubeSet = cubes.toSet
    assert(cubeSet.contains(new Cube(2, BitSet(0, 1), BitSet(0, 1))))
    assert(cubeSet.contains(new Cube(2, BitSet(), BitSet(0, 1))))
    assert(cubeSet.contains(new Cube(2, BitSet(1), BitSet(0, 1))))
  }

  test("test abstraction of integer constraints") { solver: SolverContext =>
    var arena = Arena.create(solver) // create a default arena to get a few constants

    def mkCell(t: CellT): ArenaCell = {
      arena = arena.appendCell(t)
      arena.topCell
    }

    val x = mkCell(IntT())
    val y = mkCell(IntT())
    val z = mkCell(IntT())
    val p0 = mkCell(BoolT())
    val p1 = mkCell(BoolT())
    // the Boolean formula we like to abstract: x > y /\ y = z + 1
    solver.assertGroundExpr(
      tla.and(
        tla.gt(x.toNameEx, y.toNameEx),
        tla.eql(y.toNameEx, tla.plus(z.toNameEx, tla.int(1)))
      )) ///
    // add the predicates: p0 == (x > z), p1 == (y = 2 * x - 1)
    solver.assertGroundExpr(tla.eql(p0.toNameEx, tla.gt(x.toNameEx, z.toNameEx)))
    solver.assertGroundExpr(tla.eql(p1.toNameEx,
      tla.eql(y.toNameEx,
        tla.plus(tla.mult(tla.int(2), x.toNameEx), tla.int(1)))))
    // abstract it using p0 and p1
    val finder = new CubeFinder(solver, List(p0, p1).map(_.toNameEx))
    val cubes = finder.allCubes()
    assert(2 == cubes.size)
    val cubeSet = cubes.toSet
    assert(cubeSet.contains(new Cube(2, BitSet(0, 1), BitSet(0, 1))))
    assert(cubeSet.contains(new Cube(2, BitSet(0), BitSet(0, 1))))
  }
}
