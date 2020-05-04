package at.forsyte.apalache.tla.boolka

import at.forsyte.apalache.tla.bmcmt.smt.SolverContext
import at.forsyte.apalache.tla.lir.values.TlaBool
import at.forsyte.apalache.tla.lir.{NameEx, ValEx}
import at.forsyte.apalache.tla.lir.convenience.tla

import scala.collection.immutable.BitSet

/**
  * A cube enumerator. This is the simplest enumerator using an incremental SMT context.
  *
  * TODO: this enumeration technique is quite primitive. Use the more advanced techniques that integrated with BDDs.
  *
  * @param solver a solver instance
  * @param preds the names of the predicates
  */
class CubeFinder(solver: SolverContext, val preds: List[NameEx]) {
  /**
    * Enumerate all cubes within the current context. This method updates the SMT context in a way that the second call
    * to `allCubes` would return an empty list. Hence, the user has to save the SMT context and pop it after
    * a call `allCubes`.
    *
    * @return the list of cubes
    */
  def allCubes(): List[Cube] = {
    var cubes = List[Cube]()
    var finished = false
    do {
      next() match {
        case Some(c) => cubes = cubes :+ c
        case None => finished = true
      }
    } while (!finished)

    cubes
  }

  /**
    * Find a cube, if it exists.
    *
    * @return a cube, if it exists; None otherwise
    */
  def next(): Option[Cube] = {
    if (!solver.sat()) {
      None
    } else {
      var bits = BitSet()
      // collect the cube l_1 /\ ... /\ l_n from the SMT model
      for ((p, no) <- preds.zipWithIndex) {
        val value = solver.evalGroundExpr(p) == ValEx(TlaBool(true))
        bits = if (value) bits + no else bits
      }
      // add the cube negation for the next iteration: ~l_1 \/ ... \/ ~l_n
      def mkNotLit(predAndNo: (NameEx, Int)) = if (bits(predAndNo._2)) tla.not(predAndNo._1) else predAndNo._1
      solver.assertGroundExpr(tla.or(preds.zipWithIndex.map(mkNotLit) :_*))
      val fullMask = BitSet(preds.indices :_*)
      Some(Cube(preds.length, bits, fullMask))
    }
  }
}
