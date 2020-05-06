package at.forsyte.apalache.tla.boolka.cube
import at.forsyte.apalache.tla.lir.NameEx
import jdd.bdd.BDD

import scala.collection.immutable.BitSet

/**
  * <p>A simple BDD encoding of a set of cubes. I have written it quickly to test how it works.</p>
  *
  * <p>In the future, we should switch another BDD library: either add-lib or jsylvan.
  * I am not sure about how reference counting and garbage collection actually work in JDD.
  * The ref/deref interface looks a bit fragile in the presence of Java GC.</p>
  *
  * @param finder an underlying cube enumerator
  *
  * @author Igor Konnov
  */
class CubeBddZipper(finder: CubeFinder) extends CubeFinder {
  private var bddComp: Option[BddComp] = None

  private class BddComp(npreds: Int) {
    private val mgr = new BDD(10000, 1000) // initial node table and value cache
    private val bddVars = Map(0.until(npreds).map(_ -> mgr.createVar()) :_*)
    private var cubesBdd: Int = computeBaseCubes()

    def next(): Option[Cube] = {
      if (cubesBdd == mgr.getZero) {
        // no cubes left
        None
      } else {
        val oneSat = mgr.ref(mgr.oneSat(cubesBdd))
        val cube = getOneCube(oneSat)
        val notOneSat = mgr.ref(mgr.not(oneSat))
        val diff = mgr.ref(mgr.and(cubesBdd, notOneSat))
        mgr.deref(notOneSat)
        mgr.deref(oneSat)
        mgr.deref(cubesBdd)
        cubesBdd = diff
        Some(cube)
      }
    }

    private def getOneCube(bdd: Int): Cube = {
      val buf = mgr.oneSat(bdd, null)
      var bits = BitSet()
      var mask = BitSet()
      for ((v, i) <- buf.zipWithIndex) {
        if (v == 1) {
          bits += i
          mask += i
        } else if (v == 0) {
          mask += i
        } // else don't care
      }

      Cube(npreds, bits, mask)
    }

    private def computeBaseCubes(): Int = {
      var setBdd = mgr.ref(mgr.getZero) // initially, the BDD is empty

      var finished = false
      do {
        finder.next() match {
          case Some(c) =>
            // add one more cube
            val asBdd = cubeToBdd(c)
            setBdd = mgr.ref(mgr.or(setBdd, asBdd))
            mgr.deref(asBdd)

          case None =>
            finished = true
        }
      } while (!finished)

      setBdd
    }

    private def cubeToBdd(cube: Cube): Int = {
      var cubeBdd = mgr.ref(mgr.getOne) // initially, true

      for (i <- cube.mask.toList) {
        if (cube.bits(i)) {
          cubeBdd = mgr.ref(mgr.and(cubeBdd, bddVars(i)))
        } else {
          val notVar = mgr.ref(mgr.not(bddVars(i)))
          cubeBdd = mgr.ref(mgr.and(cubeBdd, notVar))
          mgr.deref(notVar)
        }
      }

      cubeBdd
    }
  }

  /**
    * The list of predicates over which the cube finder is running
    *
    * @return the list of name expressions that correspond to the predicates
    */
  override def preds: List[NameEx] = finder.preds

  /**
    * Find a cube, if it exists.
    *
    * @return a cube, if it exists; None otherwise
    */
  override def next(): Option[Cube] = {
    // the first call to next activates the zipper that immediately calls the underlying finder
    bddComp match {
      case None =>
        val bc = new BddComp(preds.length)
        bddComp = Some(bc)
        bc.next()

      case Some(bc) =>
        bc.next()
    }
  }
}
