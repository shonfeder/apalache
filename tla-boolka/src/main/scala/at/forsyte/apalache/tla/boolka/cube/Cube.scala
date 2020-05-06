package at.forsyte.apalache.tla.boolka.cube

import scala.collection.BitSet

/**
  * <p>This class represents a Boolean cube, or a trivector, that is a vector v_0, ..., v_n, whose every element v_i
  * can have one of the three values: 0 (false), 1 (true), or * (false or true). We assume that the predicate indices
  * start with 0. Trivectors are used
  * in predicate abstraction, e.g., see the paper by Ball, Podelski, Rajamani, TACAS 2001:
  * Boolean and Cartesian Abstraction for Model Checking C Programs.</p>
  *
  * To keep things simple, we represent a trivector with two bitsets: `bits` and `mask`.
  * The tri-value `v_i` is determined as follows:
  *
  * {{{
  * if (mask.contains(i))
  *   if bits(i) 1 else 0
  * else 2
  * }}}
  *
  * @author Igor Konnov
  */
class Cube(val nbits: Int, val bits: BitSet, val mask: BitSet) {
  def setBits(f: BitSet => BitSet): Cube = {
    new Cube(nbits, f(bits), mask)
  }

  def setMask(f: BitSet => BitSet): Cube = {
    new Cube(nbits, bits, f(bits))
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[Cube]

  override def equals(other: Any): Boolean = other match {
    case that: Cube =>
      (that canEqual this) &&
        bits == that.bits &&
        mask == that.mask
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(bits, mask)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = {
    val buffer = new StringBuffer()
    for (i <- 0.until(nbits)) {
      if (mask(i)) {
        buffer.append(if (bits(i)) '1' else '0')
      } else {
        buffer.append('*')
      }
    }
    buffer.toString
  }
}

object Cube {
  def empty(n: Int) = new Cube(n, BitSet.empty, BitSet.empty)

  def apply(n: Int, bits: BitSet, mask: BitSet): Cube = {
    new Cube(n, bits, mask)
  }
}
