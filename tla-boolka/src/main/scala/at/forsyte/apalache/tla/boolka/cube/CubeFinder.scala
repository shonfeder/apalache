package at.forsyte.apalache.tla.boolka.cube

import at.forsyte.apalache.tla.lir.NameEx

trait CubeFinder {
  /**
    * The list of predicates over which the cube finder is running
    * @return the list of name expressions that correspond to the predicates
    */
  def preds: List[NameEx]

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
  def next(): Option[Cube]
}
