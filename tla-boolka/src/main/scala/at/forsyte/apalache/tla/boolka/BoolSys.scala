package at.forsyte.apalache.tla.boolka

/**
  * A Boolean transition system that is produced by predicate abstraction. It is geared toward an encoding with NuSMV.
  *
  * @author Igor Konnov
  */
class BoolSys(val npreds: Int) {
  var init: List[Cube] = List.empty

  var next: List[Cube] = List.empty

  var notInv: List[Cube] = List.empty
}
