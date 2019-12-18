package at.forsyte.apalache.tla.bmcmt.search

/**
  * The state that is shared among the workers. Always use sharedState.synchronized { ... } to access the fields
  * or update them.
  *
  * @author Igor Konnov
  */
class SharedSearchState {
  /**
    * The workers' states, from the ranks to WorkerState.
    */
  var workerStates: Map[Int, WorkerState] = Map()

  /**
   * The hyper-tree that is constructed during the search.
   */
  var searchRoot: HyperNode = HyperNode(HyperTransition(0))
}
