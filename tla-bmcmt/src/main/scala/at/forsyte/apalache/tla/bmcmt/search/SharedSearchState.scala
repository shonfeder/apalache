package at.forsyte.apalache.tla.bmcmt.search

import java.util.concurrent.CyclicBarrier

/**
  * The state that is shared among the workers. Always use sharedState.synchronized { ... } to access the fields
  * or update them.
  *
  * @author Igor Konnov
  */
class SharedSearchState(nworkers: Int) {
  /**
    * The workers' states, from the ranks to WorkerState.
    */
  var workerStates: Map[Int, WorkerState] = Map()

  /**
   * The hyper-tree that is constructed during the search.
   */
  var searchRoot: HyperNode = HyperNode(HyperTransition(0))

  /**
    * A cyclic barrier that is used by the workers to initialize the search.
    */
  var barrier: CyclicBarrier = new CyclicBarrier(nworkers)
}
