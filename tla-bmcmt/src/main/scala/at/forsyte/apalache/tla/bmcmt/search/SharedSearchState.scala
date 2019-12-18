package at.forsyte.apalache.tla.bmcmt.search

import at.forsyte.apalache.tla.lir.TlaEx

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
    * The status of the transitions that have to explored
    */
  var transitions: Map[Int, (TlaEx, TransitionStatus)] = Map()

  /**
   * The hyper-tree that is constructed during the search.
   */
  var searchTree: HyperTree = HyperTree(HyperTransition(0))

  /**
    * The node that is currently explored by the explorer threads
    */
  var activeNode: HyperTree = searchTree

  /**
    * Extend the active node with a child and set the child to be active.
    * @param ht a hyper-transition that corresponds to the active node
    * @return a new active node
    */
  def extendActiveNode(ht: HyperTransition): HyperTree = {
    val newNode = HyperTree(ht)
    activeNode.append(newNode)
    activeNode = newNode
    activeNode
  }
}
