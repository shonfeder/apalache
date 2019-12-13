package at.forsyte.apalache.tla.bmcmt.search

import at.forsyte.apalache.tla.bmcmt.SymbState
import at.forsyte.apalache.tla.bmcmt.rules.aux.Oracle
import at.forsyte.apalache.tla.bmcmt.types.CellT

import scala.collection.immutable.SortedMap

/**
  * A context that maintains the search stack, rewriter, solver context, type finder, etc.
  * Most importantly, the context can be saved and restored. This is essential for the multi-core algorithm.
  *
  * @author Igor Konnov
  */
class ModelCheckerContext {
  /**
    * A hyperpath that is collected during the search.
    */
  var runningPath: HyperPath = Seq()

  /**
    * A stack of the symbolic states that might constitute a counterexample (the last state is on top).
    */
  var stateStack: List[SymbState] = List()

  /**
    * A stack of the oracles that define which transitions should be fired.
    */
  var oracleStack: List[Oracle] = List()

  /**
    * A stack of types that is computed for each variable at every step.
    */
  var typesStack: Seq[SortedMap[String, CellT]] = List()

  /**
    * Get the step number
    * @return the step number
    */
  def stepNo: Int = stateStack.size - 1

  /**
    * Get the state on top of the stack
    * @return the state on top of the stack, if it exists; exception otherwise.
    */
  def state: SymbState = stateStack.head

  /**
    * Get the types on top of the stack
    * @return the types on top of the stack, if it exists; exception otherwise.
    */
  def types: SortedMap[String, CellT] = typesStack.head

  /**
    * Push a new level in the context.
    * @param state a new symbolic state
    * @param oracle an oracle that defines which transition reaches the state
    * @param types the types associated with the state
    */
  def push(state: SymbState, oracle: Oracle, types: SortedMap[String, CellT]): Unit = {
    stateStack +:= state
    oracleStack +:= oracle
    typesStack +:= types
  }

}
