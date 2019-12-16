package at.forsyte.apalache.tla.bmcmt

import at.forsyte.apalache.tla.bmcmt.smt.SolverContext
import at.forsyte.apalache.tla.bmcmt.types.{BoolT, IntT}
import at.forsyte.apalache.tla.lir.oper.{TlaBoolOper, TlaOper}
import at.forsyte.apalache.tla.lir.{NameEx, OperEx}

/**
  * Coercion from one theory to another. The coercion results are cached, and thus
  * this class supports StackableContext.
  *
  * As we are assigning sorts to cells now, this class is here just for backward compatibility with the code.
  * It will be removed in the future.
  *
  * @author Igor Konnov
  */
// TODO: remove, not needed anymore
class CoercionWithCache(val stateRewriter: SymbStateRewriter) extends StackableContext with Serializable {
  type SourceT = (String, Theory)
  type TargetT = String

  /**
    * A context level, see StackableContext
    */
  private var level: Int = 0

  // cache the integer constants that are introduced in SMT for integer literals
  private var cache: Map[SourceT, (TargetT, Int)] = Map()


  def coerce(state: SymbState, targetTheory: Theory): SymbState = {
    state
  }

  /**
    * Save the current context and push it on the stack for a later recovery with pop.
    */
  override def push(): Unit = {
    level += 1
  }

  /**
    * Pop the previously saved context. Importantly, pop may be called multiple times and thus it is not sufficient
    * to save only the latest context.
    */
  override def pop(): Unit = {
    assert(level > 0)

    def isEntryOlder(mapEntry: (SourceT, (TargetT, Int))): Boolean =
      mapEntry._2._2 < level

    cache = cache filter isEntryOlder
    level -= 1
  }

  /**
    * Pop the context as many times as needed to reach a given level.
    *
    * @param n the number of times to call pop
    */
  override def pop(n: Int): Unit = {
    for (_ <- 1.to(n)) {
      pop()
    }
  }

  /**
    * Clean the context.
    */
  override def dispose(): Unit = {
    cache = Map()
    level = 0
  }
}
