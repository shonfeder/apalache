package at.forsyte.apalache.tla.bmcmt.trex

import at.forsyte.apalache.tla.bmcmt.SymbStateRewriter
import at.forsyte.apalache.tla.bmcmt.rewriter.Recoverable
import at.forsyte.apalache.tla.bmcmt.smt.SolverContext
import at.forsyte.apalache.tla.bmcmt.types.{CellT, TypeFinder}

/**
  * A context that is used by TransitionExecutor. By default, a context is not thread-safe,
  * that is, you should have one context per thread.
  * IncrementalExecutorContext is the simplest implementation.
  *
  * @author Igor Konnov
  */
trait ExecutorContext[SnapshotT] extends Recoverable[SnapshotT] {
  type SnapT = SnapshotT

  val rewriter: SymbStateRewriter
  val typeFinder: TypeFinder[CellT]

  def solver: SolverContext = rewriter.solverContext
}
