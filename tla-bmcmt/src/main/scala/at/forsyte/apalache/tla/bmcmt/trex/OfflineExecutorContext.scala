package at.forsyte.apalache.tla.bmcmt.trex

import at.forsyte.apalache.tla.bmcmt.{SymbStateRewriter, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.bmcmt.smt.RecordingZ3SolverContext

/**
  * An executor context for an offline SMT solver.
  *
  * @param rewriter an expression rewriter
  */
class OfflineExecutorContext(var rewriter: SymbStateRewriter)
    extends ExecutorContext[OfflineSnapshot] {

  /**
    * Create a snapshot of the context. This method is non-destructive, that is,
    * the context may be used after a snapshot has been made.
    *
    * @return a snapshot
    */
  override def snapshot(): OfflineSnapshot = {
    val rs = rewriter.snapshot()
    val smtLog = rewriter.solverContext.asInstanceOf[RecordingZ3SolverContext].extractLog()
    new OfflineSnapshot(rs, smtLog, typeFinder.varTypes)
  }

  /**
    * <p>Recover the context from a snapshot that was created earlier.
    * It is up to the implementation to require, whether the snapshot should be created
    * within the same context.</p>
    *
    * <p>This method recovers the snapshot in place, so the context gets overwritten
    * with the snapshot contents. Note that a call recover(A) renders useless the
    * snapshots that were created in the time frame between A = snapshot()
    * and recover(A).</p>
    *
    * @param snapshot a snapshot
    * @throws IllegalStateException when recovery is impossible
    */
  override def recover(snapshot: OfflineSnapshot): Unit = {
    val solver = RecordingZ3SolverContext(Some(snapshot.smtLog), debug = false, profile = false)
    // XXX: the rewriter recovery is still a hack, see the issue #105
    val newRewriter = new SymbStateRewriterImpl(solver, typeFinder, rewriter.exprGradeStore)
    newRewriter.formulaHintsStore = rewriter.formulaHintsStore
    newRewriter.config = rewriter.config
    newRewriter.recover(snapshot.rewriterSnapshot)
    newRewriter.solverContext = solver
    newRewriter.typeFinder.reset(snapshot.varTypes)
    rewriter = newRewriter
  }
}
