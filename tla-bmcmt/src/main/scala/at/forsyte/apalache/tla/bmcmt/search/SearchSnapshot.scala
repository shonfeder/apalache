package at.forsyte.apalache.tla.bmcmt.search

import at.forsyte.apalache.tla.bmcmt.SymbState
import at.forsyte.apalache.tla.bmcmt.rewriter.SymbStateRewriterSnapshot
import at.forsyte.apalache.tla.bmcmt.rules.aux.Oracle
import at.forsyte.apalache.tla.bmcmt.smt.SmtLog

/**
  * A snapshot made by a worker after exploring a tree node.
  *
  * @param rewriterSnapshot a snapshot that can be used to recover the rewriter state at the node
  * @param smtLog an SMT log that can be replayed to recover the SMT context
  * @param state the symbolic state at the node
  * @param oracle the oracle that stores the transition that led to the state
  *
  * @author Igor Konnov
  */
@deprecated("Use ExecutorSnapshot instead")
class SearchSnapshot(val rewriterSnapshot: SymbStateRewriterSnapshot,
                     val smtLog: SmtLog,
                     val state: SymbState,
                     val oracle: Oracle) {
}
