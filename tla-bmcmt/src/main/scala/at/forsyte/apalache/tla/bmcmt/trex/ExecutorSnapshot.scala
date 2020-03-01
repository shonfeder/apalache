package at.forsyte.apalache.tla.bmcmt.trex

import at.forsyte.apalache.tla.bmcmt.SymbState

class ExecutorSnapshot[ExecCtxT](val controlState: ExecutorControlState,
                                 val stack: List[SymbState],
                                 val preparedTransitions: Map[Int, SymbState],
                                 val ctxSnapshot: ExecCtxT)
