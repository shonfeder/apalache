package at.forsyte.apalache.tla.bmcmt.search

import java.io._

import at.forsyte.apalache.tla.bmcmt.trex.{ExecutorSnapshot, OfflineExecutorContext, OfflineSnapshot, TransitionExecutor}
import at.forsyte.apalache.tla.lir._
import at.forsyte.apalache.tla.lir.io.CounterexampleWriter
import com.typesafe.scalalogging.LazyLogging

/**
  * A context that maintains the search stack, rewriter, solver context, type finder, etc.
  * Most importantly, the context can be saved and restored. This is essential for the multi-core algorithm.
  * The snapshot/recovery functions were extracted into ExecutorContext.
  *
  * @author Igor Konnov
  */
class WorkerContext(var rank: Int,
                    initNode: HyperNode,
                    val trex: TransitionExecutor[OfflineSnapshot]) extends Serializable with LazyLogging {
  /**
    * A local copy of the worker state.
    */
  var workerState: WorkerState = IdleState()

  /**
    * The position in the search tree that the worker is exploring.
    */
  var activeNode: HyperNode = initNode

  /**
    * Get the step number.
    *
    * @return the step number
    */
  def stepNo: Int = trex.stepNo

  /**
    * Produce a snapshot of the context.
    *
    * @return a snapshot produced by TransitionExecutor
    */
  def saveSnapshotToActiveNode(): Unit = {
    assert(activeNode.snapshot.isEmpty)
    activeNode.snapshot = Some(trex.snapshot())
  }

  /**
    * Recover from a snapshot
    * @param node a node to synchronize with
    */
  def recoverFromNodeAndActivate(node: HyperNode): Unit = {
    logger.debug(s"Worker $rank is synchronizing with tree node ${node.id}")
    trex.recover(node.snapshot.get)
    this.activeNode = node
    //    context.solver.log(";;;;;;;;;;;;; RECOVERY FROM node %d".format(context.activeNode.id))
    logger.debug(s"Worker $rank synchronized")
  }



  /**
    * Dispose the resources that are associated with the context
    */
  def dispose(): Unit = {
    trex.dispose()
  }

  def dumpCounterexample(rootModule: TlaModule, notInv: TlaEx): List[String] = {
    val exec = trex.decodedExecution()
    val states = exec.path.map(p => (p._2.toString, p._1))
    // TODO: add a prefix to the filename to avoid overwriting the output by other nodes
    CounterexampleWriter.writeAllFormats(rootModule, notInv, states)
  }
}

object WorkerContext {
  type TrexT = TransitionExecutor[OfflineExecutorContext]
  type SnapshotT = ExecutorSnapshot[OfflineSnapshot]
}
