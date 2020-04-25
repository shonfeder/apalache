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

  // TODO: when solver is removed from Arena, fix that
//  def state: SymbState = activeNode.snapshot.get.state.updateArena(_.setSolver(solver))

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

  /*
  def dumpCounterexample(filename: String, notInv: TlaEx): Unit = {
    def findStates: Option[HyperNode] => List[SymbState] = {
      // TODO: when solver is removed from Arena, fix that
      case Some(tree) => tree.snapshot.get.state.updateArena(_.setSolver(solver)) :: findStates(tree.parent)
      case None => List()
    }

    def findOracles: Option[HyperNode] => List[Oracle] = {
      case Some(tree) => tree.snapshot.get.oracle :: findOracles(tree.parent)
      case None => List()
    }

    val states = findStates(Some(activeNode)).reverse
    val oracles = findOracles(Some(activeNode)).reverse

    val writer = new PrintWriter(new FileWriter(filename, false))

    def printState(heading: String, state: SymbState, stateNo: Int, oracle: Oracle): Unit = {
      val decoder = new SymbStateDecoder(solver, rewriter)
      val transition = oracle.evalPosition(solver, state)
      writer.println(s"""\\* Transition $transition: State $stateNo to State ${stateNo+1}:""")
      writer.println()
      val binding = decoder.decodeStateVariables(state)
      // construct a record and print it with PrettyWriter
      val body =
        if (binding.isEmpty) {
          ValEx(TlaBool(true))
        } else {
          val keyVals = binding.toList.sortBy(_._1)
            .map(p => OperEx(TlaOper.eq, NameEx(p._1), p._2))
          OperEx(TlaBoolOper.and, keyVals :_*)
        }

      new PrettyWriter(writer).write(TlaOperDecl(heading, List(), body))
      writer.println("")
      writer.println("")
    }

    writer.println("%s MODULE Counterexample %s\n".format("-" * 25, "-" * 25))

    // the first state is initializing the constants
    printState(s"ConstInit", states.head, 0, oracles.head)

    // the other states are the computed by Init and then by multiple applications of Next
    for (((state, oracle), i) <- states.tail.zip(oracles).zipWithIndex) {
      printState(s"State${i+1}", state, i, oracle)
    }

    // print the violated invariant
    writer.println(s"(* The following formula holds true in State${states.length - 1} and violates the invariant *)")
    new PrettyWriter(writer).write(TlaOperDecl("InvariantViolation", List(), notInv))

    writer.println("\n%s".format("=" * 80))
    writer.println("\\* Created %s by Apalache".format(Calendar.getInstance().getTime))
    writer.println("\\* https://github.com/konnov/apalache")
    writer.close()
  }
  */
}

object WorkerContext {
  type TrexT = TransitionExecutor[OfflineExecutorContext]
  type SnapshotT = ExecutorSnapshot[OfflineSnapshot]
}
