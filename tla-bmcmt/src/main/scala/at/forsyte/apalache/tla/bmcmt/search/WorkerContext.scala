package at.forsyte.apalache.tla.bmcmt.search

import java.io._

import at.forsyte.apalache.tla.bmcmt.rules.aux.Oracle
import at.forsyte.apalache.tla.bmcmt.{SymbState, SymbStateDecoder, SymbStateRewriter, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.bmcmt.smt.RecordingZ3SolverContext
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.lir.io.UTFPrinter

/**
  * A context that maintains the search stack, rewriter, solver context, type finder, etc.
  * Most importantly, the context can be saved and restored. This is essential for the multi-core algorithm.
  *
  * @author Igor Konnov
  */
class WorkerContext(var rank: Int,
                    initNode: HyperNode,
                    var solver: RecordingZ3SolverContext,
                    var rewriter: SymbStateRewriter,
                    var typeFinder: TrivialTypeFinder) extends Serializable {
  /**
    * A local copy of the worker state.
    */
  var workerState: WorkerState = IdleState()

  /**
    * The position in the search tree that the worker is exploring.
    */
  var activeNode: HyperNode = initNode

  def stepNo: Int = activeNode.depth

  // TODO: when solver is removed from Arena, fix that
  def state: SymbState = activeNode.snapshot.get.state.updateArena(_.setSolver(solver))

  def makeSnapshot(state: SymbState, oracle: Oracle): SearchSnapshot = {
    val smtLog = solver.extractLog()
    val rewriterSnapshot = rewriter.snapshot()
    // TODO: when solver is removed from Arena, fix that
    val safeState = state.updateArena(_.setSolver(null))
    new SearchSnapshot(rewriterSnapshot, smtLog, safeState, oracle)
  }

  /**
    * Dispose the resources that are associated with the context
    */
  def dispose(): Unit = {
    rewriter.dispose()
  }

  def saveToFile(file: File): Unit = {
    val fos = new FileOutputStream(file, false)
    val oos = new ObjectOutputStream(fos)
    try {
      oos.writeObject(WorkerContext.getClass.getName)
      oos.writeObject(this)
      oos.flush()
      fos.flush()
    } finally {
      oos.close()
      fos.close()
    }
  }

  def dumpCounterexample(filename: String): Unit = {
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
    writer.println(s"FIXME in WorkerContext.dumpCounterexample")
    for (((state, oracle), i) <- states.zip(oracles).zipWithIndex) {
      val decoder = new SymbStateDecoder(solver, rewriter)
      val transition = oracle.evalPosition(solver, state)
      writer.println(s"State $i (from transition $transition):")
      writer.println("--------")
      val binding = decoder.decodeStateVariables(state)
      for (name <- binding.keys.toSeq.sorted) { // sort the keys
        writer.println("%-15s ->  %s".format(name, UTFPrinter.apply(binding(name))))
      }
      writer.println("========\n")
    }
    writer.close()
  }

}

object WorkerContext {
  def load(file: File, newRank: Int): WorkerContext = {
    val fis = new FileInputStream(file)
    val ois = new ObjectInputStream(fis)
    try {
      val className = ois.readObject().asInstanceOf[String]
      if (className != WorkerContext.getClass.getName) {
        throw new IOException("Corrupted serialized file: " + file)
      } else {
        val context = ois.readObject().asInstanceOf[WorkerContext]
        context.rank = newRank
        context
      }
    } finally {
      ois.close()
      fis.close()
    }
  }

  def recover(rank: Int,
              node: HyperNode,
              params: ModelCheckerParams,
              protoRewriter: SymbStateRewriterImpl): WorkerContext = {
    assert(node.snapshot.isDefined)
    val solver = RecordingZ3SolverContext(Some(node.snapshot.get.smtLog), params.debug, profile = false)
    val typeFinder = new TrivialTypeFinder()
    // XXX: the rewriter recovery is still a hack
    val rewriter = new SymbStateRewriterImpl(solver, typeFinder, protoRewriter.exprGradeStore)
    rewriter.freeExistentialsStore = protoRewriter.freeExistentialsStore
    rewriter.formulaHintsStore = protoRewriter.formulaHintsStore
    rewriter.config = protoRewriter.config
    rewriter.recover(node.snapshot.get.rewriterSnapshot)
    new WorkerContext(rank, node, solver, rewriter, typeFinder)
  }
}
