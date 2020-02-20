package at.forsyte.apalache.tla.bmcmt.search

import java.io._
import java.util.Calendar

import at.forsyte.apalache.tla.bmcmt.rules.aux.Oracle
import at.forsyte.apalache.tla.bmcmt.smt.RecordingZ3SolverContext
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.bmcmt.{SymbState, SymbStateDecoder, SymbStateRewriter, SymbStateRewriterImpl}
import at.forsyte.apalache.tla.lir.io.PrettyWriter
import at.forsyte.apalache.tla.lir.oper.{TlaBoolOper, TlaFunOper, TlaOper}
import at.forsyte.apalache.tla.lir.values.{TlaBool, TlaStr}
import at.forsyte.apalache.tla.lir._

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

  /**
    * Get the step number.
    * @return the step number
    */
  def stepNo: Int = {
    // the constants are initialized at depth 0 and the initial states are computed at step 0: from depth 0 to depth 1
    activeNode.depth
  }

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
    recover(rank, node, node.snapshot.get, params, protoRewriter)
  }

  def recover(rank: Int,
              node: HyperNode,
              snapshot: SearchSnapshot,
              params: ModelCheckerParams,
              protoRewriter: SymbStateRewriterImpl): WorkerContext = {
    val solver = RecordingZ3SolverContext(Some(snapshot.smtLog), params.debug, profile = false)
    val typeFinder = new TrivialTypeFinder()
    // XXX: the rewriter recovery is still a hack
    val rewriter = new SymbStateRewriterImpl(solver, typeFinder, protoRewriter.exprGradeStore)
    rewriter.formulaHintsStore = protoRewriter.formulaHintsStore
    rewriter.config = protoRewriter.config
    rewriter.recover(snapshot.rewriterSnapshot)
    new WorkerContext(rank, node, solver, rewriter, typeFinder)
  }
}
