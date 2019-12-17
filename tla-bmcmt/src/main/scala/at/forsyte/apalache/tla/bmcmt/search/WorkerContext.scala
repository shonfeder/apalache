package at.forsyte.apalache.tla.bmcmt.search

import java.io._

import at.forsyte.apalache.tla.bmcmt.rules.aux.Oracle
import at.forsyte.apalache.tla.bmcmt.smt.SolverContext
import at.forsyte.apalache.tla.bmcmt.types.{CellT, TypeFinder}
import at.forsyte.apalache.tla.bmcmt.{SymbState, SymbStateDecoder, SymbStateRewriter}
import at.forsyte.apalache.tla.lir.io.UTFPrinter

import scala.collection.immutable.SortedMap

/**
  * A context that maintains the search stack, rewriter, solver context, type finder, etc.
  * Most importantly, the context can be saved and restored. This is essential for the multi-core algorithm.
  *
  * @author Igor Konnov
  */
class WorkerContext(var rank: Int,
                    val typeFinder: TypeFinder[CellT],
                    val solver: SolverContext,
                    val rewriter: SymbStateRewriter,
                    initNode: HyperTree) extends Serializable {
  /**
    * The position in the search tree that the worker is exploring.
    */
  var activeNode: HyperTree = initNode

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
    * Dispose the resources that are associated with the context
    */
  def dispose(): Unit = {
    rewriter.dispose()
  }

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
    val writer = new PrintWriter(new FileWriter(filename, false))
    for (((state, oracle), i) <- stateStack.reverse.zip(oracleStack.reverse).zipWithIndex) {
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
}
