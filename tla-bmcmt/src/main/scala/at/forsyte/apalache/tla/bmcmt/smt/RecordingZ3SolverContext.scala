package at.forsyte.apalache.tla.bmcmt.smt

import java.io.{ObjectInputStream, ObjectOutputStream}

import at.forsyte.apalache.tla.bmcmt.profiler.SmtListener
import at.forsyte.apalache.tla.bmcmt.{Arena, ArenaCell}
import at.forsyte.apalache.tla.lir.TlaEx


object RecordingZ3SolverContext {
  /**
    * A record in the solver log
    */
  sealed abstract class Record extends Serializable

  case class DeclareCellRecord(cell: ArenaCell) extends Record with Serializable
  case class DeclareInPredRecord(set: ArenaCell, elem: ArenaCell) extends Record with Serializable
  case class IntroIntConstRecord() extends Record with Serializable
  case class AssertGroundExprRecord(ex: TlaEx) extends Record with Serializable
}

@SerialVersionUID(700L)
class RecordingZ3SolverContext(debug: Boolean, profile: Boolean) extends SolverContext with Serializable {
  import RecordingZ3SolverContext._

  private var solver = new Z3SolverContext(debug, profile)

  /**
    * The sequence of logs, one per context, except the last log, which is maintained in lastLog.
    */
  private var logStack: List[List[Record]] = List()

  /**
    * The current log.
    */
  private var lastLog: List[Record] = List()

  /**
    * Deserialize the context log by smashing SMT contexts. Defining custom serialization for Serializable.
    * @param ois object input stream
    */
  private def readObject(ois: ObjectInputStream): Unit = {
    val totalSize = ois.readObject().asInstanceOf[Int]
    lastLog = List[Record]()
    solver = new Z3SolverContext(debug, profile)
    for (_ <- 1.to(totalSize)) {
      val record = ois.readObject().asInstanceOf[Record]
      lastLog = lastLog :+ record

      record match {
        case DeclareCellRecord(cell) => solver.declareCell(cell)
        case DeclareInPredRecord(set, elem) => solver.declareInPredIfNeeded(set, elem)
        case IntroIntConstRecord() => solver.introIntConst()
        case AssertGroundExprRecord(ex) => solver.assertGroundExpr(ex)
      }
    }
    logStack = List()
  }

  /**
    * Serialize the context log. Defining custom serialization for Serializable.
    * @param oos object output stream
    */
  private def writeObject(oos: ObjectOutputStream): Unit = {
    val totalSize = logStack.map(_.size).sum + lastLog.size
    oos.writeObject(totalSize)
    for (logPerContext <- logStack) {
      for (record <- logPerContext) {
        oos.writeObject(record)
      }
    }
    for (record <- lastLog) {
      oos.writeObject(record)
    }
  }

  def replayLog(): Unit = {
    def applyRecord: Record => Unit = {
      case DeclareCellRecord(cell) => solver.declareCell(cell)
      case DeclareInPredRecord(set, elem) => solver.declareInPredIfNeeded(set, elem)
      case IntroIntConstRecord() => solver.introIntConst()
      case AssertGroundExprRecord(ex) => solver.assertGroundExpr(ex)
    }

    for (logPerContext <- logStack) {
      for (record <- logPerContext) {
        applyRecord(record)
      }
    }
    for (record <- lastLog) {
      applyRecord(record)
    }
  }

  /**
    * Save the current context and push it on the stack for a later recovery with pop.
    */
  override def push(): Unit = {
    solver.push()
    logStack = logStack :+ lastLog
    lastLog = List()
  }

  /**
    * Pop the previously saved context. Importantly, pop may be called multiple times and thus it is not sufficient
    * to save only the latest context.
    */
  override def pop(): Unit = {
    solver.pop()
    lastLog = logStack.last
    logStack = logStack.dropRight(1)
  }

  /**
    * Pop the context as many times as needed to reach a given level.
    *
    * @param n pop n times, if n > 0, otherwise, do nothing
    */
  override def pop(n: Int): Unit = {
    solver.pop(n)
    logStack = logStack.dropRight(n - 1) // n - 1 in logStack + 1 in lastLog
    lastLog = logStack.last
    logStack = logStack.dropRight(1)
  }

  /**
    * Clean the context
    */
  override def dispose(): Unit = {
    solver.dispose()
  }

  /**
    * Declare a constant for an arena cell.
    * This method is called automatically by the arena.
    *
    * @param cell a (previously undeclared) cell
    */
  override def declareCell(cell: ArenaCell): Unit = {
    lastLog = lastLog :+ DeclareCellRecord(cell)
    solver.declareCell(cell)
  }

  /**
    * Declare an arena edge of type 'has'. This method introduces a Boolean variable for the edge.
    * This method is called automatically by the arena. If the context already contains the constant
    * with the same name, then this method does nothing.
    *
    * @param set  the containing set
    * @param elem a set element
    */
  override def declareInPredIfNeeded(set: ArenaCell, elem: ArenaCell): Unit = {
    lastLog = lastLog :+ DeclareInPredRecord(set, elem)
    solver.declareInPredIfNeeded(set, elem)
  }

  /**
    * Check whether the current view of the SMT solver is consistent with arena.
    *
    * @param arena an arena
    */
  override def checkConsistency(arena: Arena): Unit = {
    solver.checkConsistency(arena)
  }

  /**
    * Get the names of the active integer constants (not the cells of type IntT).
    * This method is used for debugging purposes and may be slow.
    *
    * @return a list of integer constants that are active in the current context
    */
  override def getIntConsts: Iterable[String] = {
    solver.getIntConsts
  }

  /**
    * Introduce a new integer constant.
    *
    * WARNING: this method is obsolete and will be removed in the future. Just introduce a cell of type IntT().
    *
    * @return the name of a new constant
    */
  override def introIntConst(): String = {
    lastLog = lastLog :+ IntroIntConstRecord()
    solver.introIntConst()
  }

  /**
    * Assert that a Boolean TLA+ expression holds true.
    *
    * @param ex a simplified TLA+ expression over cells
    */
  override def assertGroundExpr(ex: TlaEx): Unit = {
    lastLog = lastLog :+ AssertGroundExprRecord(ex)
    solver.assertGroundExpr(ex)
  }

  /**
    * Evaluate a ground TLA+ expression in the current model, which is available after a call to sat().
    * This method assumes that the outcome is either a Boolean or integer.
    * If not, it throws SmtEncodingException.
    *
    * @param ex an expression to evaluate
    * @return a TLA+ value
    */
  override def evalGroundExpr(ex: TlaEx): TlaEx = {
    solver.evalGroundExpr(ex)
  }

  /**
    * Write a message to the log file. This is helpful to debug the SMT encoding.
    *
    * @param message message text, call-by-name
    */
  override def log(message: => String): Unit = {
    // ignore logging
  }

  /**
    * Is the current context satisfiable?
    *
    * @return true if and only if the context is satisfiable
    */
  override def sat(): Boolean = {
    solver.sat()
  }

  /**
    * Check satisfiability of the context with a timeout
    *
    * @param timeoutSec the timeout in seconds. If timeout <= 0, it is not effective
    * @return Some(result), if no timeout happened; otherwise, None
    */
  override def satOrTimeout(timeoutSec: Long): Option[Boolean] = {
    solver.satOrTimeout(timeoutSec)
  }

  /**
    * Register an SMT listener
    *
    * @param listener register a listener, overrides the previous listener, if it was set before
    */
  override def setSmtListener(listener: SmtListener): Unit = {
    solver.setSmtListener(listener)
  }
}
