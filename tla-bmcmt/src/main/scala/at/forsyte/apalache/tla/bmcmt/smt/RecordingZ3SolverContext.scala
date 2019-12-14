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
  case class IntroBoolConstRecord() extends Record with Serializable
  case class IntroIntConstRecord() extends Record with Serializable
  case class AssertGroundExprRecord(ex: TlaEx) extends Record with Serializable
}

@SerialVersionUID(700L)
class RecordingZ3SolverContext(debug: Boolean, profile: Boolean) extends SolverContext with Serializable {
  import RecordingZ3SolverContext._

  private var solver = new Z3SolverContext(debug, profile)

  /**
    * The sequence of logs, one per context, reversed.
    */
  private var logStackRev: List[List[Record]] = List()

  /**
    * The top log, reversed.
    */
  private var topLogRev: List[Record] = List()

  /**
    * De-serialize the context log by smashing SMT contexts.
    * @param ois object input stream
    */
  private def readObject(ois: ObjectInputStream): Unit = {
    val totalSize = ois.readObject().asInstanceOf[Int]
    topLogRev = List[Record]()
    solver = new Z3SolverContext(debug, profile)
    for (_ <- 1.to(totalSize)) {
      val record = ois.readObject().asInstanceOf[Record]
      topLogRev = record :: topLogRev

      record match {
        case DeclareCellRecord(cell) => solver.declareCell(cell)
        case DeclareInPredRecord(set, elem) => solver.declareInPredIfNeeded(set, elem)
        case IntroBoolConstRecord() => solver.introBoolConst()
        case IntroIntConstRecord() => solver.introIntConst()
        case AssertGroundExprRecord(ex) => solver.assertGroundExpr(ex)
      }
    }
    logStackRev = List()
  }

  /**
    * Serialize the context log
    * @param oos object output stream
    */
  private def writeObject(oos: ObjectOutputStream): Unit = {
    val totalSize = logStackRev.map(_.size).sum + topLogRev.size
    oos.writeObject(totalSize)
    for (logPerContext <- logStackRev.reverse) {
      for (record <- logPerContext) {
        oos.writeObject(record)
      }
    }
    for (record <- topLogRev.reverse) {
      oos.writeObject(record)
    }
  }

  /**
    * Save the current context and push it on the stack for a later recovery with pop.
    */
  override def push(): Unit = {
    solver.push()
    logStackRev = topLogRev :: logStackRev
    topLogRev = List()
  }

  /**
    * Pop the previously saved context. Importantly, pop may be called multiple times and thus it is not sufficient
    * to save only the latest context.
    */
  override def pop(): Unit = {
    solver.pop()
    topLogRev = logStackRev.head
    logStackRev = logStackRev.tail
  }

  /**
    * Pop the context as many times as needed to reach a given level.
    *
    * @param n pop n times, if n > 0, otherwise, do nothing
    */
  override def pop(n: Int): Unit = {
    solver.pop(n)
    logStackRev = logStackRev.drop(n - 1)
    topLogRev = logStackRev.head
    logStackRev = logStackRev.tail
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
    topLogRev = DeclareCellRecord(cell) :: topLogRev
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
    topLogRev = DeclareInPredRecord(set, elem) :: topLogRev
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
    * Introduce a new Boolean constant.
    *
    * WARNING: this method is obsolete and will be removed in the future. Just introduce a cell of type BoolT().
    *
    * @return the name of a new constant
    */
  override def introBoolConst(): String = {
    topLogRev = IntroBoolConstRecord() :: topLogRev
    solver.introBoolConst()
  }

  /**
    * Get the names of the active Boolean constants (not the cells of type BoolT).
    * This method is used for debugging purposes and may be slow.
    *
    * @return a list of Boolean constants that are active in the current context
    */
  override def getBoolConsts: Iterable[String] = {
    solver.getBoolConsts
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
    topLogRev = IntroIntConstRecord() :: topLogRev
    solver.introIntConst()
  }

  /**
    * Assert that a Boolean TLA+ expression holds true.
    *
    * @param ex a simplified TLA+ expression over cells
    */
  override def assertGroundExpr(ex: TlaEx): Unit = {
    topLogRev = AssertGroundExprRecord(ex) :: topLogRev
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
