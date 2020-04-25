package at.forsyte.apalache.tla.bmcmt.search

import java.io.{File, FileWriter, PrintWriter}
import java.util.concurrent.atomic.AtomicLong

import at.forsyte.apalache.tla.bmcmt.trex.OfflineSnapshot
import at.forsyte.apalache.tla.lir.TlaEx


/**
  * The tree that is constructed in the course of the search. In this tree, a node is indexed by the sequence
  * of edge indices.
  *
  * @author Igor Konnov
  */
class HyperNode private(val id: Long, val transition: HyperTransition) extends Serializable {
  var depth: Int = 0

  var parent: Option[HyperNode] = None

  private var nodeChildren: Seq[HyperNode] = Seq()

  /**
    * Is the node explored? If true, the workers have to pick another node, e.g., one of the node's children.
    */
  var isExplored: Boolean = false

  /**
    * The status of the transitions that have to be explored in this node.
    */
  var openTransitions: Map[Int, (TlaEx, TransitionStatus)] = Map()

  /**
    * The status of the transitions that have been explored in this node (excluding the ones that were split).
    */
  var closedTransitions: Map[Int, (TlaEx, TransitionStatus)] = Map()

  /**
    * A set of the transitions that took too long to be checked. The node can be closed
    * without waiting for the slow ones. The slow transitions will be isolated in their
    * own nodes.
    */
  var slowTransitions: Set[Int] = Set()

  /**
    * Have all verification conditions in the node been checked with the SMT solver?
    */
  var isChecked: Boolean = false

  /**
    * The verification conditions that are yet to be proven.
    */
  var unprovenVCs: Map[Int, VCStatus] = Map()

  /**
    * The verification conditions that were proven.
    */
  var provenVCs: Map[Int, VCStatus] = Map()

  /**
    * The snapshot that is made after exploring the node.
    */
  var snapshot: Option[WorkerContext.SnapshotT] = None

  /**
    * The timeout that is used to jail a transition. It is set by ModelChecker.
    */
  var jailTimeoutSec: Long = 60

  /**
    * Get the node children
    * @return the list of children (may change after append)
    */
  def children: Seq[HyperNode] = nodeChildren

  /**
    * Append one more node to the end of the children list. This method sets the child's parent to this.
    * @param child new child
    */
  def append(child: HyperNode): Unit = {
    nodeChildren = nodeChildren :+ child
    child.parent = Some(this)
    child.depth = depth + 1
  }

  /**
    * Find a node from the current one by following the node's prefix, that is, the sequence of children indices
    * that leads to the node.
    * @return Some(node) for the found node, if the sequence leads to a node, and None otherwise
    */
  def findByPrefix: HyperNode.indexType => Option[HyperNode] = {
    case Nil => Some(this)
    case childIndex :: tail =>
      if (children.isDefinedAt(childIndex)) {
        children(childIndex).findByPrefix(tail)
      } else {
        None
      }
  }

  /**
    * Given a tree node, find the node's prefix from the root to the node.
    * @return node's prefix, that is, the sequence of indices leading from the root to the node.
    */
  def findPrefix: HyperNode.indexType = {
    parent match  {
      case None => Seq()
      case Some(p) => p.findPrefix :+ p.children.indexOf(this)
    }
  }

  def findPrefixAsString: String = {
    findPrefix.mkString(".")
  }

  /**
    * Compute the maximum time of feasibility checking among the transitions, except timeouts
    * @return the maximum time in milliseconds
    */
  def maxTransitionTimeMs(): Long = {
    def transitionTime: TransitionStatus => Long = {
      case EnabledTransition(ms, _) => ms
      case DisabledTransition(ms) => ms
      case _ => 0
    }

    closedTransitions.values.map(p => transitionTime(p._2)).foldLeft(0L) { case (max, ms) => Math.max(max, ms) }
  }

  /**
    * Same as maxTransitionTimeMs but truncated to seconds.
    * @return maximal transition time in seconds
    */
  def maxTransitionTimeSec(): Long = {
    maxTransitionTimeMs() / 1000
  }

  /**
    * Print the node (including its children) in the JSON format. The last line does not contain line feed.
    *
    * @param nspaces the number of spaces to add in front of every line.
    * @param writer a print writer
    */
  def printJson(nspaces: Int, writer: PrintWriter): Unit = {
    def transitionStatus(keyValue: (Int, (TlaEx, TransitionStatus))): String = {
      "{%d: \"%s\"}".format(keyValue._1, keyValue._2._2)
    }

    def vcStatus(keyValue: (Int, VCStatus)): String = {
      "{%d: \"%s\"}".format(keyValue._1, keyValue._2)
    }

    val spaces = " " * nspaces
    def indentln(text: String): Unit = {
      writer.println(spaces + text)
    }
    indentln("{")
    indentln(s""" "id": $id,""")
    indentln(""" "parent": %s,""".format(if (parent.isDefined) parent.get.id else """"None""""))
    indentln(s""" "depth": $depth,""")
    indentln(""" "transition": [%s],""".format(transition.indices.toList.sorted.mkString(", ")))
    indentln(s""" "isExplored": $isExplored,""")
    indentln(s""" "isChecked": $isChecked,""")
    indentln(s""" "jailTimeoutSec": $jailTimeoutSec,""")
    indentln(s""" "openTransitions": [%s],""".
      format(openTransitions.map(transitionStatus).mkString(", ")))
    indentln(s""" "closedTransitions": [%s],""".
      format(closedTransitions.map(transitionStatus).mkString(", ")))
    indentln(s""" "slowTransitions": [%s],""".
      format(slowTransitions.toList.sorted.mkString(", ")))
    indentln(s""" "provenVCs": [%s],""".
      format(provenVCs.map(vcStatus).mkString(", ")))
    indentln(s""" "unprovenVCs": [%s],""".
      format(unprovenVCs.map(vcStatus).mkString(", ")))
    indentln(""" "children": [""")
    if (children.nonEmpty) {
      children.head.printJson(nspaces + 2, writer)
      for (child <- children.tail) {
        writer.println(",")
        child.printJson(nspaces + 2, writer)
      }
    }
    writer.println("")
    indentln(" ]")
    writer.print(spaces + "}")
  }

  /**
    * Print the node including its children in the JSON format.
    *
    * @param file output file
    */
  def printJsonToFile(file: File): Unit = {
    val writer = new PrintWriter(new FileWriter(file))
    try {
      printJson(0, writer)
    } finally {
      writer.close()
    }
  }
}

object HyperNode {
  private val nextId: AtomicLong = new AtomicLong()

  type indexType = Seq[Int]

  def apply(transition: HyperTransition, children: HyperNode*): HyperNode = {
    val id = nextId.getAndIncrement()
    val node = new HyperNode(id, transition)
    children.foreach(node.append)
    node
  }
}
