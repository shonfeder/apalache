package at.forsyte.apalache.tla.bmcmt.search

import java.util.concurrent.atomic.AtomicLong


/**
  * The tree that is constructed in the course of the search. In this tree, a node is indexed by the sequence
  * of edge indices.
  *
  * @author Igor Konnov
  */
class HyperTree(val id: Long, val transition: HyperTransition, initChildren: Seq[HyperTree]) extends Serializable {
  var parent: Option[HyperTree] = None
  private var nodeChildren: Seq[HyperTree] = initChildren

  // update the children's parent
  initChildren.foreach { _.parent = Some(this) }

  /**
    * Get the node children
    * @return the list of children (may change after append)
    */
  def children: Seq[HyperTree] = nodeChildren

  /**
    * Append one more node to the end of the children list. This method sets the child's parent to this.
    * @param child new child
    */
  def append(child: HyperTree): Unit = {
    nodeChildren = nodeChildren :+ child
    child.parent = Some(this)
  }

  /**
    * Find a node from the current one by following the node's prefix, that is, the sequence of children indices
    * that leads to the node.
    * @return Some(node) for the found node, if the sequence leads to a node, and None otherwise
    */
  def findByPrefix: HyperTree.indexType => Option[HyperTree] = {
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
  def findPrefix: HyperTree.indexType = {
    parent match  {
      case None => Seq()
      case Some(p) => p.findPrefix :+ p.children.indexOf(this)
    }
  }

  def findPrefixAsString: String = {
    findPrefix.mkString(".")
  }
}

object HyperTree {
  private var nextId: AtomicLong = new AtomicLong()

  type indexType = Seq[Int]

  def apply(transition: HyperTransition, children: HyperTree*): HyperTree = {
    val id = nextId.getAndIncrement()
    new HyperTree(id, transition, children)
  }
}
