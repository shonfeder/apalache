package at.forsyte.apalache.tla.bmcmt.search

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TestHyperNode extends FunSuite {
  test("construct tree and check prefixes") {
    //    1
    //  /   \
    // 2    3
    //      |
    //      4
    val node2 = HyperNode(HyperTransition(1, 2))
    val node4 = HyperNode(HyperTransition(1, 4))
    val node3 = HyperNode(HyperTransition(3, 4), node4)
    val node1 = HyperNode(HyperTransition(1, 2, 3), node2, node3)
    assert(node4.parent == Some(node3))
    assert(node2.parent == Some(node1))
    assert(node3.parent == Some(node1))
    assert(node1.parent == None)
    // find operations
    assert(node4.findPrefix == Seq(1, 0))
    assert(node4.findPrefixAsString == "1.0")
    assert(node1.findByPrefix(Seq(1, 0)) == Some(node4))
    assert(node1.findByPrefix(Seq(1, 2)) == None)
    val ids = Set(node1, node2, node3, node4).map(_.id)
    assert(ids.size == 4)
    // adding nodes
    val node5 = HyperNode(HyperTransition(2))
    node3.append(node5)
    assert(node3.children == List(node4, node5))
  }
}
