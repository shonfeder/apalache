package at.forsyte.apalache.tla.bmcmt

package object search {
  /**
    * A transition that corresponds to a non-deterministic choice of one of the enabled transitions from the set.
    */
  class HyperTransition(val indices: Set[Int]) extends Serializable {
    /**
      * If isJailed = true, then the transition has been isolated in a group of its own.
      */
    var isJailed = false
  }

  object HyperTransition {
    def apply(indices: Int*): HyperTransition = {
      new HyperTransition(indices.toSet)
    }
  }

  /**
    * A hyperpath is a sequence of hypertransitions T_1, ..., T_n, T_n+1, where every t \in T_i+1 is enabled
    * for at least one feasible sequence of transitions from T_1, ..., T_i, for 1 <= i <= n.
    */
  type HyperPath = Seq[HyperTransition]
}
