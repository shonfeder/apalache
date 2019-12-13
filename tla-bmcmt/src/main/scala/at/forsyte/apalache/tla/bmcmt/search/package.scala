package at.forsyte.apalache.tla.bmcmt

import at.forsyte.apalache.tla.bmcmt.Checker.Outcome.Value

package object search {
  /**
    * A transition that corresponds to a non-deterministic choice of one of the enabled transitions from the set.
    */
  type HyperTransition = Set[Int]

  /**
    * A hyperpath is a sequence of hypertransitions T_1, ..., T_n, T_n+1, where every t \in T_i+1 is enabled
    * for at least one feasible sequence of transitions from T_1, ..., T_i, for 1 <= i <= n.
    */
  type HyperPath = Seq[HyperTransition]
}
