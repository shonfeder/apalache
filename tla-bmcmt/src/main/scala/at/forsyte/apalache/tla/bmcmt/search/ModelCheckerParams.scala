package at.forsyte.apalache.tla.bmcmt.search

/**
  * A collection of model checker parameters that come from user configurations.
  *
  * @author Igor Konnov
  */
class ModelCheckerParams(tuningOptions: Map[String, String]) {
  val stepFilters: Seq[String] =
    tuningOptions.getOrElse("search.transitionFilter", ".*").split(",")

  val invFilter: String =
    tuningOptions.getOrElse("search.invariantFilter", "")

  val transitionTimeout: Long =
    BigInt(tuningOptions.getOrElse("search.transition.timeout", "0")).toLong

  val invariantTimeout: Long =
    BigInt(tuningOptions.getOrElse("search.invariant.timeout", "0")).toLong

  // does the transition number satisfy the given filter at the given step?
  def stepMatchesFilter(stepNo: Int, transitionNo: Int): Boolean = {
    if (stepFilters.size <= stepNo) {
      true // no filter applied
    } else {
      transitionNo.toString.matches("^%s$".format(stepFilters(stepNo)))
    }
  }

}
