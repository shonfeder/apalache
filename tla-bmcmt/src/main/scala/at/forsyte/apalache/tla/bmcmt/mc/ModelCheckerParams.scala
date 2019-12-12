package at.forsyte.apalache.tla.bmcmt.mc

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

}
