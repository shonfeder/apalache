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

  val invariantSplitByTransition: Boolean =
    tuningOptions.getOrElse("search.invariant.split", "true").toLowerCase == "true"

  val learnTransFromUnsat: Boolean =
    tuningOptions.getOrElse("search.transition.learnFromUnsat", "").toLowerCase == "true"

  val learnInvFromUnsat: Boolean =
    tuningOptions.getOrElse("search.invariant.learnFromUnsat", "").toLowerCase == "true"

  val transitionTimeout: Long =
    BigInt(tuningOptions.getOrElse("search.transition.timeout", "0")).toLong

  val invariantTimeout: Long =
    BigInt(tuningOptions.getOrElse("search.invariant.timeout", "0")).toLong

}
