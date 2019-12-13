package at.forsyte.apalache.tla.bmcmt.search

import at.forsyte.apalache.tla.bmcmt.CheckerInput

/**
  * A collection of model checker parameters that come from user configurations.
  *
  * @author Igor Konnov
  */
class ModelCheckerParams(checkerInput: CheckerInput,
                         val stepsBound: Int,
                         tuningOptions: Map[String, String] = Map(),
                         val debug: Boolean = true) {
  /**
    * A set of CONSTANTS, which are special (rigid) variables, as they do not change in the course of execution.
    */
  val constants = Set(checkerInput.rootModule.constDeclarations.map(_.name): _*)

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
