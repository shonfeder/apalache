package at.forsyte.apalache.tla.bmcmt.search

import java.io.File

import at.forsyte.apalache.tla.bmcmt.CheckerInput

/**
  * A collection of model checker parameters that come from the user configuration.
  *
  * @author Igor Konnov
  */
class ModelCheckerParams(checkerInput: CheckerInput,
                         val stepsBound: Int,
                         val saveDirectory: File,
                         tuningOptions: Map[String, String] = Map(),
                         val debug: Boolean) {
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

  /**
    * A timeout upon which a transition is split in its own group.
    */
  val jailTimeout: Long =
    BigInt(tuningOptions.getOrElse("search.split.timeout", "60")).toLong

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
