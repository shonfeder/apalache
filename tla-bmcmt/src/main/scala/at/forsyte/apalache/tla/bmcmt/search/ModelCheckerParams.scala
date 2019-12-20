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

  /**
    * A timeout upon which a transition is split in its own group.
    * This is the minimal timeout. The actual timeout is updated at every step using `search.split.timeout.factor`.
    *
    */
  val jailTimeoutMinSec: Long =
    BigInt(tuningOptions.getOrElse("search.split.timeout.minimum", "60")).toLong

  /**
   * At every step, the jail timeout for the next step is computed as `maxTime * factor / 100`,
    * where `maxTime` is the maximum checking time among all enabled or disabled transition.
   */
  val jailTimeoutFactor: Long =
    BigInt(tuningOptions.getOrElse("search.split.timeout.factor", "200")).toInt

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
