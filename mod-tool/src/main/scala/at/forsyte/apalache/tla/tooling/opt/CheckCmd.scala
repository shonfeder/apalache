package at.forsyte.apalache.tla.tooling.opt

import java.io.File

import org.backuity.clist.{Command, _}

/**
  * This command initiates the 'check' command line.
  *
  * @author Igor Konnov
  */
class CheckCmd extends Command(name = "check",
  description = "Check a TLA+ specification") with General {

  var file: File = arg[File](description = "a file containing a TLA+ specification")
  var nworkers: Int = opt[Int](
    name = "nworkers", default = 1,
    description = "the number of workers for the parallel checker, default: 1")
  var algo: String = opt[String](
    name = "algo", default = "incremental",
    description = "the search algorithm: offline, incremental, parallel, default: incremental")
  var cinit: String = opt[String](
    name = "cinit", default = "",
    description = "the name of an operator that initializes CONSTANTS,\n" +
      "default: None")
  var init: String = opt[String](
    name = "init", default = "Init",
    description = "the name of an operator that initializes VARIABLES,\n" +
      "default: Init")
  var next: String = opt[String](
    name = "next", default = "Next",
    description = "the name of a transition operator, default: Next")
  var inv: String =
    opt[String](name = "inv", default = "",
      description = "the name of an invariant operator, e.g., Inv")
  var length: Int =
    opt[Int](name = "length", default = 10,
      description = "maximal number of Next steps, default: 10")
  var tuning: String =
    opt[String](name="tuning", default = "",
      description = "filename of the tuning options, see docs/tuning.md")
  var enabled: Boolean = opt[Boolean](
    name = "all-enabled", default = false,
    description = "treat all transitions as enabled, to save on SMT queries, default: false")
  var noDeadlocks: Boolean = opt[Boolean](
    name = "no-deadlock", default = true,
    description = "do not check for deadlocks, default: true")
}
