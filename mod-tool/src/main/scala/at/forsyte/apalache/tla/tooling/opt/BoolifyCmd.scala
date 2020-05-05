package at.forsyte.apalache.tla.tooling.opt

import java.io.File

import org.backuity.clist.{Command, _}

/**
  * This command initiates the 'check' command line.
  *
  * @author Igor Konnov
  */
class BoolifyCmd extends Command(name = "boolify",
  description = "Abstract a TLA+ specification into a Boolean transition system") with General {

  var file: File = arg[File](description = "a file containing a TLA+ specification")
  var config: String = opt[String](
    name = "config", default = "",
    description = "configuration file in TLC format,\n" +
      "default: <file>.cfg, or none if <file>.cfg not present")
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
  var typeInit: String = opt[String](
    name = "typeInit", default = "TypeOK",
    description = "the name of an inductive invariant over types,\n" +
      "default: TypeOK")
  var length: Int =
    opt[Int](name = "length", default = 1,
      description = "maximal number of Next steps, default: 1")
}
