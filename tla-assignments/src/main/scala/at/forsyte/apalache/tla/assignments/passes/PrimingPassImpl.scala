package at.forsyte.apalache.tla.assignments.passes

import java.io.File
import java.nio.file.Path

import at.forsyte.apalache.infra.passes.{Pass, PassOptions, TlaModuleMixin}
import at.forsyte.apalache.tla.lir._
import at.forsyte.apalache.tla.lir.io.PrettyWriter
import at.forsyte.apalache.tla.lir.storage.{BodyMap, BodyMapFactory}
import at.forsyte.apalache.tla.lir.transformations.TransformationTracker
import at.forsyte.apalache.tla.lir.transformations.standard._
import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.scalalogging.LazyLogging

/**
  * PrimingPass adds primes to the variables in state initializers and constant initializers.
  */
class PrimingPassImpl @Inject()(options: PassOptions,
                                tracker: TransformationTracker,
                                @Named("AfterPriming") nextPass: Pass with TlaModuleMixin)
  extends PrimingPass with LazyLogging {
  /**
    * The name of the pass
    *
    * @return the name associated with the pass
    */
  override def name: String = "PrimingPass"

  /**
    * Run the pass
    *
    * @return true, if the pass was successful
    */
  override def execute(): Boolean = {
    val declarations = tlaModule.get.declarations
    val varSet = tlaModule.get.varDeclarations.map(_.name).toSet
    val constSet = tlaModule.get.constDeclarations.map(_.name).toSet
    val bodyMap = BodyMapFactory.makeFromDecls(declarations)

    val cinitPrime = primeDecl(bodyMap, "checker", "cinit", constSet)
    val initPrime = primeDecl(bodyMap, "checker", "init", varSet)
    val typeInitPrime = primeDecl(bodyMap, "boolifier", "typeInit", varSet)

    // build a new module
    val newDeclarations: Seq[TlaDecl] = declarations ++ Seq(cinitPrime, initPrime, typeInitPrime).flatten
    val newModule = new TlaModule(tlaModule.get.name, newDeclarations)

    val outdir = options.getOrError("io", "outdir").asInstanceOf[Path]
    PrettyWriter.write(newModule, new File(outdir.toFile, "out-priming.tla"))

    setModule(newModule)
    true
  }

  private def primeDecl(bodyMap: BodyMap,
                        pass: String,
                        optionName: String,
                        vars: Set[String]): Option[TlaOperDecl] = {
    options.get[String](pass, optionName) match {
      case None => None

      case Some(name) =>
        if (!bodyMap.contains(name)) {
          val msg = s"Operator $name is not declared (check the spec and the command-line parameters)"
          throw new UndeclaredOperatorError(msg)
        }
        val operatorBody = bodyMap(name).body
        val primedName = name + "Primed"
        logger.info(s"  > Introducing $primedName for $name'")
        val primeTransformer = Prime(vars, tracker)
        val primedBody = primeTransformer(DeepCopy(tracker)(operatorBody))
        val primedDecl = TlaOperDecl(primedName, List(), primedBody)
        Some(primedDecl)
    }
  }

  /**
    * Get the next pass in the chain. What is the next pass is up
    * to the module configuration and the pass outcome.
    *
    * @return the next pass, if exists, or None otherwise
    */
  override def next(): Option[Pass] = {
    tlaModule map { m =>
      nextPass.setModule(m)
      nextPass
    }
  }
}
