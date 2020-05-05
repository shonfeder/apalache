package at.forsyte.apalache.tla.boolka.passes

import java.io.{FileWriter, PrintWriter}
import java.nio.file.Path

import at.forsyte.apalache.infra.ExceptionAdapter
import at.forsyte.apalache.infra.passes.{Pass, PassOptions}
import at.forsyte.apalache.tla.assignments.ModuleAdapter
import at.forsyte.apalache.tla.bmcmt._
import at.forsyte.apalache.tla.bmcmt.analyses.{ExprGradeStore, FormulaHintsStore}
import at.forsyte.apalache.tla.bmcmt.rewriter.RewriterConfig
import at.forsyte.apalache.tla.bmcmt.search._
import at.forsyte.apalache.tla.bmcmt.smt.RecordingZ3SolverContext
import at.forsyte.apalache.tla.bmcmt.trex._
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.boolka.io.BoolSysSmvWriter
import at.forsyte.apalache.tla.boolka.{Boolifier, BoolifierInput}
import at.forsyte.apalache.tla.imp.src.SourceStore
import at.forsyte.apalache.tla.lir.{NullEx, TlaEx}
import at.forsyte.apalache.tla.lir.storage.ChangeListener
import at.forsyte.apalache.tla.lir.transformations.LanguageWatchdog
import at.forsyte.apalache.tla.lir.transformations.standard.KeraLanguagePred
import at.forsyte.apalache.tla.pp.NormalizedNames
import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.scalalogging.LazyLogging

/**
  * The implementation of a bounded model checker with SMT.
  *
  * @author Igor Konnov
  */
class BoolifierPassImpl @Inject()(val options: PassOptions,
                                  hintsStore: FormulaHintsStore,
                                  exprGradeStore: ExprGradeStore,
                                  sourceStore: SourceStore,
                                  changeListener: ChangeListener,
                                  exceptionAdapter: ExceptionAdapter,
                                  @Named("AfterBoolifier") nextPass: Pass)
  extends BoolifierPass with LazyLogging {

  /**
    * The pass name.
    *
    * @return the name associated with the pass
    */
  override def name: String = "Boolifier"

  /**
    * Run the pass.
    *
    * @return true, if the pass was successful
    */
  override def execute(): Boolean = {
    if (tlaModule.isEmpty) {
      throw new CheckerException(s"The input of $name pass is not initialized", NullEx)
    }
    val module = tlaModule.get

    for (decl <- module.operDeclarations) {
      LanguageWatchdog(KeraLanguagePred()).check(decl.body)
    }

    // input to the transition executor
    val initTrans = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.INIT_PREFIX)
    val nextTrans = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.NEXT_PREFIX)
    val cinitP = ModuleAdapter.getOperatorOption(module, NormalizedNames.CONST_INIT)
    val vcInvs = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.VC_INV_PREFIX)
    val vcNotInvs = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.VC_NOT_INV_PREFIX)
    val invariantsAndNegations = vcInvs.zip(vcNotInvs)

    val checkerInput = new CheckerInput(module, initTrans.toList, nextTrans.toList, cinitP, invariantsAndNegations.toList)
    val nworkers = options.getOrElse("checker", "nworkers", 1)
    val stepsBound = options.getOrElse("checker", "length", 10)
    val debug = options.getOrElse("general", "debug", false)
    val saveDir = options.getOrError("io", "outdir").asInstanceOf[Path].toFile

    val params = new ModelCheckerParams(checkerInput, stepsBound, saveDir, Map(), debug)

    // input to the Boolifier
    val typeInitTrans = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.TYPE_INIT_PREFIX)
    val boolifierInput = new BoolifierInput(typeInitTrans.toList, extractPredicates())

    runBoolifier(params, checkerInput, boolifierInput)
    // For now, return true. This may change in the future.
    true
  }

  private def runBoolifier(params: ModelCheckerParams,
                           checkerInput: CheckerInput,
                           boolifierInput: BoolifierInput): Unit = {
    val profile = options.getOrElse("smt", "prof", false)
    val solverContext: RecordingZ3SolverContext = RecordingZ3SolverContext(None, params.debug, profile)

    val typeFinder = new TrivialTypeFinder
    val rewriter: SymbStateRewriterImpl = new SymbStateRewriterImpl(solverContext, typeFinder, exprGradeStore)
    rewriter.formulaHintsStore = hintsStore
    rewriter.config = RewriterConfig(Map())

    val executorContext = new IncrementalExecutorContext(rewriter)
    val trex = new TransitionExecutorImpl[IncrementalSnapshot](params.consts, params.vars, executorContext)
    val filteredTrex = new FilteredTransitionExecutor[IncrementalSnapshot](params.transitionFilter, params.invFilter, trex)

    val boolifier = new Boolifier(checkerInput, boolifierInput, solverContext, filteredTrex)
    logger.info(" > Computing Boolean abstraction with %d predicates".format(boolifierInput.preds.length))
    val boolSys = boolifier.compute()
    logger.info(" > Done")
    logger.info(" > Writing the system to bool.smv")
    val writer = new PrintWriter(new FileWriter("bool.smv"))
    try {
      new BoolSysSmvWriter(writer).write(boolSys)
    } finally {
      writer.close()
    }
    logger.info(" > Done")
  }

  private def extractPredicates(): List[TlaEx] = {
    val module = tlaModule.get
    val predOpers =
      module.operDeclarations.filter {
        _.name.startsWith(NormalizedNames.PRED_PREFIX) // transitions end in 0,1,...
      }

    predOpers.sortBy(_.name).map(_.body).toList
  }

  /**
    * Get the next pass in the chain. What is the next pass is up
    * to the module configuration and the pass outcome.
    *
    * @return the next pass, if exists, or None otherwise
    */
  override def next(): Option[Pass] =
    tlaModule map { _ => nextPass }
}
