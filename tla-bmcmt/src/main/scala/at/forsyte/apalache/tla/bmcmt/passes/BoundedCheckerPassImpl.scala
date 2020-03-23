package at.forsyte.apalache.tla.bmcmt.passes

import java.nio.file.Path

import at.forsyte.apalache.infra.ExceptionAdapter
import at.forsyte.apalache.infra.passes.{Pass, PassOptions}
import at.forsyte.apalache.tla.assignments.ModuleAdapter
import at.forsyte.apalache.tla.bmcmt.Checker.Outcome
import at.forsyte.apalache.tla.bmcmt._
import at.forsyte.apalache.tla.bmcmt.analyses.{ExprGradeStore, FormulaHintsStore}
import at.forsyte.apalache.tla.bmcmt.rewriter.RewriterConfig
import at.forsyte.apalache.tla.bmcmt.search._
import at.forsyte.apalache.tla.bmcmt.smt.RecordingZ3SolverContext
import at.forsyte.apalache.tla.bmcmt.trex._
import at.forsyte.apalache.tla.bmcmt.types.eager.TrivialTypeFinder
import at.forsyte.apalache.tla.imp.src.SourceStore
import at.forsyte.apalache.tla.lir.NullEx
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
class BoundedCheckerPassImpl @Inject() (val options: PassOptions,
                                        hintsStore: FormulaHintsStore,
                                        exprGradeStore: ExprGradeStore,
                                        sourceStore: SourceStore,
                                        changeListener: ChangeListener,
                                        exceptionAdapter: ExceptionAdapter,
                                        @Named("AfterChecker") nextPass: Pass)
      extends BoundedCheckerPass with LazyLogging {

  /**
    * The pass name.
    *
    * @return the name associated with the pass
    */
  override def name: String = "BoundedChecker"

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

    val initTrans = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.INIT_PREFIX)
    val nextTrans = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.NEXT_PREFIX)
    val cinitP = ModuleAdapter.getOperatorOption(module, NormalizedNames.CONST_INIT)
    val vcInvs = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.VC_INV_PREFIX)
    val vcNotInvs = ModuleAdapter.getTransitionsFromSpec(module, NormalizedNames.VC_NOT_INV_PREFIX)
    val invariantsAndNegations = vcInvs.zip(vcNotInvs)

    val input = new CheckerInput(module, initTrans.toList, nextTrans.toList, cinitP, invariantsAndNegations.toList)
    val nworkers = options.getOrElse("checker", "nworkers", 1)
    val stepsBound = options.getOrElse("checker", "length", 10)
    val tuning = options.getOrElse("general", "tuning", Map[String, String]())
    val debug = options.getOrElse("general", "debug", false)
    val saveDir = options.getOrError("io", "outdir").asInstanceOf[Path].toFile

    val params = new ModelCheckerParams(input, stepsBound, saveDir, tuning, debug)
    params.pruneDisabled = !options.getOrElse("checker", "allEnabled", false)
    params.checkForDeadlocks = !options.getOrElse("checker", "noDeadlocks", false)

    options.getOrElse("checker", "algo", "incremental") match {
      case "parallel" => runParallelChecker(params, input, tuning, nworkers)
      case "incremental" => runIncrementalChecker(params, input, tuning)
      case "offline" => runOfflineChecker(params, input, tuning)
      case algo => throw new IllegalArgumentException(s"Unexpected checker.algo=$algo")
    }

  }

  private def runIncrementalChecker(params: ModelCheckerParams,
                                    input: CheckerInput,
                                    tuning: Map[String, String]): Boolean = {
    val profile = options.getOrElse("smt", "prof", false)
    val solverContext: RecordingZ3SolverContext = RecordingZ3SolverContext(None, params.debug, profile)

    val typeFinder = new TrivialTypeFinder
    val rewriter: SymbStateRewriterImpl = new SymbStateRewriterImpl(solverContext, typeFinder, exprGradeStore)
    rewriter.formulaHintsStore = hintsStore
    rewriter.config = RewriterConfig(tuning)

    val executorContext = new IncrementalExecutorContext(rewriter)
    val trex = new TransitionExecutorImpl[IncrementalSnapshot](params.consts, params.vars, executorContext)
    val stepFilter = tuning.getOrElse("search.transitionFilter", "")
    val filteredTrex = new FilteredTransitionExecutor[IncrementalSnapshot](stepFilter, params.invFilter, trex)

    val checker = new SeqModelChecker[IncrementalSnapshot](params, input, filteredTrex)
    checker.run() == Outcome.NoError
  }

  private def runOfflineChecker(params: ModelCheckerParams,
                                input: CheckerInput,
                                tuning: Map[String, String]): Boolean = {
    val profile = options.getOrElse("smt", "prof", false)
    val solverContext: RecordingZ3SolverContext = RecordingZ3SolverContext(None, params.debug, profile)

    val typeFinder = new TrivialTypeFinder
    val rewriter: SymbStateRewriterImpl = new SymbStateRewriterImpl(solverContext, typeFinder, exprGradeStore)
    rewriter.formulaHintsStore = hintsStore
    rewriter.config = RewriterConfig(tuning)

    val executorContext = new OfflineExecutorContext(rewriter)
    val trex = new TransitionExecutorImpl[OfflineSnapshot](params.consts, params.vars, executorContext)
    val stepFilter = tuning.getOrElse("search.transitionFilter", "")
    val filteredTrex = new FilteredTransitionExecutor[OfflineSnapshot](stepFilter, params.invFilter, trex)

    val checker = new SeqModelChecker[OfflineSnapshot](params, input, filteredTrex)
    checker.run() == Outcome.NoError
  }

  private def runParallelChecker(params: ModelCheckerParams,
                                 input: CheckerInput,
                                 tuning: Map[String, String],
                                 nworkers: Int): Boolean = {
    val sharedState = new SharedSearchState(nworkers)

    def createCheckerThread(rank: Int): Thread = {
      new Thread {
        override def run(): Unit = {
          try {
            val checker = createParallelWorker(rank, sharedState, params, input, tuning)
            val outcome = checker.run()
            logger.info(s"Worker $rank: The outcome is: $outcome")
          } catch {
            case e: Exception if exceptionAdapter.toMessage.isDefinedAt(e) =>
              val message = exceptionAdapter.toMessage(e)
              logger.info(s"Worker $rank: The outcome is: Error")
              logger.error("Worker %s: %s".format(rank, message))

            case e: Throwable =>
              logger.error(s"Worker $rank has thrown an exception", e)
              System.exit(EXITCODE_ON_EXCEPTION)
          }
        }
      }
    }

    // run the threads and join
    val workerThreads = 1.to(nworkers) map createCheckerThread
    val shutdownHook = createShutdownHook(workerThreads)
    Runtime.getRuntime.addShutdownHook(shutdownHook)    // shutdown the threads, if needed
    workerThreads.foreach(_.start())                    // start the workers
    workerThreads.foreach(_.join())                     // wait for their termination
    Runtime.getRuntime.removeShutdownHook(shutdownHook) // no need for the hook anymore

    sharedState.workerStates.values.forall(_ == BugFreeState())
  }

  private def createParallelWorker(rank: Int,
                                 sharedState: SharedSearchState,
                                 params: ModelCheckerParams,
                                 input: CheckerInput,
                                 tuning: Map[String, String]): ModelChecker = {
    val profile = options.getOrElse("smt", "prof", false)
    val solverContext: RecordingZ3SolverContext = RecordingZ3SolverContext(None, params.debug, profile)

    val typeFinder = new TrivialTypeFinder
    val rewriter: SymbStateRewriterImpl = new SymbStateRewriterImpl(solverContext, typeFinder, exprGradeStore)
    rewriter.formulaHintsStore = hintsStore
    rewriter.config = RewriterConfig(tuning)
    val context = new WorkerContext(rank, sharedState.searchRoot, solverContext, rewriter, typeFinder)

    new ModelChecker(input, params, sharedState, context, changeListener, sourceStore)
  }

  private def createShutdownHook(workerThreads: Seq[Thread]): Thread = {
    new Thread() {
      override def run(): Unit = {
        logger.error("Shutdown hook activated. Interrupting the workers and joining them for %d ms."
          .format(JOIN_TIMEOUT_MS))
        workerThreads.foreach(_.interrupt())
        workerThreads.foreach(_.join(JOIN_TIMEOUT_MS))
        logger.error("Forced shutdown")
        Runtime.getRuntime.halt(EXITCODE_ON_SHUTDOWN)
      }
    }
  }

  /**
    * Get the next pass in the chain. What is the next pass is up
    * to the module configuration and the pass outcome.
    *
    * @return the next pass, if exists, or None otherwise
    */
  override def next(): Option[Pass] =
    tlaModule map {_ => nextPass}
}
