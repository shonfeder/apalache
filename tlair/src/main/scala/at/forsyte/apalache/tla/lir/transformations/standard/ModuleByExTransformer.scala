package at.forsyte.apalache.tla.lir.transformations.standard

import at.forsyte.apalache.tla.lir._
import at.forsyte.apalache.tla.lir.transformations.{TlaExTransformation, TlaModuleTransformation}

/**
  * This transformer uses a TlaExTransformer to modify all expressions inside a module.
  *
  * @author Igor Konnov
  */
class ModuleByExTransformer(exTrans: TlaExTransformation, whenPred: TlaDecl => Boolean)
      extends TlaModuleTransformation {
  override def apply(mod: TlaModule): TlaModule = {
    def mapOneDeclaration: TlaDecl => TlaDecl = {
      case d @ TlaOperDecl(name, params, body) =>
        if (!whenPred(d)) {
          d
        } else {
          TlaOperDecl(name, params, exTrans(body))
        }

      case d @ TlaAssumeDecl(body) =>
        if (!whenPred(d)) {
          d
        } else {
          TlaAssumeDecl(exTrans(body))
        }

      case d => d
    }

    new TlaModule(mod.name, mod.declarations map mapOneDeclaration)
  }
}

object ModuleByExTransformer {
  def apply(exTrans: TlaExTransformation): ModuleByExTransformer = {
    new ModuleByExTransformer(exTrans, _ => true)
  }

  def apply(exTrans: TlaExTransformation, whenPred: TlaDecl => Boolean): ModuleByExTransformer = {
    new ModuleByExTransformer(exTrans, whenPred)
  }
}
