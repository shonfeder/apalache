package at.forsyte.apalache.tla.bmcmt.analyses

trait LoopAnalyser {
  def checkNotLiveness(): Boolean
}