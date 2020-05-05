package at.forsyte.apalache.tla.boolka.io

import java.io.PrintWriter

import at.forsyte.apalache.tla.boolka.{BoolSys, Cube}
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter

/**
  * <p>A pretty printer of a Boolean system to NuSMV format.</p>
  *
  * @author Igor Konnov
  */
class BoolSysSmvWriter(writer: PrintWriter, textWidth: Int = 80, indent: Int = 2) extends PrettyPrinter {
  override val defaultIndent: Int = indent

  def write(sys: BoolSys): Unit = {
    writer.write(pretty(toDoc(sys), textWidth).layout)
  }

  def toDoc(sys: BoolSys): Doc = {
    val predDecls = 0.until(sys.npreds).map(n => text(s"p$n") <> colon <> space <> text("boolean") <> semi)

    "MODULE main" <> line <>
      text("VAR") <>
      nest(lsep(predDecls, text("")), 2) <> line <>
      text("INIT") <> nest(line <> parens(cubeListToDoc(sys.npreds, sys.init)), 2) <> line <>
      text("TRANS") <> nest(line <> parens(cubeListToDoc(sys.npreds, sys.next)), 2) <> line <>
      text("SPEC") <> space <> "AG" <> space <> "!" <> parens(nest(cubeListToDoc(sys.npreds, sys.notInv), 2))
  }

  def cubeListToDoc(npreds: Int, cubes: List[Cube]): Doc = {
    val cubeDocs = cubes.map(c => cubeToDoc(npreds, c))
    if (cubeDocs.isEmpty) {
      text("TRUE")
    } else {
      group(lsep2(cubeDocs, " |"))
    }
  }

  def cubeToDoc(npreds: Int, cube: Cube): Doc = {
    def mkLit(i: Int) = {
      val doc = predToDoc(npreds, i)
      if (cube.bits(i)) doc else "!" <> doc
    }

    val lits = 0.until(cube.nbits).filter(i => cube.mask(i)).map(mkLit)

    if (lits.isEmpty) {
      text("TRUE")
    } else {
      group(lsep(lits, " &"))
    }
  }

  def predToDoc(npreds:Int, no: Int): Doc = {
    if (no < npreds) {
      text(s"p$no")
    } else {
      text("next") <> parens(text("p%d".format(no - npreds)))
    }
  }
}


