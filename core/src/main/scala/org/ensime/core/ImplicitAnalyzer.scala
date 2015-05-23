package org.ensime.core

import java.io.File
import org.ensime.api._
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.RangePosition
import scala.reflect.io.AbstractFile
import scala.tools.nsc.symtab.Flags._

class ImplicitAnalyzer(val global: RichPresentationCompiler) {

  import global._

  class ImplicitsTraverser(p: Position) extends Traverser {
    val log = LoggerFactory.getLogger(getClass)
    val implicits = new ListBuffer[ImplicitInfo]

    override def traverse(t: Tree): Unit = {
      val treeP = t.pos
      if (p.overlaps(treeP) || treeP.includes(p)) {
        try {
          t match {
            case t: ApplyImplicitView =>
              implicits.append(ImplicitConversionInfo(
                treeP.start,
                treeP.end,
                SymbolInfo(t.fun.symbol)
              ))
            case t: ApplyToImplicitArgs =>
              val funIsImplicit = t.fun match {
                case tt: ApplyImplicitView => true
                case _ => false
              }
              implicits.append(ImplicitParamInfo(
                treeP.start,
                treeP.end,
                SymbolInfo(t.fun.symbol),
                t.args.map { a => SymbolInfo(a.symbol) },
                funIsImplicit
              ))
            case _ =>
          }
        } catch {
          case e: Throwable =>
            log.error("Error in AST traverse:", e)
        }
        super.traverse(t)
      }
    }
  }

  def implicitDetails(p: Position): List[ImplicitInfo] = {
    val typed = new global.Response[global.Tree]
    global.askLoadedTyped(p.source, keepLoaded = true, typed)
    typed.get.left.toOption match {
      case Some(tree) =>
        val traverser = new ImplicitsTraverser(p)
        traverser.traverse(tree)
        traverser.implicits.toList
      case _ => List.empty
    }
  }

}
