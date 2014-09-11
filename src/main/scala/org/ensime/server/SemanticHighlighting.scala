package org.ensime.server

import org.ensime.model.{ SymbolDesignation, SymbolDesignations }
import org.slf4j.LoggerFactory
import scala.collection.mutable.ListBuffer
import scala.reflect.io.AbstractFile
import scala.reflect.internal.util.RangePosition
import scala.tools.nsc.symtab.Flags._
import scala.tools.refactoring.common.PimpedTrees
import scala.tools.refactoring.common.CompilerAccess

class SemanticHighlighting(val global: RichPresentationCompiler) extends CompilerAccess with PimpedTrees {

  import global._

  class SymDesigsTraverser(p: RangePosition, tpeSet: Set[scala.Symbol]) extends Traverser {

    val log = LoggerFactory.getLogger(getClass)
    val syms = ListBuffer[SymbolDesignation]()

    override def traverse(t: Tree): Unit = {

      val treeP = t.pos

      def addAt(start: Int, end: Int, designation: scala.Symbol): Boolean = {
        if (tpeSet.contains(designation)) {
          syms += SymbolDesignation(start, end, designation)
        }
        true
      }

      def add(designation: scala.Symbol): Boolean = {
        val pos = t.namePosition()
        addAt(pos.start, pos.end, designation)
      }

      def qualifySymbol(sym: Symbol): Boolean = {
        if (sym == NoSymbol) {
          false
        } else if (sym.isCaseApplyOrUnapply) {
          val owner = sym.owner
          val start = treeP.start
          val end = start + owner.name.length
          addAt(start, end, 'object)
        } else if (sym.isConstructor) {
          addAt(treeP.start, treeP.end, 'constructor)
        } else if (sym.isTypeParameterOrSkolem) {
          add('typeParam)
        } else if (sym.hasFlag(PARAM)) {
          add('param)
        } else if (sym.hasFlag(ACCESSOR)) {
          val under = sym.accessed
          if (under.isVariable) {
            add('varField)
          } else if (under.isValue) {
            add('valField)
          } else {
            false
          }
        } else if (sym.isMethod) {
          if (sym.nameString == "apply" || sym.nameString == "update") { true }
          else if (sym.name.isOperatorName) {
            add('operator)
          } else {
            add('functionCall)
          }
        } else if (sym.isVariable && sym.isLocalToBlock) {
          add('var)
        } else if (sym.isValue && sym.isLocalToBlock) {
          add('val)
        } else if (sym.hasPackageFlag) {
          add('package)
        } else if (sym.isTrait) {
          add('trait)
        } else if (sym.isClass) {
          add('class)
        } else if (sym.isModule) {
          add('object)
        } else {
          false
        }
      }

      if (!treeP.isTransparent && p.overlaps(treeP)) {
        try {
          val sym = t.symbol
          t match {
            case Import(expr, selectors) =>
              for (impSel <- selectors) {
                val start = impSel.namePos
                val end = start + impSel.name.decode.length()
                addAt(start, end, 'importedName)
              }
            case Ident(_) =>
              qualifySymbol(sym)
            case Select(_, _) =>
              qualifySymbol(sym)

            case ValDef(mods, name, tpt, rhs) =>
              if (sym != NoSymbol) {
                val isField = sym.owner.isType || sym.owner.isModule

                if (mods.hasFlag(PARAM)) {
                  add('param)
                } else if (mods.hasFlag(MUTABLE) && !isField) {
                  add('var)
                } else if (!isField) {
                  add('val)
                } else if (mods.hasFlag(MUTABLE) && isField) {
                  add('varField)
                } else if (isField) {
                  add('valField)
                }
              }

            case TypeDef(mods, name, params, rhs) =>
              if (sym != NoSymbol) {
                if (mods.hasFlag(PARAM)) {
                  add('typeParam)
                }
              }

            case TypeTree() =>
              if (!qualifySymbol(sym)) {
                if (t.tpe != null) {
                  // TODO:
                  // This case occurs when
                  // pattern matching on
                  // case classes.
                  // As in:
                  // case MyClass(a:Int,b:Int)
                  //
                  // Works, but this is *way* under-constrained.
                  val start = treeP.start
                  val end = treeP.end
                  addAt(start, end, 'object)
                }
              }
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

  def symbolDesignationsInRegion(
    p: RangePosition,
    tpes: List[scala.Symbol]): SymbolDesignations = {
    val tpeSet = Set.empty[scala.Symbol] ++ tpes
    val typed: Response[global.Tree] = new Response[global.Tree]
    global.askLoadedTyped(p.source, keepLoaded = true, typed)
    typed.get.left.toOption match {
      case Some(tree) =>
        val traverser = new SymDesigsTraverser(p, tpeSet)
        traverser.traverse(tree)
        SymbolDesignations(p.source.file.path, traverser.syms.toList)
      case None => SymbolDesignations("", List.empty)
    }
  }

  def compilationUnitOfFile(f: AbstractFile): Option[CompilationUnit] = global.unitOfFile.get(f)

}
