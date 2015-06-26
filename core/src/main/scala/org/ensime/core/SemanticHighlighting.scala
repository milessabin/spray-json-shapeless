package org.ensime.core

import java.io.File
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.RangePosition
import scala.reflect.io.AbstractFile
import scala.tools.nsc.symtab.Flags._
import scala.tools.refactoring.common.{ CompilerAccess, PimpedTrees }

import org.ensime.api._

class SemanticHighlighting(val global: RichPresentationCompiler) extends CompilerAccess with PimpedTrees {

  import global._

  class SymDesigsTraverser(p: RangePosition, tpeSet: Set[SourceSymbol]) extends Traverser {

    val log = LoggerFactory.getLogger(getClass)
    val syms = ListBuffer[SymbolDesignation]()

    override def traverse(t: Tree): Unit = {

      val treeP = t.pos

      def addAt(start: Int, end: Int, designation: SourceSymbol): Boolean = {
        if (tpeSet.contains(designation)) {
          syms += SymbolDesignation(start, end, designation)
        }
        true
      }

      def add(designation: SourceSymbol): Boolean = {
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
          addAt(start, end, ObjectSymbol)
        } else if (sym.isConstructor) {
          addAt(treeP.start, treeP.end, ConstructorSymbol)
        } else if (sym.isTypeParameterOrSkolem) {
          add(TypeParamSymbol)
        } else if (sym.hasFlag(PARAM)) {
          add(ParamSymbol)
        } else if (sym.hasFlag(ACCESSOR)) {
          val under = sym.accessed
          if (under.isVariable) {
            add(VarFieldSymbol)
          } else if (under.isValue) {
            add(ValFieldSymbol)
          } else {
            false
          }
        } else if (sym.isMethod) {
          if (sym.nameString == "apply" || sym.nameString == "update") { true }
          else if (sym.name.isOperatorName) {
            add(OperatorFieldSymbol)
          } else {
            add(FunctionCallSymbol)
          }
        } else if (sym.isVariable && sym.isLocalToBlock) {
          add(VarSymbol)
        } else if (sym.isValue && sym.isLocalToBlock) {
          add(ValSymbol)
        } else if (sym.hasPackageFlag) {
          add(PackageSymbol)
        } else if (sym.isTrait) {
          add(TraitSymbol)
        } else if (sym.isClass) {
          add(ClassSymbol)
        } else if (sym.isModule) {
          add(ObjectSymbol)
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
                addAt(start, end, ImportedNameSymbol)
              }
            case Ident(_) =>
              qualifySymbol(sym)
            case Select(_, _) =>
              qualifySymbol(sym)

            case ValDef(mods, name, tpt, rhs) =>
              if (sym != NoSymbol) {
                val isField = sym.owner.isType || sym.owner.isModule

                if (mods.hasFlag(PARAM)) {
                  add(ParamSymbol)
                } else if (mods.hasFlag(MUTABLE) && !isField) {
                  add(VarSymbol)
                } else if (!isField) {
                  add(ValSymbol)
                } else if (mods.hasFlag(MUTABLE) && isField) {
                  add(VarFieldSymbol)
                } else if (isField) {
                  add(ValFieldSymbol)
                }
              }

            case TypeDef(mods, name, params, rhs) =>
              if (sym != NoSymbol) {
                if (mods.hasFlag(PARAM)) {
                  add(TypeParamSymbol)
                }
              }

            case t: ApplyImplicitView => add(ImplicitConversionSymbol)
            case t: ApplyToImplicitArgs => add(ImplicitParamsSymbol)

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
                  addAt(start, end, ObjectSymbol)
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
    requestedTypes: List[SourceSymbol]
  ): SymbolDesignations = {
    val typed = new Response[Tree]
    askLoadedTyped(p.source, keepLoaded = true, typed)
    typed.get.left.toOption match {
      case Some(tree) =>
        val traverser = new SymDesigsTraverser(p, requestedTypes.toSet)
        traverser.traverse(tree)
        SymbolDesignations(p.source.file.file, traverser.syms.toList)
      case None =>
        SymbolDesignations(new File("."), List.empty)
    }
  }

  def compilationUnitOfFile(f: AbstractFile): Option[CompilationUnit] = unitOfFile.get(f)

}
