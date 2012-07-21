/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.server
import org.ensime.model.{ Helpers, SymbolDesignation, SymbolDesignations }
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.interactive.{ CompilerControl, Global }
import scala.tools.nsc.util.RangePosition
import scala.tools.nsc.symtab.Flags._
import scala.math

trait SemanticHighlighting { self: Global with Helpers =>

  class SymDesigsTraverser(p: RangePosition, tpeSet: Set[scala.Symbol]) extends Traverser {

    val syms = ListBuffer[SymbolDesignation]()

    override def traverse(t: Tree) {


      val treeP = t.pos

      def addAt(start: Int, end: Int, designation: scala.Symbol) {
        if (tpeSet.contains(designation)) {
          syms += SymbolDesignation(start, end, designation)
        }
      }

      def add(designation: scala.Symbol) {
        addAt(treeP.start, treeP.end, designation)
      }

      if (!treeP.isTransparent && p.overlaps(treeP)) {
	try {
          t match {
            case Import(expr, selectors) => {
              for (impSel <- selectors) {
                val start = impSel.namePos
                val end = start + impSel.name.decode.length()
                addAt(start, end, 'importedName)
              }
            }
            case Ident(_) => {
              val sym = t.symbol
              if (sym != NoSymbol) {
                if (sym.isCaseApplyOrUnapply) {
                  val owner = sym.owner
                  val start = treeP.start
                  val end = start + owner.name.length
                  addAt(start, end, 'object)
                } else if (sym.isConstructor) {
                  add('constructor)
                } else if (sym.isTypeParameter) {
                  add('typeParam)
                } else if (sym.hasFlag(PARAM)) {
                  add('param)
                } else if (sym.isMethod) {
                  add('functionCall)
                } else if (sym.isPackage) {
                  add('package)
                } else if (sym.isTrait) {
                  add('trait)
                } else if (sym.isClass) {
                  add('class)
                } else if (sym.isModule) {
                  add('object)
                } else if (sym.isVariable && sym.isLocal) {
                  add('var)
                } else if (sym.isValue && sym.isLocal) {
                  add('val)
                } else if (sym.isVariable) {
                  add('varField)
                } else if (sym.isValue) {
                  add('valField)
                }
              }
            }
            case Select(qual, selector: Name) => {
              val sym = t.symbol
              val start = try {
                qual.pos.end + 1
              } catch {
                case _ : Throwable => treeP.start
              }
              val len = selector.decode.length()
              val end = start + len

              if (sym.isCaseApplyOrUnapply) {
                val owner = sym.owner
                val start = treeP.start
                val end = start + owner.name.length
                addAt(start, end, 'object)
              } else if (sym.hasFlag(ACCESSOR)) {
                val under = sym.accessed
                if (under.isVariable) {
                  addAt(start, end, 'varField)
                } else if (under.isValue) {
                  addAt(start, end, 'valField)
                }
              } else if (sym.isConstructor) {
                val start = try { sym.pos.start }
                catch { case _ : Throwable => treeP.start }
                val end = try { sym.pos.end }
                catch { case _ : Throwable => treeP.end }
                addAt(start, end, 'constructor)
              } else if (sym.isMethod) {
                if (sym.nameString == "apply" ||
                  sym.nameString == "update") {}
                else if (selector.isOperatorName) {
                  addAt(start, end, 'operator)
                } else {
                  addAt(start, end, 'functionCall)
                }
              } else if (sym.isPackage) {
                addAt(start, end, 'package)
              } else if (sym.isTrait) {
                addAt(start, end, 'trait)
              } else if (sym.isClass) {
                addAt(start, end, 'class)
              } else if (sym.isModule) {
                addAt(start, end, 'object)
              } else {
                addAt(start, end, 'functionCall)
              }
            }


            case ValDef(mods, name, tpt, rhs) => {
              val sym = t.symbol
              if (sym != NoSymbol) {

                // TODO:
                // Unfotunately t.symbol.pos returns a RangePosition
                // that covers the entire declaration.
                //
                // This is brittle, but I don't know a better
                // way to get the position of just the name.

                val start = if (mods.positions.isEmpty) t.pos.start
                else mods.positions.map(_._2.end).max + 2
                val len = name.decode.length()
                val end = start + len
                val isField = sym.owner.isType || sym.owner.isModule

                if (mods.hasFlag(PARAM)) {
                  addAt(start, end, 'param)
                } else if (mods.hasFlag(MUTABLE) && !isField) {
                  addAt(start, end, 'var)
                } else if (!isField) {
                  addAt(start, end, 'val)
                } else if (mods.hasFlag(MUTABLE) && isField) {
                  addAt(start, end, 'varField)
                } else if (isField) {
                  addAt(start, end, 'valField)
                }
              }
            }

            case TypeTree() => {
              val sym = t.symbol
              val start = treeP.start
              val end = treeP.end
              if (sym.isTrait) {
                addAt(start, end, 'trait)
              } else if (sym.isClass) {
                addAt(start, end, 'class)
              } else if (sym.isModule) {
                addAt(start, end, 'object)
              } else if (t.tpe != null) {
                // TODO:
                // This case occurs when
                // pattern matching on
                // case classes.
                // As in:
                // case MyClass(a:Int,b:Int)
                //
                // Works, but this is *way* under-constrained.
                addAt(start, end, 'object)
              }
            }
            case _ => {}
          }
	}
	catch{
	  case e : Throwable => {
	    System.err.println("Error in AST traverse:")
	    e.printStackTrace(System.err);
	  }
	}
        super.traverse(t)
      }
    }
  }

  protected def symbolDesignationsInRegion(p: RangePosition,
    tpes: List[scala.Symbol]): SymbolDesignations = {
    val tpeSet = Set[scala.Symbol]() ++ tpes
    val typed: Response[Tree] = new Response[Tree]
    askType(p.source, false, typed)
    typed.get.left.toOption match {
      case Some(tree) => {

	// TODO: Disable designations for
	// regions with errors?

        //        val cu = unitOf(p.source)
        //        var startOfProblems = p.end
        //        for (prob <- cu.problems) {
        //	  if(prob.severityLevel >= 2){
        //            startOfProblems = math.min(
        //              prob.pos.start, startOfProblems)
        //	  }
        //        }

        val traverser = new SymDesigsTraverser(p, tpeSet)
        traverser.traverse(tree)
        SymbolDesignations(
          p.source.file.path,
          traverser.syms.toList)
      }
      case None => SymbolDesignations("", List())
    }
  }

}
