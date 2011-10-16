/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

package org.ensime.server
import org.ensime.model.Helpers
import org.ensime.model.SymbolDesignation
import org.ensime.model.SymbolDesignations
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.interactive.{ CompilerControl, Global }
import scala.tools.nsc.util.RangePosition

trait SemanticHighlighting { self: Global with Helpers =>

  class SymDesigsTraverser(p: RangePosition, tpeSet: Set[scala.Symbol]) extends Traverser {

    val syms = ListBuffer[SymbolDesignation]()
    var depth:Int = 0

    override def traverse(t: Tree) {

      //      var ds = ""
      //      for(i <- 0 until depth) ds += " "
      //      println(ds + t.getClass.getName)

      val treeP = t.pos

      def addAt(start: Int, end: Int, designation: scala.Symbol) {
        if (tpeSet.contains(designation)) {
          syms += SymbolDesignation(start, end, designation)
        }
      }

      def add(designation: scala.Symbol) {
	//	println(ds + "Adding:" + designation)
        addAt(treeP.start, treeP.end, designation)
      }

      var continue = true
      if (p.overlaps(treeP)) {
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
	    //            println("IDENT:" + symbolSummary(sym).toString())
            if (sym.isCaseApplyOrUnapply) {
              val owner = sym.owner
              val start = treeP.start
              val end = start + owner.name.length
              addAt(start, end, 'object)
            } else if (sym.isConstructor) {
              add('constructor)
            } else if (sym.isTypeParameter) {
              add('typeParam)
            } else if (sym.isParameter) {
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
            }  else {
	      //              println("WTF ident: " + sym)
            }
          }
          case Select(qual, selector: Name) => {
            val sym = t.symbol
	    //            println("SELECT:" + symbolSummary(sym).toString())
            val start = try {
              qual.pos.end + 1
            } catch {
              case _ => treeP.start
            }
            val len = selector.decode.length()
            val end = start + len

            if (sym.isCaseApplyOrUnapply) {
              val owner = sym.owner
              val start = treeP.start
              val end = start + owner.name.length
              addAt(start, end, 'object)
            } else if (sym.hasAccessorFlag) {
              val under = sym.accessed
              if (under.isVariable) {
                addAt(start, end, 'varField)
              } else if (under.isValue) {
                addAt(start, end, 'valField)
              } else {
		//                println("WTF accessor: " + under)
              }
            } else if (sym.isConstructor) {
              val start = try { sym.pos.start }
              catch { case _ => treeP.start }
              val end = try { sym.pos.end }
              catch { case _ => treeP.end }
              addAt(start, end, 'constructor)
            } else if (sym.isMethod) {
              if (selector.isOperatorName) {
                addAt(start, end, 'operator)
              } else if (sym.nameString == "apply") {}
              else {
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

            if (mods.isParameter) {
              addAt(start, end, 'param)
            } else if (mods.isMutable && !isField) {
              addAt(start, end, 'var)
            } else if (!isField) {
              addAt(start, end, 'val)
            } else if (mods.isMutable && isField) {
              addAt(start, end, 'varField)
            } else if (isField) {
              addAt(start, end, 'valField)
            }
          }

          case TypeTree() => {
            val sym = t.symbol
	    //            println("TypeTree:" + symbolSummary(sym).toString())
            val start = treeP.start
            val end = treeP.end
            if (sym.isTrait) {
              addAt(start, end, 'trait)
            } else if (sym.isClass) {
              addAt(start, end, 'class)
            } else if (sym.isModule) {
              addAt(start, end, 'object)
            }
          }

          case UnApply(t, ts) => {
            val sym = t.symbol
	    //            println("UnApply:" + symbolSummary(sym).toString())
            val owner = sym.owner
            val start = treeP.start
            val end = start + owner.name.length
            addAt(start, end, 'object)
          }

          case _ => {}
        }
	depth += 1
        if (continue) super.traverse(t)
	depth -= 1
      }
    }
  }

  protected def symbolDesignationsInRegion(p: RangePosition, 
    tpes: List[scala.Symbol]): SymbolDesignations = {
    val tpeSet = Set[scala.Symbol]() ++ tpes
    val traverser = new SymDesigsTraverser(p, tpeSet)
    val typed:Response[Tree] = new Response[Tree]
    askType(p.source, false, typed)
    typed.get.left.toOption match {
      case Some(tree) => {
        traverser.traverse(tree)
        SymbolDesignations(
          p.source.file.path,
          traverser.syms.toList)
      }
      case None => SymbolDesignations("", List())
    }
  }

}
