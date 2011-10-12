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
  class SymDesigsTraverser(p: RangePosition) extends Traverser {
    val syms = ListBuffer[SymbolDesignation]()
    override def traverse(t: Tree) {
      val treeP = t.pos

      def addAt(start: Int, end: Int, designation: scala.Symbol) {
        syms += SymbolDesignation(start, end, designation)
      }

      def add(designation: scala.Symbol) {
        addAt(treeP.start, treeP.end, designation)
      }

      def nameStart(mods: Modifiers, owner: Tree): Int = {
        if (mods.positions.isEmpty) owner.pos.start
        else mods.positions.map(_._2.end).max + 2
      }

      var continue = true
      if (p.overlaps(treeP)) {
        t match {
	  case Import(expr, selectors) => {
	    for(impSel <- selectors){
	      val start = impSel.namePos
	      val end = start + impSel.name.decode.length
	      addAt(start, end, 'importedName)
	    }
	  }
          case Ident(_) => {
            val sym = t.symbol
            if (sym.isCaseApplyOrUnapply) {
              val owner = sym.owner
              val start = treeP.start
              val end = start + owner.name.length
              addAt(start, end, 'caseApplyOrUnapply)
            } else if (sym.isConstructor) {
              add('constructor)
            } else if (sym.isTypeParameter) {
              add('typeParam)
            } else if (sym.isVariable && sym.isLocal) {
              add('var)
            } else if (sym.isValue && sym.isLocal) {
              add('val)
            } else if (sym.isVariable) {
              add('varField)
            } else if (sym.isValue) {
              add('valField)
            } else if (sym.isPackage) {
              add('package)
            } else if (sym.isClass) {
              add('class)
            } else if (sym.isModule) {
              add('object)
            } else {
              println("WTF ident: " + sym)
            }

          }
          case tree @ Select(qual, selector: Name) => {
            val sym = tree.symbol
            println(symbolSummary(sym).toString())
            val start = try {
              qual.pos.end + 1
            } catch {
              case _ => treeP.start
            }
            val len = selector.decode.length
            val end = start + len

            if (sym.isCaseApplyOrUnapply) {
              val owner = sym.owner
              val start = treeP.start
              val end = start + owner.name.length
              addAt(start, end, 'caseApplyOrUnapply)
              continue = false
            } else if (sym.hasAccessorFlag) {
              val under = sym.accessed
              if (under.isVariable) {
                addAt(start, end, 'varField)
              } else if (under.isValue) {
                addAt(start, end, 'valField)
              } else {
                println("WTF accessor: " + under)
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
              } 
	      else if(sym.nameString == "apply"){}
	      else {
                addAt(start, end, 'method)
              }
            } else if (sym.isPackage) {
              addAt(start, end, 'package)
            } else if (sym.isClass) {
              addAt(start, end, 'class)
            } else if (sym.isModule) {
              addAt(start, end, 'object)
            } else {
              addAt(start, end, 'method)
            }
          }
          case ValDef(mods, name, tpt, rhs) => {
            val start = nameStart(mods, t)
            val len = name.decode.length
            val end = start + len
            if (mods.isParameter) {
              addAt(start, end, 'param)
            } else if (mods.isMutable && mods.hasLocalFlag) {
              addAt(start, end, 'var)
            } else if (mods.hasLocalFlag) {
              addAt(start, end, 'val)
            } else if (mods.isMutable && !mods.hasLocalFlag) {
              addAt(start, end, 'varField)
            } else if (!mods.hasLocalFlag) {
              addAt(start, end, 'valField)
            }
          }
          case _ => {}
        }
        if (continue) super.traverse(t)
      }
    }
  }

  protected def symbolDesignationsInRegion(p: RangePosition): SymbolDesignations = {
    val traverser = new SymDesigsTraverser(p)
    val typed = new Response[Tree]
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
