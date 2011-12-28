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
import scala.tools.nsc.util.{ Position, RangePosition, SourceFile, BatchSourceFile }
import org.ensime.util.Arrays
import scala.tools.nsc.interactive.{ Response, CompilerControl, Global }
import scala.collection.mutable.{ListBuffer, LinkedHashSet}
import org.ensime.model.SymbolInfoLight

trait CompletionControl {
  self: RichPresentationCompiler =>
  def makeSymbolCompletions(x: Response[List[Member]], 
    prefix:String, 
    constructor: Boolean): List[SymbolInfoLight] = {
    val caseSense = prefix != prefix.toLowerCase()
    val buff = new LinkedHashSet[SymbolInfoLight]
    do{
      for (members <- x.get.left.toOption) {
	askOption { 
	  for (m <- filterMembersByPrefix(members, prefix, false, caseSense)) {
	    m match {
              case m@ScopeMember(sym, tpe, true, viaView) if !sym.isConstructor => {
		if (constructor) {
		  buff ++= SymbolInfoLight.constructorSynonyms(sym)
		} else {
		  buff += SymbolInfoLight(sym, tpe)
		  buff ++= SymbolInfoLight.applySynonyms(sym)
		}
	      }
	      case _ => 
	    }
	  }
	}
      }
    } while(!x.isComplete)
    buff.toList
  }

  def makeTypeMemberCompletions(x: Response[List[Member]], prefix:String): List[SymbolInfoLight] = {
    val caseSense = prefix != prefix.toLowerCase()
    val buff = new LinkedHashSet[SymbolInfoLight]
    do{
      for (members <- x.get.left.toOption) {
	askOption { 
	  for (m <- filterMembersByPrefix(members, prefix, false, caseSense)) {
	    m match {
	      case m@TypeMember(sym, tpe, true, _, _) if !sym.isConstructor => {
		buff += SymbolInfoLight(sym, tpe)
	      }
	      case _ => 
	    }
	  }
	}
      }
    } while(!x.isComplete)
    buff.toList
  }

  def completionsAt(p:Position):List[SymbolInfoLight] = {
    val results = completionContext(p) match {
      case Some(PackageContext(path, prefix)) => {
	askReloadFile(p.source)
	askCompletePackageMember(path, prefix)
      }
      case Some(SymbolContext(p, prefix, isConstructor)) => {
	askReloadFile(p.source)
	val x = new Response[List[Member]]
	askScopeCompletion(p, x)
	makeSymbolCompletions(x, prefix, isConstructor)
      }
      case Some(MemberContext(p, prefix)) => {
	askReloadFile(p.source)
	val x = new Response[List[Member]]
	askTypeCompletion(p, x)
	makeTypeMemberCompletions(x, prefix)
      }
      case _ => {
	System.err.println("Unrecognized completion context.")
	List()
      }
    }

    results
  }


  private val ident = "[A-z0-9]"
  private val nonIdent = "[^A-z0-9]"
  private val ws = "[ \n\r\t]"

  trait CompletionContext{}

  private val packRE = "^.*?(?:package|import)[ ]+((?:[a-z0-9]+\\.)*)([A-z0-9]*)$".r
  case class PackageContext(path:String, prefix:String) extends CompletionContext
  def packageContext(preceding:String):Option[PackageContext] = {
    if(packRE.findFirstMatchIn(preceding).isDefined){
      val m = packRE.findFirstMatchIn(preceding).get
      println("Matched package context: " + m.group(1) + "," + m.group(2))
      Some(PackageContext(m.group(1), m.group(2)))
    }    
    else None
  }

  private val nameFollowingWhiteSpaceRE = 
  List("^",ws,"*","(",ident,"*)$").mkString.r

  private val nameFollowingReservedRE = 
  List("(?:",nonIdent,"|",ws,")",
    "(?:else|case|new|with|extends|yield|return|throw)",
    ws,"+",
    "(",ident,"*)$").mkString.r

  private val nameFollowingSyntaxRE = 
  List("[!:=>\\(\\[,;\\}\\{\n+*/\\^&~%\\-]",
    ws,"*",
    "(",ident,"*)$").mkString.r


  case class SymbolContext(p:Position, prefix:String,
    isConstructor:Boolean) extends CompletionContext
  def symContext(p:Position, preceding:String):Option[SymbolContext] = {
    nameFollowingWhiteSpaceRE.findFirstMatchIn(preceding).orElse(
      nameFollowingSyntaxRE.findFirstMatchIn(preceding)).orElse(
      nameFollowingReservedRE.findFirstMatchIn(preceding)) match{
      case Some(m) => {
	println("Matched symbol context.")
	Some(SymbolContext(p, m.group(1), false))
      }
      case None => None
    }
  }

  private val constructorNameRE = 
  List("(?:",nonIdent,"|",ws,")",
    "(?:new)", ws,"+",
    "(",ident,"*)$").mkString.r

  def constructContext(p:Position, preceding:String):Option[SymbolContext] = {
    constructorNameRE.findFirstMatchIn(preceding) match{
      case Some(m) => {
	println("Matched constructor context.")
	Some(SymbolContext(p, m.group(1), true))
      }
      case None => None
    }
  }

  private def spliceSource(s:SourceFile, start:Int, end:Int, 
    replacement:String):SourceFile = {
    new BatchSourceFile(s.file, 
      Arrays.splice(s.content, start, end, 
	replacement.toArray))
  }


  private val memberRE = "([\\. ]+)([^\\. ]*)$".r
  case class MemberContext(p:Position, prefix:String) extends CompletionContext
  def memberContext(p:Position, preceding:String):Option[MemberContext] = {
    memberRE.findFirstMatchIn(preceding) match{
      case Some(m) => {
	println("Matched member context.")
	val dot = m.group(1)
	val prefix = m.group(2)

	// Replace prefix with ' ()' so parser doesn't
	// pick up next line as a continuation of current.
	val src = spliceSource(p.source, 
	  p.point - prefix.length,
	  p.point - 1,
	  " ()")

	// Move point back to target of method selection.
	val newP = p.withSource(src,0).withPoint(p.point - prefix.length - dot.length)

	Some(MemberContext(newP, prefix))
      }
      case None => None
    }
  }

  protected def completionContext(p: Position): Option[CompletionContext] = {
    val src = p.source
    val lineNum = src.offsetToLine(p.point - 1)
    val bol = src.lineToOffset(lineNum)
    val line = src.lineToString(lineNum)
    val preceding = line.take(p.point - bol)
    println("Line: " + line)
    println("Preceding: " + preceding)
    packageContext(preceding).
    orElse(constructContext(p, preceding)).
    orElse(symContext(p, preceding)).
    orElse(memberContext(p, preceding))
  }

}


trait Completion{ self: RichPresentationCompiler =>

  import self._

  def completePackageMember(path: String, prefix: String): List[SymbolInfoLight] = {
    packageSymFromPath(path) match {
      case Some(sym) => {
	val memberSyms = packageMembers(sym).filterNot { s =>
          s == NoSymbol || s.nameString.contains("$")
	}
	memberSyms.flatMap { s =>
          val name = if (s.isPackage) { s.nameString } else { typeShortName(s) }
          if (name.startsWith(prefix)) {
            Some(new SymbolInfoLight(name, "NA", -1, false))
          } else None
	}.toList
      }
      case _ => List()
    }
  }

  //  def scopeMembers(pos: Position, prefix: String, matchEntire: Boolean, caseSens: Boolean): List[ScopeMember] = {
  //    scopeMembers(pos) filter { sym =>
  //   wrapTypedTreeAt(pos) // to make sure context is entered
  //   locateContext(pos) match {
  //     case Some(context) => {
  //       val locals = new mutable.LinkedHashMap[Symbol, ScopeMember]
  //       val prefixUpper = prefix.toUpperCase()
  //       def addSymbol(sym: Symbol, pre: Type, viaImport: Tree) = {
  //         try {
  //           val ns = sym.nameString
  //           val accessible = context.isAccessible(sym, pre, false)
  //           if (accessible && ((matchEntire && ns == prefix) ||
  // 	      (!matchEntire && caseSens && ns.startsWith(prefix)) ||
  // 	      (!matchEntire && !caseSens && ns.toUpperCase().startsWith(prefixUpper)))
  //             && !sym.nameString.contains("$") && !locals.contains(sym)) {
  //             val member = new ScopeMember(
  //               sym,
  //               sym.tpe,
  //               accessible,
  //               viaImport)
  //             locals(sym) = member
  //           }
  //         } catch {
  //           case e: Exception => {
  //           System.err.println("Error: Omitting scope member.")
  //           e.printStackTrace()
  //         }
  //       }
  //     }
  //     var cx = context
  //     while (cx != NoContext) {
  //       for (sym <- cx.scope) {
  //         addSymbol(sym, NoPrefix, EmptyTree)
  //       }
  //       if (cx.prefix != null) {
  //         for (sym <- cx.prefix.members) {
  //           addSymbol(sym, cx.prefix, EmptyTree)
  //         }
  //       }
  //       cx = cx.outer
  //     }
  //     for (imp <- context.imports) {
  //       val pre = imp.qual.tpe
  //       val importedSyms = pre.members.flatMap(transformImport(
  //           imp.tree.selectors, _))
  //       for (sym <- importedSyms) {
  //         addSymbol(sym, pre, imp.qual)
  //       }
  //     }
  //     val result = locals.values.toList
  //     result
  //   }
  //   case _ => List()
  // }
  // }
  
  //    }
  //  }

}





