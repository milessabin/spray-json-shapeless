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
import scala.collection.mutable.{ ListBuffer, LinkedHashSet }
import org.ensime.model.CompletionInfo

trait CompletionControl {
  self: RichPresentationCompiler =>

  def makeCompletions(
    prefix:String,
    sym: Symbol,
    tpe: Type,
    isMember:Boolean,
    isConstructor: Boolean,
    inherited: Boolean,
    viaView: Symbol): List[CompletionInfo] = {

    var score = 0
    if(sym.nameString.startsWith(prefix)) score += 10
    if(!inherited) score += 10
    if(!sym.isPackage) score += 10
    if(!sym.isType) score += 10
    if(sym.isLocal) score += 10
    if(sym.isPublic) score += 10
    if(viaView == NoSymbol) score += 10
    if(sym.owner != definitions.AnyClass &&
      sym.owner != definitions.AnyRefClass &&
      sym.owner != definitions.ObjectClass) score += 30

    if(isMember){
      List(CompletionInfo(sym, tpe, score))
    }
    else{
      if (isConstructor) {
	constructorSynonyms(sym).map{ 
	  c => CompletionInfo(sym, c.tpe, score) }
      } else {
	val applySyns = applySynonyms(sym).map{ 
	  c => CompletionInfo(sym, c.tpe, score) }
	List(CompletionInfo(sym, tpe, score)) ++ applySyns
      }
    }
  }

  def completionsAt(p: Position): List[CompletionInfo] = {

    def makeAll(
      x: Response[List[Member]],
      prefix: String,
      isConstructor: Boolean): List[CompletionInfo] = {
      val caseSense = prefix != prefix.toLowerCase()
      val buff = new LinkedHashSet[CompletionInfo]()
      do {
	for (members <- x.get.left.toOption) {
          askOption[Unit]{
            for (m <- filterMembersByPrefix(members, prefix, false, caseSense)) {
	      m match{
		case m @ ScopeMember(sym, tpe, true, viaView) => {
		  if(!sym.isConstructor){
		    buff ++= makeCompletions(prefix, sym, tpe, false, 
		      isConstructor, false, NoSymbol)
		  }
		}
		case m @ TypeMember(sym, tpe, true, inherited, viaView) => {
		  if(!sym.isConstructor){
                    buff ++= makeCompletions(prefix, sym, tpe, true, 
		      false, inherited, viaView)
		  }
		}
		case _ => 
              }
	    }
          }
	}
      } while (!x.isComplete)
      buff.toList
    }

    val results = completionContext(p) match {
      case Some(PackageContext(path, prefix)) => {
	askReloadFile(p.source)
	askCompletePackageMember(path, prefix)
      }
      case Some(SymbolContext(p, prefix, isConstructor)) => {
	askReloadFile(p.source)
	val x = new Response[List[Member]]
	askScopeCompletion(p, x)
	makeAll(x, prefix, isConstructor)
      }
      case Some(MemberContext(p, prefix)) => {
	askReloadFile(p.source)
	val x = new Response[List[Member]]
	askTypeCompletion(p, x)
	makeAll(x, prefix, false)
      }
      case _ => {
	System.err.println("Unrecognized completion context.")
	List()
      }
    }

    results.sortWith((c1,c2) => c1.relevance > c2.relevance)
  }

  private val ident = "[A-z0-9]"
  private val nonIdent = "[^A-z0-9]"
  private val ws = "[ \n\r\t]"

  trait CompletionContext {}

  private val packRE = "^.*?(?:package|import)[ ]+((?:[a-z0-9]+\\.)*)([A-z0-9]*)$".r
  case class PackageContext(path: String, prefix: String) extends CompletionContext
  def packageContext(preceding: String): Option[PackageContext] = {
    if (packRE.findFirstMatchIn(preceding).isDefined) {
      val m = packRE.findFirstMatchIn(preceding).get
      println("Matched package context: " + m.group(1) + "," + m.group(2))
      Some(PackageContext(m.group(1), m.group(2)))
    } else None
  }

  private val nameFollowingWhiteSpaceRE =
  List("^", ws, "*", "(", ident, "*)$").mkString.r

  private val nameFollowingReservedRE =
  List("(?:", nonIdent, "|", ws, ")",
    "(?:else|case|new|with|extends|yield|return|throw)",
    ws, "+",
    "(", ident, "*)$").mkString.r

  private val nameFollowingSyntaxRE =
  List("[!:=>\\(\\[,;\\}\\{\n+*/\\^&~%\\-]",
    ws, "*",
    "(", ident, "*)$").mkString.r

  case class SymbolContext(p: Position, prefix: String,
    isConstructor: Boolean) extends CompletionContext
  def symContext(p: Position, preceding: String): Option[SymbolContext] = {
    nameFollowingWhiteSpaceRE.findFirstMatchIn(preceding).orElse(
      nameFollowingSyntaxRE.findFirstMatchIn(preceding)).orElse(
      nameFollowingReservedRE.findFirstMatchIn(preceding)) match {
      case Some(m) => {
	println("Matched symbol context.")
	Some(SymbolContext(p, m.group(1), false))
      }
      case None => None
    }
  }

  private val constructorNameRE =
  List("(?:", nonIdent, "|", ws, ")",
    "(?:new)", ws, "+",
    "(", ident, "*)$").mkString.r

  def constructContext(p: Position, preceding: String): Option[SymbolContext] = {
    constructorNameRE.findFirstMatchIn(preceding) match {
      case Some(m) => {
	println("Matched constructor context.")
	Some(SymbolContext(p, m.group(1), true))
      }
      case None => None
    }
  }

  private def spliceSource(s: SourceFile, start: Int, end: Int,
    replacement: String): SourceFile = {
    new BatchSourceFile(s.file,
      Arrays.splice(s.content, start, end,
	replacement.toArray))
  }

  private val memberRE = "([\\. ]+)([^\\. ]*)$".r
  case class MemberContext(p: Position, prefix: String) extends CompletionContext
  def memberContext(p: Position, preceding: String): Option[MemberContext] = {
    memberRE.findFirstMatchIn(preceding) match {
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
	val newP = p.withSource(src, 0).withPoint(p.point - prefix.length - dot.length)

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

trait Completion { self: RichPresentationCompiler =>

  import self._

  def completePackageMember(path: String, prefix: String): List[CompletionInfo] = {
    packageSymFromPath(path) match {
      case Some(sym) => {
        val memberSyms = packageMembers(sym).filterNot { s =>
          s == NoSymbol || s.nameString.contains("$")
        }
        memberSyms.flatMap { s =>
          val name = if (s.isPackage) { s.nameString } else { typeShortName(s) }
          if (name.startsWith(prefix)) {
            Some(new CompletionInfo(name, "NA", -1, false, 20))
          } else None
        }.toList
      }
      case _ => List()
    }
  }

}

