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
import scala.tools.nsc.util.{ Position, RangePosition, SourceFile }

trait CompletionContexts {

  private val ident = "[A-z0-9]"
  private val nonIdent = "[^A-z0-9]"
  private val ws = "[ \n\r\t]"

  trait CompletionContext{}

  private val packRE = "^.*?(?:package|import)[ ]+((?:[a-z0-9]+\\.)*)([A-z0-9]*)$".r
  case class PackageContext(path:String, prefix:String) extends CompletionContext
  def packageContext(preceding:String):Option[PackageContext] = {

    //		   (backward-delete-char (length full-match))
    //		   (insert "object ensimesynthetic${")
    //
    //		   (if (eq (length path) 0)
    //
    //		       (progn
    //			 (save-excursion
    //			   (insert "  }"))
    //			 (ensime-write-buffer)
    //			 (ensime-rpc-name-completions-at-point prefix))
    //
    //		     (progn
    //		       (insert path)
    //		       (save-excursion
    //			 (insert "  }"))
    //		       (backward-char 2)
    //		       (ensime-write-buffer)
    //		       (ensime-rpc-members-for-type-at-point prefix))))))
    if(packRE.findFirstMatchIn(preceding).isDefined){
      val m = packRE.findFirstMatchIn(preceding).get
      println("Matched package: " + m.group(1) + "," + m.group(2))
      Some(PackageContext(m.group(1), m.group(2)))
    }    
    else None
  }

  private val nameFollowingKeywordRE = 
  List("(?:",nonIdent,"|",ws,")",
    "(?:else|case|new|with|extends|yield)",
    ws,"+",
    "(",ident,"*)$").mkString.r

  private val nameFollowingSyntaxRE = 
  List("[!:=>\\(\\[,;\\}\\{\n+*/\\^&~%\\-]",
    ws,"*",
    "(",ident,"*)$").mkString.r
  

  case class SymbolContext(p:Position, prefix:String, 
    isConstructor:Boolean) extends CompletionContext
  def symContext(p:Position, preceding:String):Option[SymbolContext] = {

    //	    (backward-delete-char (length prefix))
    //	    (insert ";{")
    //	    (save-excursion
    //	      ;; Insert a dummy value after (point), so that
    //	      ;; if we are at the end of a method body, the
    //	      ;; method context will be extended to include
    //	      ;; the completion point.
    //	      (insert "  ;exit()};"))
    //	    (ensime-write-buffer)
    //	    (ensime-rpc-name-completions-at-point
    //	     prefix is-constructor))))

    nameFollowingSyntaxRE.findFirstMatchIn(preceding).orElse(
      nameFollowingKeywordRE.findFirstMatchIn(preceding)) match{
      case Some(m) => Some(SymbolContext(p, m.group(1), false))
      case None => None
    }
  }

  private val constructorNameRE = 
  List("(?:",nonIdent,"|",ws,")",
    "(?:new)", ws,"+",
    "(",ident,"*)$").mkString.r

  def constructContext(p:Position, preceding:String):Option[SymbolContext] = {
    constructorNameRE.findFirstMatchIn(preceding) match{
      case Some(m) => Some(SymbolContext(p, m.group(1), true))
      case None => None
    }
  }


  private val memberRE = "([\\. ]+)([^\\. ]*)$".r
  case class MemberContext(p:Position, prefix:String) extends CompletionContext
  def memberContext(p:Position, preceding:String):Option[MemberContext] = {

    //	  (ensime-ac-delete-text-back-to-call-target)
    //
    //	  ;; Add a trailing '.' so singleton object accesses parse correctly
    //	  ;; Move cursor forward so it will be on '.'
    //	  (forward-char)
    //	  (save-excursion
    //	    (insert ". ()"))
    memberRE.findFirstMatchIn(preceding) match{
      case Some(m) => {
	val dot = m.group(1)
	val prefix = m.group(2)
	val ownerP = p.withPoint(p.point - prefix.length - dot.length)
	Some(MemberContext(ownerP, prefix))
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
