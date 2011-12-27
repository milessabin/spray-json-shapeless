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

  private val packRE = "^.*?(?:package|import)[ ]+((?:[a-z0-9]+\\.)*)([A-z0-9]*)$".r
  case class PackageContext(path:String, prefix:String)
  def packageContext(preceding:String):Option[PackageContext] = {
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
  

  case class SymbolContext(p:Position, prefix:String, isConstructor:Boolean)
  def symContext(p:Position, preceding:String):Option[SymbolContext] = {
    nameFollowingSyntaxRE.findFirstMatchIn(preceding).orElse(
      nameFollowingKeywordRE.findFirstMatchIn(preceding)) match{
      case Some(m) => Some(SymbolContext(p, m.group(1), false))
      case None => None
    }
  }

  private val memberRE = "[\\. ]+([^\\. ]*)?$".r
  case class MemberContext(p:Position, prefix:String)
  def memberContext(p:Position, preceding:String):Option[MemberContext] = {
    memberRE.findFirstMatchIn(preceding) match{
      case Some(m) => Some(MemberContext(p, m.group(1)))
      case None => None
    }
    None
  }

  protected def completionContext(p: Position): Option[Any] = {
    val src = p.source
    val lineNum = src.offsetToLine(p.point - 1)
    val bol = src.lineToOffset(lineNum)
    val line = src.lineToString(lineNum)
    val preceding = line.take(p.point - bol)
    println("Line: " + line)
    println("Preceding: " + preceding)
    packageContext(preceding).
    orElse(symContext(p, preceding)).
    orElse(memberContext(p, preceding))
  }

}
