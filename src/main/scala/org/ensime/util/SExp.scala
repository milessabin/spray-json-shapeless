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

package org.ensime.util
import scala.collection.immutable.Map
import scala.util.parsing.combinator._
import scala.util.parsing.input

abstract class SExp extends WireFormat {
  def toReadableString: String = toString
  override def toWireString: String = toReadableString
  def toScala: Any = toString
}

case class SExpList(items: Iterable[SExp]) extends SExp with Iterable[SExp] {

  override def iterator = items.iterator

  override def toString = "(" + items.mkString(" ") + ")"

  override def toReadableString = {
    "(" + items.map { _.toReadableString }.mkString(" ") + ")"
  }

  def toKeywordMap(): Map[KeywordAtom, SExp] = {
    var m = Map[KeywordAtom, SExp]()
    items.sliding(2, 2).foreach {
      case (key: KeywordAtom) ::(sexp: SExp) :: rest => {
        m += (key -> sexp)
      }
      case _ => {}
    }
    m
  }

  def toSymbolMap(): Map[scala.Symbol, Any] = {
    var m = Map[scala.Symbol, Any]()
    items.sliding(2, 2).foreach {
      case SymbolAtom(key) ::(sexp: SExp) :: rest => {
        m += (Symbol(key) -> sexp.toScala)
      }
      case _ => {}
    }
    m
  }
}


object BooleanAtom {

  def unapply(z: SExp): Option[Boolean] = z match {
    case TruthAtom() => Some(true)
    case NilAtom() => Some(false)
    case _ => None
  }

}

abstract class BooleanAtom extends SExp {
  def toBool: Boolean
  override def toScala = toBool
}

case class NilAtom() extends BooleanAtom {
  override def toString = "nil"
  override def toBool: Boolean = false

}
case class TruthAtom() extends BooleanAtom {
  override def toString = "t"
  override def toBool: Boolean = true
  override def toScala: Boolean = true
}
case class StringAtom(value: String) extends SExp {
  override def toString = value
  override def toReadableString = {
    val printable = value.replace("\\", "\\\\").replace("\"", "\\\"");
    "\"" + printable + "\""
  }
}
case class IntAtom(value: Int) extends SExp {
  override def toString = String.valueOf(value)
  override def toScala = value
}
case class SymbolAtom(value: String) extends SExp {
  override def toString = value
}
case class KeywordAtom(value: String) extends SExp {
  override def toString = value
}

object SExp extends RegexParsers {

  import scala.util.matching.Regex

  lazy val string = regexGroups("""\"((?:[^\"\\]|\\.)*)\"""".r) ^^ { m => 
    StringAtom(m.group(1).replace("\\\\", "\\")) 
  }
  lazy val sym = regex("[a-zA-Z][a-zA-Z0-9-:]*".r) ^^ { s => 
    if(s == "nil") NilAtom()
    else if(s == "t") TruthAtom()
    else SymbolAtom(s)
  }
  lazy val keyword = regex(":[a-zA-Z][a-zA-Z0-9-:]*".r) ^^ KeywordAtom
  lazy val number = regex("-?[0-9]+".r) ^^ { s => IntAtom(s.toInt) }
  lazy val list = literal("(") ~> rep(expr) <~ literal(")") ^^ SExpList.apply
  lazy val expr: Parser[SExp] = list | keyword | string | number | sym

  def read(r: input.Reader[Char]): SExp = {
    val result: ParseResult[SExp] = expr(r)
    result match {
      case Success(value, next) => value
      case Failure(errMsg, next) => {
        println(errMsg)
        NilAtom()
      }
      case Error(errMsg, next) => {
        println(errMsg)
        NilAtom()
      }
    }
  }

  /** A parser that matches a regex string and returns the match groups */
  def regexGroups(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) => Success(matched, in.drop(start + matched.end - offset))
        case None =>
        Failure("string matching regex `" + r +
          "' expected but `" +
          in.first + "' found", in.drop(start - offset))
      }
    }
  }

  def apply(items: SExp*): SExpList = {
    SExpList(items)
  }

  def apply(items: Iterable[SExp]): SExpList = {
    SExpList(items)
  }

  // Helpers for common case of key,val prop-list.
  // Omit keys for nil values.
  def propList(items: (String, SExp)*): SExpList = {
    propList(items)
  }
  def propList(items: Iterable[(String, SExp)]): SExpList = {
    val nonNil = items.filter {
      case (s, NilAtom()) => false
      case (s, SExpList(items)) if items.isEmpty => false
      case _ => true
    }
    SExpList(nonNil.flatMap(ea => List(key(ea._1), ea._2)))
  }

  implicit def strToSExp(str: String): SExp = {
    StringAtom(str)
  }

  def key(str: String): KeywordAtom = {
    KeywordAtom(str)
  }

  implicit def intToSExp(value: Int): SExp = {
    IntAtom(value)
  }

  implicit def boolToSExp(value: Boolean): SExp = {
    if (value) {
      TruthAtom()
    } else {
      NilAtom()
    }
  }

  implicit def symbolToSExp(value: Symbol): SExp = {
    if (value == 'nil) {
      NilAtom()
    } else {
      SymbolAtom(value.toString.drop(1))
    }
  }

  implicit def nilToSExpList(nil: NilAtom): SExp = {
    SExpList(List())
  }

  implicit def toSExp(o: SExpable): SExp = {
    o.toSExp
  }

  implicit def toSExpable(o: SExp): SExpable = new SExpable {
    def toSExp = o
  }

  implicit def listToSExpable(o: Iterable[SExpable]): SExpable = new Iterable[SExpable] with SExpable {
    override def iterator = o.iterator
    override def toSExp = SExp(o.map { _.toSExp })
  }


  def main(args:Array[String]){
    def readStr(s:String) = {
      val chars = new Array[Char](s.length)
      s.getChars(0, s.length, chars, 0)
      val r = new input.CharArrayReader(chars)
      SExp.read(r)      
    }
    def check(s:String, r:String) {
      assert(readStr(s).toString() == r, "Failed at: " + s)
    }
    check("()", "()")
    check("(nil)", "(nil)")
    check("(t)", "(t)")
    check("(a b c d)", "(a b c d)")
    check("(a b c () nil)", "(a b c () nil)")
    check("(a b c () trait)", "(a b c () trait)")
    check("(a b c () t())", "(a b c () t ())")
    check("(a b c\n() nil(nil\n t))", "(a b c () nil (nil t))")
    check("(nildude)", "(nildude)")


    assert(readStr("t\n").toScala == true, "t should be true!")
    assert(readStr("t\n\t").toScala == true, "t should be true!")
    assert(readStr("t\n\t").toScala == true, "t should be true!")
    assert(readStr("t ").toScala == true, "t should be true!")
    assert(readStr("t").toScala == true, "t should be true!")


    assert(readStr("nil\n").toScala == false, "nil should be false!")
    assert(readStr("nil\n\t").toScala == false, "nil should be false!")
    assert(readStr("nil\n\t").toScala == false, "nil should be false!")
    assert(readStr("nil ").toScala == false, "nil should be false!")
    assert(readStr("nil").toScala == false, "nil should be false!")

    val map = readStr("(:use-sbt t :dude 1212)") match{
      case ls:SExpList => ls.toKeywordMap()
    }
    map.get(key(":use-sbt")) match{
      case Some(v) => assert(v.toScala == true)
      case None => assert(false)
    }

  }

}

abstract trait SExpable {
  implicit def toSExp(): SExp
}

