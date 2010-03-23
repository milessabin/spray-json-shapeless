package com.ensime.server

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input._

abstract class SExp
case class SExpList(items:List[SExp]) extends SExp{
  override def toString = "(" + items.mkString(" ") + ")"
}
case class NilAtom() extends SExp{
  override def toString = "nil"
}
case class TruthAtom() extends SExp{
  override def toString = "t"
}
case class StringAtom(value:String) extends SExp{
  override def toString = "\"" + value + "\""
}
case class IntAtom(value:Int) extends SExp{
  override def toString = String.valueOf(value)
}
case class SymbolAtom(value:String) extends SExp{
  override def toString = value
}
case class KeywordAtom(value:String) extends SExp{
  override def toString = value
}



object SExp extends RegexParsers{

  import scala.util.matching.Regex

  lazy val string = regexGroups("""\"((?:[^\"\\]|\\.)*)\"""".r) ^^ {m => StringAtom(m.group(1))}
  lazy val sym = regex("[a-zA-Z][a-zA-Z0-9-:]*".r) ^^ SymbolAtom
  lazy val keyword = regex(":[a-zA-Z][a-zA-Z0-9-:]*".r) ^^ KeywordAtom
  lazy val number = regex("[0-9]+".r) ^^ {cs => IntAtom(cs.toInt)}
  lazy val list = literal("(") ~> rep(expr) <~ literal(")") ^^ SExpList
  lazy val nil = literal("nil") ^^ { cs => NilAtom() }
  lazy val truth = literal("t") ^^ { cs => TruthAtom() }
  lazy val expr: Parser[SExp] = list | nil | truth | keyword | sym | number | string

  def read(r:Reader[Char]):SExp = {
    val result:ParseResult[SExp] = expr(r)
    result match{
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
        case Some(matched) =>
        Success(matched,
          in.drop(start + matched.end - offset))
        case None =>
        Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }

  
  def main(args: Array[String]) {
    println(read(new CharArrayReader(args(0).toCharArray())))
  }

}



