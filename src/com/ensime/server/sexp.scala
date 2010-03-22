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

  lazy val string = regex("""\"([^\"\\]|\\.)*\"""".r) ^^ StringAtom
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
  
  def main(args: Array[String]) {
    println(read(new CharArrayReader(args(0).toCharArray())))
  }

}



