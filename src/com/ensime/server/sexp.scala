package com.ensime.server

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input._
import scala.collection.immutable.Map


abstract class SExp
case class SExpList(items:Iterable[SExp]) extends SExp with Iterable[SExp]{
  override def iterator = items.iterator
  override def foreach[U](f: SExp => U) = items.foreach(f)
  override def toString = "(" + items.mkString(" ") + ")"
  def toSExpMap():Map[SExp, SExp] = {
    var m = Map[SExp, SExp]()
    var l = items
    while(l.size > 1){
      m += l.head -> l.drop(1).head
      l = l.drop(2)
    }
    m
  }
}
case class NilAtom() extends SExp{
  override def toString = "nil"
}
case class TruthAtom() extends SExp{
  override def toString = "t"
}
case class StringAtom(value:String) extends SExp{
  override def toString = {
    val printable = value.replace("\\", "\\\\").replace("\"", "\\\"");
    "\"" + printable + "\""
  }
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
  lazy val list = literal("(") ~> rep(expr) <~ literal(")") ^^ SExpList.apply
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

  def apply(items:SExp *):SExpList = {
    SExpList(items)
  }

  def apply(items:Iterable[SExp]):SExpList = {
    SExpList(items)
  }

  implicit def strToSExp(str:String):SExp = {
    StringAtom(str)
  }

  def key(str:String):SExp = {
    KeywordAtom(str)
  }

  implicit def intToSExp(value:Int):SExp = {
    IntAtom(value)
  }

  implicit def boolToSExp(value:Boolean):SExp = {
    if(value){
      TruthAtom()
    }
    else{
      NilAtom()
    }
  }

  implicit def symbolToSExp(value:Symbol):SExp = {
    if(value == 'nil){
      NilAtom()
    }
    else{
      SymbolAtom(value.toString.drop(1))
    }
  }
  
  def main(args: Array[String]) {
    println(read(new CharArrayReader(args(0).toCharArray())))
  }

}



