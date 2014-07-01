package org.ensime.util

import scala.collection.immutable.Map
import scala.util.parsing.combinator._
import scala.util.parsing.input

abstract class SExp extends WireFormat {
  def toReadableString(debug: Boolean): String = toString
  override def toWireString: String = toReadableString(debug = false)
  def toScala: Any = toString
}

case class SExpList(items: Iterable[SExp]) extends SExp with Iterable[SExp] {

  override def iterator = items.iterator

  override def toString() = "(" + items.mkString(" ") + ")"

  override def toReadableString(debug: Boolean) = {
    "(" + items.map { _.toReadableString(debug) }.mkString(" ") + ")"
  }

  def toKeywordMap: Map[KeywordAtom, SExp] = {
    var m = Map[KeywordAtom, SExp]()
    items.sliding(2, 2).foreach {
      case (key: KeywordAtom) :: (sexp: SExp) :: rest =>
        m += (key -> sexp)
      case _ =>
    }
    m
  }

  def toSymbolMap: Map[scala.Symbol, Any] = {
    var m = Map[scala.Symbol, Any]()
    items.sliding(2, 2).foreach {
      case SymbolAtom(key) :: (sexp: SExp) :: rest =>
        m += (Symbol(key) -> sexp.toScala)
      case _ =>
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
  override def toReadableString(debug: Boolean) = {
    if (debug && value.length() > 500) {
      escape_string(value.substring(0, 500) + "...")
    } else {
      escape_string(value)
    }
  }
  def escape_string(s: String) = {
    val printable = s.replace("\\", "\\\\").replace("\"", "\\\"")
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

object StringParser extends RegexParsers {
  override def skipWhitespace = false

  def string = "\"" ~ rep(string_char) ~ "\"" ^^ {
    case "\"" ~ l ~ "\"" => l.mkString
  }
  lazy val string_char = string_escaped_char | string_lone_backslash | string_normal_chars
  lazy val string_escaped_char = """\\["\\]""".r ^^ { _.charAt(1) }
  lazy val string_lone_backslash = "\\"
  lazy val string_normal_chars = """[^"\\]+""".r ^^ { _.mkString }
}

object SExp extends RegexParsers {

  // Parse strings using an auxiliary parser. This is because
  // spaces inside strings shouldn't be skipped.
  lazy val string = new Parser[StringAtom] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      StringParser.string(in.drop(start - offset)) match {
        case StringParser.Success(value, next) => Success(StringAtom(value), next)
        case StringParser.Failure(errMsg, next) => Failure(errMsg, next)
        case StringParser.Error(errMsg, next) => Error(errMsg, next)
      }
    }
  }
  lazy val sym = regex("[a-zA-Z][a-zA-Z0-9-:]*".r) ^^ { s =>
    if (s == "nil") NilAtom()
    else if (s == "t") TruthAtom()
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
      case Failure(errMsg, next) =>
        println(errMsg)
        NilAtom()
      case Error(errMsg, next) =>
        println(errMsg)
        NilAtom()
    }
  }

  def read(s: String): SExp = {
    SExp.read(new input.CharSequenceReader(s))
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

  implicit def longToSExp(value: Long): SExp = {
    IntAtom(value.toInt)
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
      SymbolAtom(value.toString().drop(1))
    }
  }

  implicit def nilToSExpList(nil: NilAtom): SExp = {
    SExpList(List())
  }

  implicit def toSExp(o: SExpable): SExp = {
    o.toSExp()
  }

  implicit def toSExpable(o: SExp): SExpable = new SExpable {
    override def toSExp = o
  }

  implicit def listToSExpable(o: Iterable[SExpable]): SExpable =
    new Iterable[SExpable] with SExpable {
      override def iterator = o.iterator
      override def toSExp = SExp(o.map { _.toSExp })
    }

}

trait SExpable {
  implicit def toSExp(): SExp
}
