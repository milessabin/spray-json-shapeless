package org.ensime.util

import org.slf4j.LoggerFactory

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input

object SExpParser extends RegexParsers {

  val log = LoggerFactory.getLogger(SExpParser.getClass)

  object StringParser extends RegexParsers {
    override def skipWhitespace = false

    def string = "\"" ~ rep(string_char) ~ "\"" ^^ {
      case "\"" ~ l ~ "\"" => l.mkString
    }

    lazy val string_char = string_escaped_char | string_lone_backslash | string_normal_chars
    lazy val string_escaped_char = """\\["\\ntr]""".r ^^ {
      _.charAt(1) match {
        case 'n' => '\n'
        case 't' => '\t'
        case 'r' => '\r'
        case x => x
      }
    }
    lazy val string_lone_backslash = "\\"
    lazy val string_normal_chars = """[^"\\]+""".r ^^ {
      _.mkString
    }
  }

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
    if (s == "nil") NilAtom
    else if (s == "t") TruthAtom
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
        log.error("Failure whilst reading SExp: " + errMsg)
        NilAtom
      case Error(errMsg, next) =>
        log.error("Error whilst reading SExp: " + errMsg)
        NilAtom
    }
  }

  def read(s: String): SExp = {
    read(new input.CharSequenceReader(s))
  }

}
