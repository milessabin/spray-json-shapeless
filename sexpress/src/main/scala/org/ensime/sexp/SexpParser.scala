package org.ensime.sexp

import org.parboiled.scala._

import org.ensime.sexp.util.ParboiledParser

/**
 * Parse Emacs Lisp into an `Sexp`. Other lisp variants may
 * require tweaking, e.g. Scheme's nil, infinity, NaN, etc.
 */
object SexpParser extends ParboiledParser[Sexp] {
  protected val Top = SexpP

  // e.g. for .el files
  def flatParse(el: String): Sexp = parse("(" + el + "\n)")

  private lazy val SexpP: Rule1[Sexp] = rule("Sexp") {
    SexpAtomP | SexpListP | SexpConsP | SexpQuotedP
  }

  private lazy val SexpAtomP: Rule1[SexpAtom] = rule("Atom") {
    SexpNilP | SexpCharP | SexpStringP | SexpNaNP | SexpNumberP | SexpSymbolP
  }

  private lazy val SexpNilP: Rule1[SexpAtom] = rule("nil") {
    "nil" | (LeftBrace ~ RightBrace)
  } ~> { _ => SexpNil }

  private lazy val SexpCharP: Rule1[SexpChar] = rule("Char") {
    ch('?') ~ NormalChar
  } ~~> { c => SexpChar(c.head) }

  private lazy val SexpStringP: Rule1[SexpString] = rule("String") {
    ch('"') ~ zeroOrMore(Character) ~~> (chars => SexpString(chars.mkString(""))) ~ ch('"')
  }

  private lazy val SexpNumberP: Rule1[SexpNumber] = rule("Number") {
    group(Integer ~ optional(Frac) ~ optional(Exp))
  } ~> {
    value => SexpNumber(BigDecimal(value))
  }

  private lazy val SexpNaNP: Rule1[SexpAtom] = rule("NaN") {
    ("-1.0e+INF" ~> { _ => SexpNegInf }) |
      ("1.0e+INF" ~> { _ => SexpPosInf }) |
      (optional("-") ~ "0.0e+NaN" ~> { _ => SexpNaN })
  }

  private lazy val SexpSymbolP: Rule1[SexpSymbol] = rule("Symbol") {
    // ? allowed at the end of symbol names
    oneOrMore(Alpha | Digit | SymbolSpecial) ~ zeroOrMore(Alpha | Digit | SymbolSpecial | ".") ~ optional("?")
  } ~> SexpSymbol.apply

  private lazy val SexpListP: Rule1[Sexp] = rule("List") {
    LeftBrace ~ (SexpP ~ zeroOrMore(Whitespace ~ SexpP)) ~ RightBrace
  } ~~> {
    (head, tail) => SexpList(head :: tail)
  }

  private lazy val SexpConsP: Rule1[SexpCons] = rule("Cons") {
    LeftBrace ~
      SexpP ~ Whitespace ~ "." ~ Whitespace ~ SexpP ~ RightBrace
  } ~~> {
    (x, y) => SexpCons(x, y)
  }

  private val SexpQuote = SexpSymbol("quote")
  private lazy val SexpQuotedP: Rule1[Sexp] = rule("Quoted") {
    "'" ~ SexpP
  } ~~> { v => SexpCons(SexpQuote, v) }

  private lazy val Character: Rule1[String] = rule("Character") { EscapedChar | NormalChar }
  private lazy val EscapedChar: Rule1[String] = rule("EscapedChar") {
    "\\" ~ ANY ~> { s => unescape(s) }
  }
  private lazy val NormalChar: Rule1[String] = rule("NormalChar") { !anyOf("\"\\") ~ ANY ~> identity }

  // Rule0 primitives and helpers...
  private lazy val Alpha = rule("Alpha") { "a" - "z" | "A" - "Z" }
  private lazy val Integer = rule("Integer") { optional("-") ~ (("1" - "9") ~ Digits | Digit) }
  private lazy val Digits = rule("Digits") { oneOrMore(Digit) }
  private lazy val Digit = rule("Digit") { "0" - "9" }
  private lazy val Frac = rule("Frac") { "." ~ Digits }
  private lazy val Exp = rule("Exp") { ignoreCase("e") ~ optional(anyOf("+-")) ~ Digits }

  private lazy val SymbolSpecial = rule("SymbolSpecial") { anyOf("+-*/_~!@$%^&=:<>{}") }

  private lazy val Whitespace = rule("Whitespace") { zeroOrMore(Comment | anyOf(" \n\r\t\f")) }
  private lazy val Comment = rule("Comment") { ";" ~ zeroOrMore(noneOf("\n")) ~ ("\n" | EOI) }

  private lazy val LeftBrace = rule("(") { Whitespace ~ "(" ~ Whitespace }
  private lazy val RightBrace = rule(")") { Whitespace ~ ")" ~ Whitespace }

  // https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html
  // https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-for-Strings.html
  // Not supported: https://www.gnu.org/software/emacs/manual/html_node/elisp/Non_002dASCII-in-Strings.html
  private[sexp] val specialChars = Map[String, String](
    "\"" -> "\"",
    "a" -> 7.toChar.toString,
    "b" -> "\b",
    "t" -> "\t",
    "n" -> "\n",
    "v" -> 11.toChar.toString,
    "f" -> "\f",
    "r" -> "\r",
    "e" -> 27.toChar.toString,
    "s" -> " ",
    "d" -> 127.toChar.toString,
    "\\" -> "\\"
  )
  private val ignore = Set("\n", " ")

  private def unescape(c: String): String = {
    if (ignore(c)) ""
    else {
      val unescaped = specialChars.get(c)
      require(unescaped.isDefined, c + " is not a valid escaped character")
      unescaped.get
    }
  }
}
