package org.ensime.sexp

/**
 * Emacs flavoured lisp printer.
 */
trait SexpPrinter extends (Sexp => String) {
  /**
   * Convert the input to a `String` (constructing the entire `String`
   * in memory).
   */
  def apply(x: Sexp): String = {
    val sb = new StringBuilder
    print(x, sb)
    sb.toString()
  }

  /**
   * Convert the input to a `String` with a swank message title.
   */
  def apply(x: Sexp, swank: String, rpc: Long): String = {
    val sb = new StringBuilder
    print(x, swank, rpc, sb)
    sb.toString()
  }

  /**
   * Perform the side effect of rendering `x` to `sb`, wrapping in a
   * swank message title and RPC id.
   */
  def print(x: Sexp, swank: String, rpc: Long, sb: StringBuilder): Unit = {
    sb.append('(').append(swank).append(' ')
    print(x, sb)
    sb.append(' ').append(rpc).append(')')
  }

  /**
   * Perform the side effect of rendering `x` to `sb`, which may
   * reduce memory requirements when rendering very large objects.
   */
  def print(x: Sexp, sb: StringBuilder): Unit

  protected def printAtom(sexp: SexpAtom, sb: StringBuilder): Unit = sexp match {
    case SexpChar(c) => sb.append('?').append(c)
    case SexpSymbol(s) => printSymbol(s, sb)
    case SexpString(s) => printString(s, sb)
    // will not work for Scheme
    // e.g. http://rosettacode.org/wiki/Infinity#Scheme
    case SexpNil => sb.append("nil")
    case SexpNegInf => sb.append("-1.0e+INF")
    case SexpPosInf => sb.append("1.0e+INF")
    case SexpNaN => sb.append("0.0e+NaN")
    case SexpNumber(n) =>
      // .toString and .apply does not round-trip for really big exponents
      // but PlainString can be *really* slow and eat up loads of memory
      //sb.append(n.underlying.toPlainString)
      //sb.append(n.underlying.toEngineeringString())
      sb.append(n.toString())
  }

  // we prefer not to escape some characters when in strings
  private val exclude = Set("\n", "\t", " ")
  private val specials = SexpParser.specialChars.toList.map(_.swap)
  private val stringSpecials = SexpParser.specialChars.toList.map(_.swap).filterNot {
    case (from, to) => exclude(from)
  }

  protected def printSymbol(s: String, sb: StringBuilder): Unit = {
    val escaped = specials.foldLeft(s) {
      case (r, (from, to)) => r.replace(from, "\\" + to)
    }
    sb.append(escaped)
  }

  protected def printString(s: String, sb: StringBuilder): Unit = {
    val escaped = stringSpecials.foldLeft(s) {
      case (r, (from, to)) => r.replace(from, "\\" + to)
    }
    sb.append('"').append(escaped).append('"')
  }

  protected def printSeq[A](iterable: Iterable[A], printSeparator: => Unit)(f: A => Unit): Unit = {
    var first = true
    iterable.foreach { a =>
      if (first) first = false else printSeparator
      f(a)
    }
  }
}
