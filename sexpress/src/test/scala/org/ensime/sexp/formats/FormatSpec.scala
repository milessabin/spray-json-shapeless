package org.ensime.sexp.formats

import org.scalatest.FunSpec
import org.ensime.sexp._

trait FormatSpec extends FunSpec {
  val foo = SexpString("foo")
  val bar = SexpSymbol("bar")

  import org.ensime.sexp.pimpAny
  def assertFormat[T: SexpFormat](start: T, expect: Sexp): Unit = {
    assert(start.toSexp === expect)
    assert(expect.convertTo[T] === start)
  }
}
