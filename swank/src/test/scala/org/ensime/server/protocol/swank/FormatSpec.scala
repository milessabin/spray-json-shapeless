package org.ensime.server.protocol.swank

import org.scalatest.FunSpec
import org.ensime.sexp._

// copied from S-Express to avoid a dependency on sexp:test
trait FormatSpec extends FunSpec {
  import org.ensime.sexp.pimpAny
  def assertFormat[T: SexpFormat](start: T, expect: Sexp): Unit = {
    assert(start.toSexp === expect)
    assert(expect.convertTo[T] === start)
  }
}
