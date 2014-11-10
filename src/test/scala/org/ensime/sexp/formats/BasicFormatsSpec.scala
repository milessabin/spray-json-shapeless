package org.ensime.sexp.formats

import org.scalatest.FunSpec
import org.scalatest.Matchers
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

class BasicFormatsSpec extends FormatSpec with BasicFormats {
  describe("BasicFormats") {
    it("should support Int") {
      assertFormat(13, SexpNumber(13))
      assertFormat(-1, SexpNumber(-1))
      assertFormat(0, SexpNumber(0))
      assertFormat(Int.MaxValue, SexpNumber(Int.MaxValue))
      assertFormat(Int.MinValue, SexpNumber(Int.MinValue))
    }

    it("should support Long") {
      assertFormat(13L, SexpNumber(13))
      assertFormat(-1L, SexpNumber(-1))
      assertFormat(0L, SexpNumber(0))
      assertFormat(Long.MaxValue, SexpNumber(Long.MaxValue))
      assertFormat(Long.MinValue, SexpNumber(Long.MinValue))
    }

    it("should support Float") {
      assertFormat(13.0f, SexpNumber(13.0f))
      assertFormat(-1.0f, SexpNumber(-1.0f))
      assertFormat(0.0f, SexpNumber(0.0f))
      assertFormat(Float.MaxValue, SexpNumber(Float.MaxValue))
      assertFormat(Float.MinValue, SexpNumber(Float.MinValue))
      assertFormat(Float.NegativeInfinity, SexpNegInf)
      assertFormat(Float.PositiveInfinity, SexpPosInf)

      // remember NaN != NaN
      assert(Float.NaN.toSexp === SexpNaN)
      assert(SexpNaN.convertTo[Float].isNaN)
    }

    it("should support Double") {
      assertFormat(13.0d, SexpNumber(13.0d))
      assertFormat(-1.0d, SexpNumber(-1.0d))
      assertFormat(0.0d, SexpNumber(0.0d))
      assertFormat(Double.MaxValue, SexpNumber(Double.MaxValue))
      assertFormat(Double.MinValue, SexpNumber(Double.MinValue))
      assertFormat(Double.NegativeInfinity, SexpNegInf)
      assertFormat(Double.PositiveInfinity, SexpPosInf)

      // remember NaN != NaN
      assert(Double.NaN.toSexp === SexpNaN)
      assert(SexpNaN.convertTo[Double].isNaN)
    }

    it("should support Boolean") {
      assertFormat(true, SexpSymbol("t"))
      assertFormat(false, SexpNil)
    }

    it("should support Char") {
      assertFormat('t', SexpChar('t'))
    }

    it("should support Unit") {
      assertFormat((), SexpNil)
    }

    it("should support Symbol") {
      assertFormat('blah, SexpString("blah"))
    }
  }

}
