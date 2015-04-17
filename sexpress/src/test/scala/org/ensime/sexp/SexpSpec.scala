package org.ensime.sexp

import org.scalatest._

class SexpSpec extends FunSpec with Matchers {

  val foostring = SexpString("foo")
  val barstring = SexpString("bar")
  val foosym = SexpSymbol("foo")
  val barsym = SexpSymbol("bar")
  val fookey = SexpSymbol(":foo")
  val barkey = SexpSymbol(":bar")

  describe("SexpList") {
    it("should create from varargs") {
      assert(SexpList(foosym, barsym) === SexpList(List(foosym, barsym)))
    }

    it("should unroll as basic") {
      assert(SexpList(Nil) === SexpNil)

      assert(SexpList(foosym) ===
        SexpCons(foosym, SexpNil))

      assert(SexpList(foosym, barsym) ===
        SexpCons(foosym, SexpCons(barsym, SexpNil)))
    }

    it("should match lists") {
      SexpCons(foosym, SexpNil) match {
        case SexpList(els) if els == List(foosym) =>
        case _ => fail()
      }
      SexpCons(foosym, SexpCons(barsym, SexpNil)) match {
        case SexpList(els) if els == List(foosym, barsym) =>
        case _ => fail()
      }
      SexpNil match {
        case SexpList(_) => fail()
        case _ =>
      }
    }
  }

  describe("SexpData") {
    it("should create from varargs") {
      assert(SexpData(
        fookey -> barsym,
        barkey -> foosym
      ) === SexpList(
          fookey, barsym,
          barkey, foosym
        ))
    }

    it("should unroll as basic") {
      assert(SexpData(
        fookey -> barsym,
        barkey -> foosym
      ) === SexpCons(
          fookey, SexpCons(
          barsym, SexpCons(
          barkey, SexpCons(
          foosym, SexpNil
        )
        )
        )
        ))
    }

    it("should match SexpData") {
      SexpCons(
        fookey, SexpCons(
        barsym, SexpCons(
        barkey, SexpCons(
        foosym, SexpNil
      )
      )
      )
      ) match {
        case SexpData(kvs) if kvs.size == 2 =>
        case _ => fail()
      }

      SexpNil match {
        case SexpData(_) => fail()
        case _ =>
      }
    }
  }

  describe("SexpCons") {
    it("should unroll as fully basic") {
      val a = SexpList(foosym)
      val b = SexpList(barsym)
      assert(SexpCons(a, b) ===
        SexpCons(SexpCons(foosym, SexpNil), SexpCons(barsym, SexpNil)))
    }
  }
}
