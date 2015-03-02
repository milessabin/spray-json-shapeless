package org.ensime.model

import org.scalatest.{ FunSpec, Matchers }

class PatchSourceSpec extends FunSpec with Matchers {

  describe("PatchSource") {

    it("should apply patches correctly") {
      import org.ensime.util.PatchSource._
      assert(applyOperations("abc", List(PatchReplace(0, 1, ""))) == "bc")
      assert(applyOperations("abc", List(PatchDelete(0, 1))) == "bc")
      assert(applyOperations("abc", List(
        PatchDelete(0, 1),
        PatchInsert(1, "z"),
        PatchDelete(2, 3))) == "zb")
      assert(applyOperations("hello there", List(PatchReplace(0, 6, ""))) == "there")
      assert(applyOperations("hello there", List(PatchInsert(0, "zz"))) == "zzhello there")
      assert(applyOperations("", List(PatchInsert(0, "moose"))) == "moose")
      assert(applyOperations("abcde", List(
        PatchReplace(0, 3, "z"),
        PatchReplace(3, 5, "q")
      )) == "zq")
      assert(applyOperations("", List(PatchInsert(0, "darling!\n"))) == "darling!\n")
      assert(applyOperations("\n", List(PatchInsert(1, "darling!\n"))) == "\ndarling!\n")
    }

  }
}

