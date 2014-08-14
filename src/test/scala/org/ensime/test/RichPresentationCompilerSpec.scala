package org.ensime.test

import scala.reflect.internal.util.OffsetPosition
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.test.util.Helpers

class RichPresentationCompilerSpec extends FunSpec with Matchers {

  describe("RichPresentationCompiler") {

    it("can call askTypeInfoByName on a class") {
      Helpers.withPresCompiler { cc =>
        val file = Helpers.srcFile("abc.scala", Helpers.contents(
          "package com.example",
          "class A { }"))
        cc.askLoadedTyped(file)
        val info = cc.askTypeInfoByName("com.example.A").get
        assert(info.declaredAs == 'class)
        assert(info.name == "A")
        assert(info.fullName == "com.example.A")
        assert(info.pos.point == 26)
      }
    }

    it("can call askTypeInfoByName on an object") {
      Helpers.withPresCompiler { cc =>
        val file = Helpers.srcFile("abc.scala", Helpers.contents(
          "package com.example",
          "object A { }"))
        cc.askLoadedTyped(file)
        val info = cc.askTypeInfoByName("com.example.A$").get
        assert(info.declaredAs == 'object)
        assert(info.name == "A$")
        assert(info.fullName == "com.example.A$")
        assert(info.pos.point == 27)
      }
    }

    it("can get completions on member with no prefix") {
      Helpers.withPresCompiler { cc =>
        val file = Helpers.srcFile("def.scala", Helpers.contents(
          "package com.example",
          "object A { def aMethod(a: Int) = a }",
          "object B { val x = A. "
        ))
        val p = new OffsetPosition(file, 78)
        val infoList = cc.completionsAt(p, 10, false)
        println(s"${infoList.completions}")
        assert(infoList.completions.length > 1)
        assert(infoList.completions.head.name == "aMethod")
      }
    }

    it("can get completions on a member with a prefix") {
      Helpers.withPresCompiler { cc =>
        val file = Helpers.srcFile("abc.scala", Helpers.contents(
          "package com.example",
          "object A { def aMethod(a: Int) = a }",
          "object B { val x = A.aMeth }"
        ))
        val p = new OffsetPosition(file, 83)
        println("p = " + p)
        val infoList = cc.completionsAt(p, 10, false)
        println(s"${infoList.completions}")
        assert(infoList.completions.length == 1)
        assert(infoList.completions.head.name == "aMethod")
      }
    }

    it("can get completions on an object name") {
      Helpers.withPresCompiler { cc =>
        val file = Helpers.srcFile("abc.scala", Helpers.contents(
          "package com.example",
          "object Abc { def aMethod(a: Int) = a }",
          "object B { val x = Ab }"
        ))
        val p = new OffsetPosition(file, 80)
        println("p = " + p)
        val infoList = cc.completionsAt(p, 10, false)
        println(s"${infoList.completions}")
        assert(infoList.completions.length > 1)
        assert(infoList.completions.head.name == "Abc")
      }
    }
  }
}
