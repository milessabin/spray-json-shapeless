package org.ensime.test

import scala.reflect.internal.util.OffsetPosition
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.slf4j.LoggerFactory
import org.ensime.test.util.Helpers

class RichPresentationCompilerSpec extends FunSpec with Matchers {

  val log = LoggerFactory.getLogger(this.getClass)

  describe("RichPresentationCompiler") {

    it("can call askTypeInfoByName on a class") {
      Helpers.withPresCompiler { cc =>
        val file = Helpers.srcFile("abc.scala", Helpers.contents(
          "package com.example",
          "class A { }"))
        cc.askReloadFile(file)
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
        cc.askReloadFile(file)
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
        cc.askReloadFile(file)
        cc.askLoadedTyped(file)
        val p = new OffsetPosition(file, 78)
        val infoList = cc.completionsAt(p, 10, false)
        log.debug(s"${infoList.completions}")
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
        cc.askReloadFile(file)
        cc.askLoadedTyped(file)
        val p = new OffsetPosition(file, 83)
        log.info("p = " + p)
        val infoList = cc.completionsAt(p, 10, false)
        log.info(s"${infoList.completions}")
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
        cc.askReloadFile(file)
        cc.askLoadedTyped(file)
        val p = new OffsetPosition(file, 80)
        log.info("p = " + p)
        val infoList = cc.completionsAt(p, 10, false)
        log.info(s"${infoList.completions}")
        assert(infoList.completions.length > 1)
        assert(infoList.completions.head.name == "Abc")
      }
    }

    it("should show classes without visible members in the inspector") {
      Helpers.withPresCompiler { cc =>
        val file = Helpers.srcFile("abc.scala", Helpers.contents(
          "package com.example",
          "trait bidon { }",
          "case class pipo extends bidon { }"
        ))
        cc.askReloadFile(file)
        cc.askLoadedTyped(file)
        val info = cc.askInspectTypeAt(new OffsetPosition(file, 37))
        val supers = info.map(_.supers).getOrElse(List())
        val supersNames = supers.map(_.tpe.name).toList
        assert(supersNames.toSet === Set("pipo", "bidon", "Object", "Product", "Serializable", "Any"))
      }
    }
  }
}
