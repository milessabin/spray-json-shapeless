package org.ensime.test

import java.io.{ File => JFile }
import org.ensime.model.SymbolInfo
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.ensime.test.util.Helpers
import scala.tools.nsc.util.BatchSourceFile
import scala.tools.nsc.io.PlainFile
import scala.tools.nsc.util.OffsetPosition

class RichPresentationCompilerSpec extends FunSpec with ShouldMatchers {

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
  }
}
