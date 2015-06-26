package org.ensime.core

import org.ensime.fixture._
import org.ensime.api._
import org.scalatest._

import scala.reflect.internal.util.{ OffsetPosition, RangePosition }

class ImplicitAnalyzerSpec extends WordSpec with Matchers
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  def original = EnsimeConfigFixture.EmptyTestProject

  def getImplicitDetails(cc: RichPresentationCompiler, content: String) = {
    val file = srcFile(cc.config, "abc.scala", contents(content))
    cc.askLoadedTyped(file)
    val pos = new RangePosition(file, 0, 0, file.length)
    val dets = new ImplicitAnalyzer(cc).implicitDetails(pos)
    dets.map {
      case c: ImplicitConversionInfo => (
        "conversion",
        content.substring(c.start, c.end),
        c.fun.name
      )
      case c: ImplicitParamInfo => (
        "param",
        content.substring(c.start, c.end),
        c.fun.name,
        c.params.map { p => p.name },
        c.funIsImplicit
      )
    }
  }

  "ImplicitAnalyzer" should {
    "render implicit conversions" in withPresCompiler { (config, cc) =>
      val dets = getImplicitDetails(
        cc,
        """
            package com.example
            class Test {}
            object I {
              implicit def StringToTest(v: String): Test = new Test
              val t: Test  = "sample";
            }
        """
      )
      assert(dets === List(
        ("conversion", "\"sample\"", "StringToTest")
      ))
    }

    "render implicit parameters passed to implicit conversion functions" in withPresCompiler { (config, cc) =>
      val dets = getImplicitDetails(
        cc,
        """
            package com.example
            class Test {}
            class Thing {}
            object I {
              implicit def myThing = new Thing
              implicit def StringToTest(v: String)(implicit th: Thing): Test = new Test
              val t: Test = "sample"
            }
        """
      )
      assert(dets === List(
        ("param", "\"sample\"", "StringToTest", List("myThing"), true),
        ("conversion", "\"sample\"", "StringToTest")
      ))
    }

    "render implicit parameters" in withPresCompiler { (config, cc) =>
      val dets = getImplicitDetails(
        cc,
        """
            package com.example
            class Thing {}
            class Thong {}
            object I {
              implicit def myThing = new Thing
              implicit val myThong = new Thong
              def zz(u: Int)(v: String)(implicit s: Thing, t: Thong) = u
              def yy(implicit s: Thing) = s
              val t = zz(1)("abc")    // Two explicit applications
              val z = yy              // Zero explicit application
            }
        """
      )
      assert(dets === List(
        ("param", "zz(1)(\"abc\")", "zz", List("myThing", "myThong"), false),
        ("param", "yy", "yy", List("myThing"), false)
      ))
    }

    "work with offset positions" in withPresCompiler { (config, cc) =>

      val content = """
            package com.example
            class Test {}
            object I {
              implicit def StringToTest(v: String): Test = new Test
              val t: Test  = "sample"/*1*/;
            }
        """

      val file = srcFile(cc.config, "abc.scala", content)
      cc.askLoadedTyped(file)
      val implicitPos = content.indexOf("/*1*/");

      val pos = new OffsetPosition(file, implicitPos)
      val dets = new ImplicitAnalyzer(cc).implicitDetails(pos)
      assert(dets.length === 1)

      val pos1 = new OffsetPosition(file, implicitPos + 1)
      val dets1 = new ImplicitAnalyzer(cc).implicitDetails(pos1)
      assert(dets1.length === 0)
    }
  }
}
