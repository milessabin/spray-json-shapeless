package org.ensime.core

import org.ensime.fixture._
import org.ensime.api._
import org.scalatest._

import scala.reflect.internal.util.RangePosition

class SemanticHighlightingSpec extends WordSpec with Matchers
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  def original = EnsimeConfigFixture.EmptyTestProject

  def getSymbolDesignations(
    config: EnsimeConfig,
    cc: RichCompilerControl,
    content: String,
    tpes: List[SourceSymbol] = SourceSymbol.allSymbols
  ): List[(SourceSymbol, String)] = {

    val file = srcFile(config, "abc.scala", contents(content))
    cc.askLoadedTyped(file)
    val pos = new RangePosition(file, 0, 0, file.length)
    val sds = cc.askSymbolDesignationsInRegion(pos, tpes)
    sds.syms.sortWith((a, b) => a.start < b.start).
      map { sym =>
        (sym.symType, content.substring(sym.start, sym.end))
      }
  }

  "SemanticHighlighting" should {
    "highlight classes" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class X1[+A] { }
            class X2 { class Y { } }
            class Test {
              val a: Int = 1
              val b: X1[Any] = new X1[String]()
              val c: X2 = new X2
              val d = new c.Y
              def fun(a: Any) = a match { case x: Test => Unit }
            }
          """,
        List(ClassSymbol)
      )
      assert(sds === List(
        (ClassSymbol, "Int"),
        (ClassSymbol, "X1[Any]"),
        (ClassSymbol, "X1[String]"),
        (ClassSymbol, "X2"),
        (ClassSymbol, "X2"),
        (ClassSymbol, "Y"),
        (ClassSymbol, "Any"),
        (ClassSymbol, "Test")
      ))
    }

    "highlight constructors" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class X1(a: Int = 0) { }
            class X2(a: String) { }
            class X3 { }
            class Test {
              val b = new X1(3)
              val c1 = new X1(  )
              val d1 = new X2("y")
              val e1 = new   X3
            }
          """,
        List(ConstructorSymbol)
      )
      // TODO It would be better if the "new" was consistent.
      assert(sds === List(
        (ConstructorSymbol, "X1"),
        (ConstructorSymbol, "new X1(  )"),
        (ConstructorSymbol, "X2"),
        (ConstructorSymbol, "new   X3")
      ))
    }

    "highlight function calls" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int) { u + v }
              def foo(u: Int, v: Int) { u + v }
              def bar { val X = fun(1, 2) ; foo(4, 5) }
              val x = "abc".substring(1,2)
              def baz { def quux(): Int = { 1 } ; quux() }
            }
          """,
        List(FunctionCallSymbol)
      )
      assert(sds === List(
        (FunctionCallSymbol, "fun"),
        (FunctionCallSymbol, "foo"),
        (FunctionCallSymbol, "substring"),
        (FunctionCallSymbol, "quux")
      ))
    }

    "highlight imported names" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            import scala.reflect.internal.util.RangePosition
            import org.scalatest. { Matchers,
                 FunSpec }
            """,
        List(ImportedNameSymbol)
      )
      assert(sds === List(
        (ImportedNameSymbol, "RangePosition"),
        (ImportedNameSymbol, "Matchers"),
        (ImportedNameSymbol, "FunSpec")
      ))
    }

    "highlight objects" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            object A { object B { } }
            case class C() { }
            case class D(i: Int) { case class E(i: Int) { } }
            object Test {
              def fun (x:Any) = x match { case C() => 1 }
              val b = A.B
              val c = D(1)
              val d = c.E(1)
            }
          """,
        List(ObjectSymbol)
      )
      assert(sds === List(
        (ObjectSymbol, "C"),
        (ObjectSymbol, "A"),
        (ObjectSymbol, "B"),
        (ObjectSymbol, "D"),
        // TODO two problems there: "c" should be a varField ; E should be highlighted.
        (ObjectSymbol, "c")
      ))
    }

    "highlight operators" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            object Test {
              var a = 1 + 2 * 3
              a += 8
            }
          """,
        List(OperatorFieldSymbol)
      )
      // TODO We should highlight the "+="
      assert(sds === List(
        (OperatorFieldSymbol, "+"),
        (OperatorFieldSymbol, "*")
      ))
    }

    "highlight packages" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
             package com.example
             package other
          """,
        List(PackageSymbol)
      )
      assert(sds === List(
        (PackageSymbol, "com"),
        (PackageSymbol, "example"),
        (PackageSymbol, "other")
      ))
    }

    "highlight params" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              def f(u:  Int, v   :String) = v + u
            }
          """,
        List(ParamSymbol)
      )
      assert(sds === List(
        (ParamSymbol, "u"),
        (ParamSymbol, "v"),
        (ParamSymbol, "v"),
        (ParamSymbol, "u")
      ))
    }

    "highlight traits" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            trait X1 { }
            trait X2 { }
            trait X3 { }
            trait X4 { }
            trait X5[A] { }
            class C extends X2 { trait X6 {}   }
            class Test {
              def fun(x2: X3) = x2
              var v1: X4
              val v2 = new C
              def foo(x: Any) = x match {
                case _: X5[ String] => x
                case _: v2 .X6 => x
              }
            }
          """,
        List(TraitSymbol)
      )
      assert(sds === List(
        (TraitSymbol, "X2"),
        (TraitSymbol, "X3"),
        (TraitSymbol, "X4"),
        (TraitSymbol, "X5[ String]"),
        (TraitSymbol, "v2 .X6")
      ))
    }

    "highlight typeParams" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              def f[XX,YY](y: YY, x: XX) = {
                var z: YY
              }
              f[Int, String](1, "a")
            }
          """,
        List(TypeParamSymbol)
      )
      assert(sds === List(
        (TypeParamSymbol, "XX"),
        (TypeParamSymbol, "YY"),
        (TypeParamSymbol, "YY"),
        (TypeParamSymbol, "XX"),
        (TypeParamSymbol, "YY")
      ))
    }

    "highlight vals" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              def fun() = {
                val u = 1
                val v: Int = 1
                println(u)
              }
            }
          """,
        List(ValSymbol)
      )
      assert(sds === List(
        (ValSymbol, "u"),
        (ValSymbol, "v"),
        (ValSymbol, "u")
      ))
    }

    "highlight valFields" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              val u= 1
              val v:Int = 1
              println( u )
            }
            class Test2 {
              println((new Test).v)
            }
          """,
        List(ValFieldSymbol)
      )
      assert(sds === List(
        (ValFieldSymbol, "u"),
        (ValFieldSymbol, "v"),
        (ValFieldSymbol, "u"),
        (ValFieldSymbol, "v")
      ))
    }

    "highlight vars" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              def fun() = {
                var u = 1
                var v: Int = 1
                println(u)
              }
            }
          """,
        List(VarSymbol)
      )
      assert(sds === List(
        (VarSymbol, "u"),
        (VarSymbol, "v"),
        (VarSymbol, "u")
      ))
    }

    "highlight varFields" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              var u= 1
              var v:Int   = 1
              println( u )
            }
            class Test2 {
              println((new Test).v)
            }
          """,
        List(VarFieldSymbol)
      )
      assert(sds === List(
        (VarFieldSymbol, "u"),
        (VarFieldSymbol, "v"),
        (VarFieldSymbol, "u"),
        (VarFieldSymbol, "v")
      ))
    }

    "highlight setter operators" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            object Fubar {
               private var v: Int = 0
               def value = v
               def value_=(a: Int) = v = a
            }
            class Test {
              Fubar.value = 1
            }
          """,
        List(OperatorFieldSymbol)
      )
      assert(sds === List(
        (OperatorFieldSymbol, "value")
      ))
    }

    "not be confused by whitespace" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.  example
          """,
        List(PackageSymbol)
      )
      // only part of "example" is highlighted
      assert(sds === List(
        (PackageSymbol, "com"),
        (PackageSymbol, "example")
      ))
    }

    "highlight negation operators" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              val x = !(3 == 4)
            }
          """,
        List(OperatorFieldSymbol)
      )
      // Call to foo is missing
      assert(sds === List(
        (OperatorFieldSymbol, "!"),
        (OperatorFieldSymbol, "==")
      ))
    }

    "highlight implicit conversions" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {}
            object I {
              implicit def StringToTest(v: String): Test = new Test
              val t: Test  = "sample";
              val u: Test  = StringToTest("y");
            }
          """,
        List(ImplicitConversionSymbol)
      )
      assert(sds === List(
        (ImplicitConversionSymbol, "\"sample\"")
      ))
    }

    "highlight implicit parameters" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Thing {}
            class Thong {}
            object I {
              implicit def myThing = new Thing
              implicit val myThong = new Thong
              def zz(u: Int)(implicit s: Thing, t: Thong) = u
              val t = zz(1)
            }
          """,
        List(ImplicitParamsSymbol)
      )
      assert(sds === List(
        (ImplicitParamsSymbol, "zz(1)")
      ))
    }

    "highlight method calls after operators" in withPresCompiler { (config, cc) =>
      val sds = getSymbolDesignations(
        config, cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int): Int = { u + v }
              def foo(u: Int, v: Int): Int = { u - v }
              fun(1, 2) + foo(4, 5)
            }
          """,
        List(FunctionCallSymbol)
      )
      assert(sds === List(
        (FunctionCallSymbol, "fun"),
        (FunctionCallSymbol, "foo")
      ))
    }
  }
}
