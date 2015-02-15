package org.ensime.core

import java.io.File
import org.ensime.model._
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.util.Helpers
import scala.reflect.internal.util.RangePosition

class SemanticHighlightingSpec extends FunSpec with Matchers {

  def getSymbolDesignations(
    dir: File,
    cc: RichCompilerControl,
    contents: String,
    tpes: Set[SourceSymbol] = SourceSymbol.allSymbols): List[(SourceSymbol, String)] = {

    val file = Helpers.srcFile(dir, "abc.scala", Helpers.contents(contents))
    cc.askLoadedTyped(file)
    val pos = new RangePosition(file, 0, 0, file.length)
    val sds = cc.askSymbolDesignationsInRegion(pos, tpes)
    sds.syms.sortWith((a, b) => a.start < b.start).
      map { sym =>
        (sym.symType, contents.substring(sym.start, sym.end))
      }
  }

  describe("SemanticHighlighting") {

    it("should highlight classes") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
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
          Set(ClassSymbol)
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
    }

    it("should highlight constructors") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
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
          Set(ConstructorSymbol)
        )
        // TODO It would be better if the "new" was consistent.
        assert(sds === List(
          (ConstructorSymbol, "X1"),
          (ConstructorSymbol, "new X1(  )"),
          (ConstructorSymbol, "X2"),
          (ConstructorSymbol, "new   X3")
        ))
      }
    }

    it("should highlight function calls") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int) { u + v }
              def foo(u: Int, v: Int) { u + v }
              def bar { val X = fun(1, 2) ; foo(4, 5) }
              val x = "abc".substring(1,2)
              def baz { def quux(): Int = { 1 } ; quux() }
            }
          """,
          Set(FunctionCallSymbol)
        )
        assert(sds === List(
          (FunctionCallSymbol, "fun"),
          (FunctionCallSymbol, "foo"),
          (FunctionCallSymbol, "substring"),
          (FunctionCallSymbol, "quux")
        ))
      }
    }

    it("should highlight imported names") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            import scala.reflect.internal.util.RangePosition
            import org.scalatest. { Matchers,
                 FunSpec }
            """,
          Set(ImportedNameSymbol)
        )
        assert(sds === List(
          (ImportedNameSymbol, "RangePosition"),
          (ImportedNameSymbol, "Matchers"),
          (ImportedNameSymbol, "FunSpec")
        ))
      }
    }

    it("should highlight objects") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
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
          Set(ObjectSymbol)
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
    }

    it("should highlight operators") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            object Test {
              var a = 1 + 2 * 3
              a += 8
            }
          """,
          Set(OperatorFieldSymbol)
        )
        // TODO We should highlight the "+="
        assert(sds === List(
          (OperatorFieldSymbol, "+"),
          (OperatorFieldSymbol, "*")
        ))
      }
    }

    it("should highlight packages") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
             package com.example
             package other
          """,
          Set(PackageSymbol)
        )
        assert(sds === List(
          (PackageSymbol, "com"),
          (PackageSymbol, "example"),
          (PackageSymbol, "other")
        ))
      }
    }

    it("should highlight params") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            class Test {
              def f(u:  Int, v   :String) = v + u
            }
          """,
          Set(ParamSymbol)
        )
        assert(sds === List(
          (ParamSymbol, "u"),
          (ParamSymbol, "v"),
          (ParamSymbol, "v"),
          (ParamSymbol, "u")
        ))
      }
    }

    it("should highlight traits") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
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
          Set(TraitSymbol)
        )
        assert(sds === List(
          (TraitSymbol, "X2"),
          (TraitSymbol, "X3"),
          (TraitSymbol, "X4"),
          (TraitSymbol, "X5[ String]"),
          (TraitSymbol, "v2 .X6")
        ))
      }
    }

    it("should highlight typeParams") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            class Test {
              def f[XX,YY](y: YY, x: XX) = {
                var z: YY
              }
              f[Int, String](1, "a")
            }
          """,
          Set(TypeParamSymbol)
        )
        assert(sds === List(
          (TypeParamSymbol, "XX"),
          (TypeParamSymbol, "YY"),
          (TypeParamSymbol, "YY"),
          (TypeParamSymbol, "XX"),
          (TypeParamSymbol, "YY")
        ))
      }
    }

    it("should highlight vals") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            class Test {
              def fun() = {
                val u = 1
                val v: Int = 1
                println(u)
              }
            }
          """,
          Set(ValSymbol)
        )
        assert(sds === List(
          (ValSymbol, "u"),
          (ValSymbol, "v"),
          (ValSymbol, "u")
        ))
      }
    }

    it("should highlight valFields") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
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
          Set(ValFieldSymbol)
        )
        assert(sds === List(
          (ValFieldSymbol, "u"),
          (ValFieldSymbol, "v"),
          (ValFieldSymbol, "u"),
          (ValFieldSymbol, "v")
        ))
      }
    }

    it("should highlight vars") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            class Test {
              def fun() = {
                var u = 1
                var v: Int = 1
                println(u)
              }
            }
          """,
          Set(VarSymbol)
        )
        assert(sds === List(
          (VarSymbol, "u"),
          (VarSymbol, "v"),
          (VarSymbol, "u")
        ))
      }
    }

    it("should highlight varFields") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
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
          Set(VarFieldSymbol)
        )
        assert(sds === List(
          (VarFieldSymbol, "u"),
          (VarFieldSymbol, "v"),
          (VarFieldSymbol, "u"),
          (VarFieldSymbol, "v")
        ))
      }
    }

    it("should highlight setter operators") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
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
          Set(OperatorFieldSymbol)
        )
        assert(sds === List(
          (OperatorFieldSymbol, "value")
        ))
      }
    }

    it("selects should not be confused by whitespace") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.  example
          """,
          Set(PackageSymbol)
        )
        // only part of "example" is highlighted
        assert(sds === List(
          (PackageSymbol, "com"),
          (PackageSymbol, "example")
        ))
      }
    }

    it("should highlight negation operators") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            class Test {
              val x = !(3 == 4)
            }
          """,
          Set(OperatorFieldSymbol)
        )
        // Call to foo is missing
        assert(sds === List(
          (OperatorFieldSymbol, "!"),
          (OperatorFieldSymbol, "==")
        ))
      }
    }

    it("should highlight method calls after operators") {
      Helpers.withPresCompiler { (dir, cc) =>
        val sds = getSymbolDesignations(
          dir, cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int): Int = { u + v }
              def foo(u: Int, v: Int): Int = { u - v }
              fun(1, 2) + foo(4, 5)
            }
          """,
          Set(FunctionCallSymbol)
        )
        assert(sds === List(
          (FunctionCallSymbol, "fun"),
          (FunctionCallSymbol, "foo")
        ))
      }
    }
  }
}
