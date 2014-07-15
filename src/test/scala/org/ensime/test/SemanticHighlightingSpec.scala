package org.ensime.test

import org.ensime.server.CompletionControl
import org.ensime.server.RichCompilerControl
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.test.util.Helpers
import scala.reflect.internal.util.RangePosition

class SemanticHighlightingSpec extends FunSpec with Matchers {

  val allTypes = List[Symbol](
    'class,
    'constructor,
    'functionCall,
    'importedName,
    'object,
    'operator,
    'package,
    'param,
    'trait,
    'typeParam,
    'val,
    'valField,
    'var,
    'varField
  )

  def getSymbolDesignations(cc: RichCompilerControl, contents: String, tpes: List[Symbol] = allTypes): List[(Symbol, String)] = {

    val file = Helpers.srcFile("abc.scala", Helpers.contents(contents))
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
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
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
          List('class)
        )
        assert(sds === List(
          ('class, "Int"),
          ('class, "X1[Any]"),
          ('class, "X1[String]"),
          ('class, "X2"),
          ('class, "X2"),
          ('class, "Y"),
          ('class, "Any"),
          ('class, "Test")
        ))
      }
    }

    it("should highlight constructors") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
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
          List('constructor)
        )
        // TODO It would be better if the "new" was consistent.
        assert(sds === List(
          ('constructor, "X1"),
          ('constructor, "new X1(  )"),
          ('constructor, "X2"),
          ('constructor, "new   X3")
        ))
      }
    }

    it("should highlight function calls") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int) { u + v }
              def foo(u: Int, v: Int) { u + v }
              def bar { val X = fun(1, 2) ; foo(4, 5) }
              val x = "abc".substring(1,2)
              def baz { def quux(): Int = { 1 } ; quux() }
            }
          """,
          List('functionCall)
        )
        assert(sds === List(
          ('functionCall, "fun"),
          ('functionCall, "foo"),
          ('functionCall, "substring"),
          ('functionCall, "quux")
        ))
      }
    }

    it("should highlight imported names") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            import scala.reflect.internal.util.RangePosition
            import org.scalatest. { Matchers,
                 FunSpec }
            """,
          List('importedName)
        )
        assert(sds === List(
          ('importedName, "RangePosition"),
          ('importedName, "Matchers"),
          ('importedName, "FunSpec")
        ))
      }
    }

    it("should highlight objects") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            object A { object B { } }
            case class C() { }
            object Test {
              def fun (x:Any) = x match { case C() => 1 }  // object6
              val a = A    // object2
              val b = A.B  // .B = object4
            }
            """,
          List('object)
        )
        assert(sds === List(
          ('object, "C"),
          ('object, "A"),
          ('object, "A"),
          ('object, "B")
        ))
      }
    }

    it("should highlight operators") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            object Test {
              var a = 1 + 2 * 3
              a += 8
            }
          """,
          List('operator)
        )
        // TODO We should highlight the "+="
        assert(sds === List(
          ('operator, "+"),
          ('operator, "*")
        ))
      }
    }

    it("should highlight packages") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
             package com.example
             package other
          """,
          List('package)
        )
        assert(sds === List(
          ('package, "com"),
          ('package, "example"),
          ('package, "other")
        ))
      }
    }

    it("should highlight params") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            class Test {
              def f(u:  Int, v   :String) = v + u
            }
          """,
          List('param)
        )
        assert(sds === List(
          ('param, "u"),
          ('param, "v"),
          ('param, "v"),
          ('param, "u")
        ))
      }
    }

    it("should highlight traits") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
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
          List('trait)
        )
        assert(sds === List(
          ('trait, "X2"),
          ('trait, "X3"),
          ('trait, "X4"),
          ('trait, "X5[ String]"),
          ('trait, "v2 .X6")
        ))
      }
    }

    ignore("should highlight typeParams") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            class Test {
              def f[X](x: X) = x
              f[Int](1)
            }
          """,
          List('typeParam)
        )
        // TODO The following would be ideal. Right now we don't get any typeParam at all.
        assert(sds === List(
          ('typeParam, "X"),
          ('typeParam, "X"),
          ('typeParam, "Int")
        ))
      }
    }

    it("should highlight vals") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            class Test {
              def fun() = {
                val u = 1
                val v: Int = 1
                println(u)
              }
            }
          """,
          List('val)
        )
        assert(sds === List(
          ('val, "u"),
          ('val, "v"),
          ('val, "u")
        ))
      }
    }

    it("should highlight valFields") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
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
          List('valField)
        )
        // TODO There should be no "=" or ":"
        assert(sds === List(
          ('valField, "u="),
          ('valField, "v:"),
          ('valField, "u"),
          ('valField, "v")
        ))
      }
    }

    it("should highlight vars") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            class Test {
              def fun() = {
                var u = 1
                var v: Int = 1
                println(u)
              }
            }
          """,
          List('var)
        )
        assert(sds === List(
          ('var, "u"),
          ('var, "v"),
          ('var, "u")
        ))
      }
    }

    it("should highlight varFields") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
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
          List('varField)
        )
        // TODO There should be no "=" or ":"
        assert(sds === List(
          ('varField, "u="),
          ('varField, "v:"),
          ('varField, "u"),
          ('varField, "v")
        ))
      }
    }

    it("selects should not be confused by whitespace") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.  example
          """,
          List('package)
        )
        // only part of "example" is highlighted
        assert(sds === List(
          ('package, "com"),
          ('package, "example")
        ))
      }
    }

    ignore("should highlight method calls after operators") {
      Helpers.withPresCompiler { cc =>
        val sds = getSymbolDesignations(
          cc, """
            package com.example
            class Test {
              def fun(u: Int, v: Int) { u + v }
              def foo(u: Int, v: Int) { u - v }
              fun(1, 2) + foo(4, 5)
            }
          """,
          List('functionCall)
        )
        // Call to foo is missing
        assert(sds === List(
          ('functionCall, "fun"),
          ('functionCall, "foo")
        ))
      }
    }
  }
}
