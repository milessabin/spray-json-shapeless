package org.ensime.core

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.ensime.config._
import org.ensime.core._
import org.ensime.fixture._
import org.ensime.indexer.{ SearchService, SourceResolver }
import org.ensime.model._
import org.scalatest._
import pimpathon.file._
import scala.collection.immutable.Queue
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.internal.util.{ BatchSourceFile, OffsetPosition }
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.{ ConsoleReporter, StoreReporter }

class RichPresentationCompilerSpec extends WordSpec with Matchers
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.EmptyTestProject

  "RichPresentationCompiler" should {
    "round-trip between typeFullName and askTypeInfoByName" in withPresCompiler { (config, cc) =>
      val file = srcFile(config, "abc.scala", contents(
        "package com.example",
        "object /*1*/A { ",
        "   val /*1.1*/x: Int = 1",
        "   class  /*1.2*/X {} ",
        "   object /*1.3*/X {} ",
        "}",
        "class  /*2*/A { ",
        "   class  /*2.1*/X {} ",
        "   object /*2.2*/X {} ",
        "}"
      ))
      cc.askReloadFile(file)
      cc.askLoadedTyped(file)

      def roundtrip(label: String, expectedFullName: String) = {
        val comment = "/*" + label + "*/"
        val index = file.content.mkString.indexOf(comment)
        val tpe = cc.askTypeInfoAt(new OffsetPosition(file, index + comment.length)).get
        val fullName = tpe.fullName
        assert(fullName === expectedFullName)

        val tpe2 = cc.askTypeInfoByName(fullName).get
        assert(tpe2.fullName === expectedFullName)
      }

      roundtrip("1", "com.example.A$")
      roundtrip("1.1", "scala.Int")
      roundtrip("1.2", "com.example.A$$X")
      roundtrip("1.3", "com.example.A$$X$")
      roundtrip("2", "com.example.A")
      roundtrip("2.1", "com.example.A$X")
      roundtrip("2.2", "com.example.A$X$")
    }

    "handle askMemberInfoByName" in withPresCompiler { (config, cc) =>
      val file = srcFile(config, "abc.scala", contents(
        "package com.example",
        "object A { ",
        "   val x: Int = 1",
        "   class  X {}",
        "   object X {}",
        "}",
        "class A { ",
        "   class  X {}",
        "   object X {}",
        "}"
      ))
      cc.askReloadFile(file)
      cc.askLoadedTyped(file)

      def test(typeName: String, memberName: String, isType: Boolean, expectedTypeName: String, expectedDeclAs: Symbol) = {
        val sym = cc.askMemberInfoByName(typeName, memberName, isType).get
        assert(sym.localName === memberName)
        assert(sym.tpe.fullName === expectedTypeName)
        assert(sym.tpe.declaredAs === expectedDeclAs)
      }

      test("com.example$", "A", isType = false, "com.example.A$", 'object)
      test("com.example.A$", "x", isType = false, "scala.Int", 'class)
      test("com.example.A$", "X", isType = false, "com.example.A$$X$", 'object)
      test("com.example.A$", "X", isType = true, "com.example.A$$X", 'class)

      test("com.example$", "A", isType = true, "com.example.A", 'class)
      test("com.example.A", "X", isType = false, "com.example.A$X$", 'object)
      test("com.example.A", "X", isType = true, "com.example.A$X", 'class)
    }

    "get completions on member with no prefix" in withPosInCompiledSource(
      "package com.example",
      "object A { def aMethod(a: Int) = a }",
      "object B { val x = A.@@ "
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        result.completions should not be empty
        result.completions.head.name shouldBe "aMethod"
      }

    "won't try to complete the declaration containing point" in withPosInCompiledSource(
      "package com.example",
      "object Ab@@c {}"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(!result.completions.exists(_.name == "Abc"))
      }

    "get completions on a member with a prefix" in withPosInCompiledSource(
      "package com.example",
      "object A { def aMethod(a: Int) = a }",
      "object B { val x = A.aMeth@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.length == 1)
        assert(result.completions.head.name == "aMethod")
      }

    "get completions on an object name" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = Ab@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.length > 1)
        assert(result.completions.head.name == "Abc")
      }

    "get members for infix method call" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = Abc aM@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.head.name == "aMethod")
      }

    "get members for infix method call without prefix" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = Abc @@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.head.name == "aMethod")
      }

    "complete multi-character infix operator" in withPosInCompiledSource(
      "package com.example",
      "object B { val l = Nil; val ll = l +@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "++"))
      }

    "complete top level import" in withPosInCompiledSource(
      "package com.example",
      "import ja@@"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "java"))
      }

    "complete sub-import" in withPosInCompiledSource(
      "package com.example",
      "import java.ut@@"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "util"))
      }

    "complete multi-import" in withPosInCompiledSource(
      "package com.example",
      "import java.util.{ V@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "Vector"))
      }

    "complete new construction" in withPosInCompiledSource(
      "package com.example",
      "import java.util.Vector",
      "object A { def main { new V@@ } }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(m => m.name == "Vector" && m.isCallable))
      }

    "complete symbol in logical op" in withPosInCompiledSource(
      "package com.example",
      "object A { val apple = true; true || app@@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "apple"))
      }

    "complete infix method of Set." in withPosInCompiledSource(
      "package com.example",
      "object A { val t = Set[String](\"a\", \"b\"); t @@ }"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.exists(_.name == "seq"))
        assert(result.completions.exists(_.name == "|"))
        assert(result.completions.exists(_.name == "&"))
      }

    "complete interpolated variables in strings" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = s\"hello there, ${Abc.aMe@@}\"}"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.head.name == "aMethod")
      }

    "not attempt to complete symbols in strings" in withPosInCompiledSource(
      "package com.example",
      "object Abc { def aMethod(a: Int) = a }",
      "object B { val x = \"hello there Ab@@\"}"
    ) { (p, cc) =>
        val result = cc.completionsAt(p, 10, caseSens = false)
        assert(result.completions.isEmpty)
      }

    "show all type arguments in the inspector." in withPosInCompiledSource(
      "package com.example",
      "class A { ",
      "def banana(p: List[String]): List[String] = p",
      "def pineapple: List[String] = List(\"spiky\")",
      "}",
      "object Main { def main { val my@@A = new A() }}"
    ) { (p, cc) =>
        val info = cc.askInspectTypeAt(p).get
        val sup = info.supers.find(sup => sup.tpe.name == "A").get;
        {
          val mem = sup.tpe.members.find(_.name == "banana").get.asInstanceOf[NamedTypeMemberInfo]
          val tpe = mem.tpe.asInstanceOf[ArrowTypeInfo]
          assert(tpe.resultType.name == "List")
          assert(tpe.resultType.args.head.name == "String")
          val (paramName, paramTpe) = tpe.paramSections.head.params.head
          assert(paramName == "p")
          assert(paramTpe.name == "List")
          assert(paramTpe.args.head.name == "String")
        }
        {
          val mem = sup.tpe.members.find(_.name == "pineapple").get.asInstanceOf[NamedTypeMemberInfo]
          val tpe = mem.tpe.asInstanceOf[BasicTypeInfo]
          assert(tpe.name == "List")
          assert(tpe.args.head.name == "String")
        }
      }

    "show classes without visible members in the inspector" in withPosInCompiledSource(
      "package com.example",
      "trait bidon { }",
      "case class pi@@po extends bidon { }"
    ) { (p, cc) =>
        val info = cc.askInspectTypeAt(p)
        val supers = info.map(_.supers).getOrElse(List())
        val supersNames = supers.map(_.tpe.name).toList
        assert(supersNames.toSet === Set("pipo", "bidon", "Object", "Product", "Serializable", "Any"))
      }

    "get symbol positions for compiled files" in withPresCompiler { (config, cc) =>
      val defsFile = srcFile(config, "com/example/defs.scala", contents(
        "package com.example",
        "object /*1*/A { ",
        "   val /*1.1*/x: Int = 1",
        "   class /*1.2*/X {} ",
        "}",
        "class  /*2*/B { ",
        "   val /*2.1*/y: Int = 1",
        "   def /*2.2*/meth(a: String): Int = 1",
        "   def /*2.3*/meth(a: Int): Int = 1",
        "}",
        "trait  /*3*/C { ",
        "   val /*3.1*/z: Int = 1",
        "   class /*3.2*/Z {} ",
        "}",
        "class /*4*/D extends C { }"
      ), write = true)
      val usesFile = srcFile(config, "com/example/uses.scala", contents(
        "package com.example",
        "object Test { ",
        "   val x_1 = A/*1*/",
        "   val x_1_1 = A.x/*1.1*/",
        "   val x_1_2 = new A.X/*1.2*/",
        "   val x_2 = new B/*2*/",
        "   val x_2_1 = new B().y/*2.1*/",
        "   val x_2_2 = new B().meth/*2.2*/(\"x\")",
        "   val x_2_3 = new B().meth/*2.3*/(1)",
        "   val x_3: C/*3*/ = new D/*4*/",
        "   val x_3_1 = x_3.z/*3.1*/",
        "   val x_3_2 = new x_3.Z/*3.2*/",
        "}"
      ))

      def test(label: String, cc: RichPresentationCompiler) = {
        val comment = "/*" + label + "*/"
        val defPos = defsFile.content.mkString.indexOf(comment) + comment.length
        val usePos = usesFile.content.mkString.indexOf(comment) - 1

        // Create a fresh pres. compiler unaffected by previous tests

        val cc1 = new RichPresentationCompiler(cc.config, cc.settings, cc.reporter, cc.parent, cc.indexer, cc.search)

        try {
          cc1.askReloadFile(usesFile)
          cc1.askLoadedTyped(usesFile)

          val info = cc1.askSymbolInfoAt(new OffsetPosition(usesFile, usePos)) match {
            case Some(x) => x
            case None => fail(s"For $comment, askSymbolInfoAt returned None")
          }
          val declPos = info.declPos
          declPos match {
            case Some(op: OffsetSourcePosition) => assert(op.offset === defPos)
            case _ => fail(s"For $comment, unexpected declPos value: $declPos")
          }
        } finally {
          cc1.askShutdown()
        }
      }

      compileScala(
        List(defsFile.path),
        config.subprojects.head.targetDirs.head,
        cc.settings.classpath.value
      )

      cc.search.refreshResolver()
      Await.result(cc.search.refresh(), 180.seconds)

      List("1", "1.1", "1.2", "2", "2.1", "2.2", "2.3", "3", "3.1", "3.2", "4").
        foreach(test(_, cc))
    }
  }
}

trait RichPresentationCompilerTestUtils {
  def compileScala(paths: List[String], target: File, classPath: String): Unit = {
    val settings = new Settings
    settings.outputDirs.setSingleOutput(target.getAbsolutePath)
    val reporter = new ConsoleReporter(settings)
    settings.classpath.value = classPath
    val g = new scala.tools.nsc.Global(settings, reporter)
    val run = new g.Run
    run.compile(paths)
  }

  def srcFile(proj: EnsimeConfig, name: String, content: String, write: Boolean = false, encoding: String = "UTF-8"): BatchSourceFile = {
    val src = proj.subprojects.head.sourceRoots.head / name
    if (write) {
      src.create()
      scala.tools.nsc.io.File(src)(encoding).writeAll(content)
    }
    new BatchSourceFile(src.getPath, content)
  }

  def readSrcFile(src: BatchSourceFile, encoding: String = "UTF-8"): String =
    scala.tools.nsc.io.File(src.path)(encoding).slurp()

  def contents(lines: String*) = lines.mkString("\n")
}

trait ReallyRichPresentationCompilerFixture {
  this: RichPresentationCompilerFixture with RichPresentationCompilerTestUtils =>

  // conveniences for accessing the fixtures
  final def withPresCompiler(
    testCode: (EnsimeConfig, RichPresentationCompiler) => Any): Any =
    withRichPresentationCompiler { (_, c, cc) => testCode(c, cc) }

  // final def withPosInCompiledSource(lines: String*)(testCode: (OffsetPosition, RichPresentationCompiler) => Any) =
  //   withPosInCompiledSource{ (p, _, pc) => testCode(p, pc) }

  final def withPosInCompiledSource(lines: String*)(testCode: (OffsetPosition, RichPresentationCompiler) => Any): Any =
    withPresCompiler { (config, cc) =>
      import ReallyRichPresentationCompilerFixture._
      runForPositionInCompiledSource(config, cc, lines: _*) {
        (p, _, cc) => testCode(p, cc)
      }
    }

}

object ReallyRichPresentationCompilerFixture
    extends RichPresentationCompilerTestUtils {
  def runForPositionInCompiledSource(config: EnsimeConfig, cc: RichPresentationCompiler, lines: String*)(testCode: (OffsetPosition, String, RichPresentationCompiler) => Any): Any = {
    import RichPresentationCompilerFixture._
    val contents = lines.mkString("\n")
    var offset = 0
    var points = Queue.empty[(Int, String)]
    val re = """@([a-z0-9\.]*)@"""
    val starts = re.r.findAllMatchIn(contents).foreach { m =>
      points :+= (m.start - offset, m.group(1))
      offset += (m.end - m.start)
    }
    val file = srcFile(config, "def.scala", contents.replaceAll(re, ""))
    cc.askReloadFile(file)
    cc.askLoadedTyped(file)
    assert(points.length > 0)
    for (pt <- points) {
      testCode(new OffsetPosition(file, pt._1), pt._2, cc)
    }
  }
}
