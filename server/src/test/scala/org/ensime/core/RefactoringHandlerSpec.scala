package org.ensime.core

import java.io.File
import org.ensime.model.ContentsInSourceFileInfo
import org.ensime.model.ContentsSourceFileInfo
import org.ensime.model.FileSourceFileInfo
import org.ensime.util.Helpers
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.util.TestUtil._

class RefactoringHandlerSpec extends FunSpec with Matchers {
  describe("org.ensime.server.RefactoringHandler") {
    it("should format files and preserve encoding") {
      val encoding = "UTF-16"
      implicit val cfg = { dir: File =>
        basicConfig(dir, jars = false, compilerArgs = List("-encoding", encoding))
      }
      Helpers.withAnalyzer { (dir, analyzerRef) =>
        val file = Helpers.srcFile(dir, "abc.scala", Helpers.contents(
          "package blah",
          "   class Something {",
          "def f(i:   Int) =1",
          " val x = (1\u21922)",
          "   }"
        ), write = true, encoding = encoding)

        val analyzer = analyzerRef.underlyingActor

        analyzer.handleFormatFiles(List(file.path))
        val fileContents = Helpers.readSrcFile(file, encoding)

        val expectedContents = Helpers.contents(
          "package blah",
          "class Something {",
          "  def f(i: Int) = 1",
          "  val x = (1 \u2192 2)",
          "}"
        )
        assert(fileContents === expectedContents)
      }
    }
  }

  it("should format files from ContentsSourceFileInfo with handleFormatFile") {
    Helpers.withAnalyzer { (dir, analyzerRef) =>
      val contents = Helpers.contents(
        "package blah",
        "   class  Something   {}"
      )
      val analyzer = analyzerRef.underlyingActor

      val formatted = analyzer.handleFormatFile(ContentsSourceFileInfo(new File("abc.scala"), contents))
      val expectedContents = Helpers.contents(
        "package blah",
        "class Something {}",
        ""
      )
      assert(formatted === expectedContents)
    }
  }

  it("should format files from ContentsInSourceFileInfo with handleFormatFile and handle encoding") {
    Helpers.withAnalyzer { (dir, analyzerRef) =>
      val encoding = "UTF-16"
      implicit val cfg = { dir: File =>
        basicConfig(dir, jars = false, compilerArgs = List("-encoding", encoding))
      }
      Helpers.withAnalyzer { (dir, analyzerRef) =>
        val file = Helpers.srcFile(dir, "tmp-contents", Helpers.contents(
          "package blah",
          "   class  Something   {}"
        ), write = true, encoding = encoding)

        val analyzer = analyzerRef.underlyingActor

        val formatted = analyzer.handleFormatFile(ContentsInSourceFileInfo(new File("abc.scala"), new File(file.path)))
        val expectedContents = Helpers.contents(
          "package blah",
          "class Something {}",
          ""
        )
        assert(formatted === expectedContents)
      }
    }
  }

  it("should format files from FileSourceFileInfo with handleFormatFile and handle encoding") {
    Helpers.withAnalyzer { (dir, analyzerRef) =>
      val encoding = "UTF-16"
      implicit val cfg = { dir: File =>
        basicConfig(dir, jars = false, compilerArgs = List("-encoding", encoding))
      }
      Helpers.withAnalyzer { (dir, analyzerRef) =>
        val file = Helpers.srcFile(dir, "abc.scala", Helpers.contents(
          "package blah",
          "   class  Something   {}"
        ), write = true, encoding = encoding)

        val analyzer = analyzerRef.underlyingActor

        val formatted = analyzer.handleFormatFile(FileSourceFileInfo(new File(file.path)))
        val expectedContents = Helpers.contents(
          "package blah",
          "class Something {}",
          ""
        )
        assert(formatted === expectedContents)
      }
    }
  }

}
