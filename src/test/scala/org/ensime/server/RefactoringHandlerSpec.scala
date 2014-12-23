package org.ensime

import java.io.File
import org.ensime.test.util.Helpers
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.test.TestUtil._

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
}
