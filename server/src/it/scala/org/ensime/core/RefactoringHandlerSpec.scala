package org.ensime.core

import java.io.File
import org.ensime.model.ContentsInSourceFileInfo
import org.ensime.model.ContentsSourceFileInfo
import org.ensime.model.FileSourceFileInfo
import org.scalatest._
import org.ensime.fixture._

class RefactoringHandlerSpec extends WordSpec with Matchers
    with IsolatedAnalyzerFixture with RichPresentationCompilerTestUtils {

  val encoding = "UTF-16"
  def original = EnsimeConfigFixture.EmptyTestProject.copy(
    compilerArgs = List("-encoding", encoding)
  )

  "RefactoringHandler" should {
    "format files and preserve encoding" in withAnalyzer { (config, analyzerRef) =>
      val file = srcFile(config, "abc.scala", contents(
        "package blah",
        "   class Something {",
        "def f(i:   Int) =1",
        " val x = (1\u21922)",
        "   }"
      ), write = true, encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      analyzer.handleFormatFiles(List(file.path))
      val fileContents = readSrcFile(file, encoding)

      val expectedContents = contents(
        "package blah",
        "class Something {",
        "  def f(i: Int) = 1",
        "  val x = (1 \u2192 2)",
        "}"
      )
      assert(fileContents === expectedContents)
    }

    "format files from ContentsSourceFileInfo with handleFormatFile" in withAnalyzer { (dir, analyzerRef) =>
      val content = contents(
        "package blah",
        "   class  Something   {}"
      )
      val analyzer = analyzerRef.underlyingActor

      val formatted = analyzer.handleFormatFile(ContentsSourceFileInfo(new File("abc.scala"), content))
      val expectedContents = contents(
        "package blah",
        "class Something {}",
        ""
      )
      assert(formatted === expectedContents)
    }

    "format files from ContentsInSourceFileInfo with handleFormatFile and handle encoding" in withAnalyzer { (dir, analyzerRef) =>
      val file = srcFile(dir, "tmp-contents", contents(
        "package blah",
        "   class  Something   {}"
      ), write = true, encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val formatted = analyzer.handleFormatFile(ContentsInSourceFileInfo(new File("abc.scala"), new File(file.path)))
      val expectedContents = contents(
        "package blah",
        "class Something {}",
        ""
      )
      assert(formatted === expectedContents)
    }

    "format files from FileSourceFileInfo with handleFormatFile and handle encoding" in withAnalyzer { (dir, analyzerRef) =>
      val file = srcFile(dir, "abc.scala", contents(
        "package blah",
        "   class  Something   {}"
      ), write = true, encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val formatted = analyzer.handleFormatFile(FileSourceFileInfo(new File(file.path)))
      val expectedContents = contents(
        "package blah",
        "class Something {}",
        ""
      )
      assert(formatted === expectedContents)
    }

  }
}
