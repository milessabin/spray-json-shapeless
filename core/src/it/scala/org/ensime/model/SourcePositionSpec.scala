package org.ensime.model

import org.ensime.config._
import org.ensime.api._
import org.ensime.fixture._
import org.ensime.indexer.DatabaseService.FqnSymbol
import org.ensime.indexer.EnsimeVFS
import org.scalatest._
import pimpathon.file._

class SourcePositionSpec extends WordSpec with Matchers
    with SharedEnsimeConfigFixture with SharedEnsimeVFSFixture {
  val original = EnsimeConfigFixture.SimpleTestProject.copy(
    javaLibs = Nil
  )

  "org.ensime.model.SourcePosition" should {
    "resolve FqnSymbols for local files with no line number" in withEnsimeConfig { implicit config =>
      lookup(knownFile) match {
        case Some(LineSourcePosition(name, 0)) if name.isFile =>
        case o => fail(s"not resolved $o")
      }
    }

    "resolve FqnSymbols for local with a line number" in withEnsimeConfig { implicit config =>
      lookup(knownFile, Some(100)) match {
        case Some(LineSourcePosition(name, 100)) if name.isFile =>
        case o => fail(s"not resolved $o")
      }
    }

    "resolve FqnSymbols for archive entries with no line number" in withEnsimeConfig { implicit config =>
      lookup(knownJarEntry) match {
        case Some(LineSourcePosition(name, 0)) if name.isFile =>
        case o => fail(s"not resolved $o")
      }
    }

    "resolve FqnSymbols for archive entries with a line number" in withEnsimeConfig { implicit config =>
      lookup(knownJarEntry, Some(100)) match {
        case Some(LineSourcePosition(name, 100)) if name.isFile =>
        case o => fail(s"not resolved $o")
      }
    }
  }

  def knownFile(implicit config: EnsimeConfig): String = {
    val f = scalaMain / "org/example/Foo.scala"
    "file://" + f
  }

  def knownJarEntry(implicit config: EnsimeConfig): String = {
    val scalatest = config.subprojects.head.referenceSourceJars.find(
      _.getName.contains("scalatest_")
    ).get.getAbsoluteFile
    "jar:" + scalatest + "!/org/scalatest/FunSpec.scala"
  }

  def lookup(uri: String, line: Option[Int] = None)(implicit config: EnsimeConfig) = {
    withVFS { implicit vfs: EnsimeVFS =>
      val sym = FqnSymbol(None, "", "", "", None, None, Some(uri), line, Some(0))
      LineSourcePositionHelper.fromFqnSymbol(sym)
    }
  }
}
