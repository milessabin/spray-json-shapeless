package org.ensime.model

import org.ensime.indexer.DatabaseService.FqnSymbol
import org.ensime.test.TestUtil
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.apache.commons.vfs2._
import TestUtil._

import pimpathon.file._

class SourcePositionSpec extends FunSpec with Matchers {

  val vfs = VFS.getManager
  implicit val config = basicConfig()
  val module = config.modules.values.head
  val f = module.sourceRoots.head / "blah.scala"
  f.writeLines(Nil)
  val r = vfs.toFileObject(f)
  val scalatest = module.referenceSourcesJars.find(_.getName.contains("scalatest_")).get.getAbsoluteFile
  val jarentry = "jar:" + scalatest + "!/org/scalatest/FunSpec.scala"

  private def lookup(uri: String, line: Option[Int] = None) = {
    val sym = FqnSymbol(None, "", "", "", None, None, Some(uri), line, Some(0))
    LineSourcePosition.fromFqnSymbol(sym)
  }

  describe("org.ensime.model.SourcePosition") {
    it("should resolve FqnSymbols for local files with no line number") {
      lookup(r.getName.getURI) match {
        case Some(LineSourcePosition(name, 0)) if name.getPath == f.getPath =>
        case o => fail(s"not resolved $o")
      }
    }

    it("should resolve FqnSymbols for local with a line number") {
      lookup(r.getName.getURI, Some(100)) match {
        case Some(LineSourcePosition(name, 100)) if name.getPath == f.getPath =>
        case o => fail(s"not resolved $o")
      }
    }

    it("should resolve FqnSymbols for archive entries with no line number") {
      lookup(jarentry) match {
        case Some(LineSourcePosition(name, 0)) if name.isFile =>
        case o => fail(s"not resolved $o")
      }
    }

    it("should resolve FqnSymbols for archive entries with a line number") {
      lookup(jarentry, Some(100)) match {
        case Some(LineSourcePosition(name, 100)) if name.isFile =>
        case o => fail(s"not resolved $o")
      }
    }
  }
}
