package org.ensime.test
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.protocol.SwankProtocol
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.io.{ VirtualFile, PlainFile, ZipArchive }

class SwankProtocolSpec extends FunSpec with Matchers {

  class MockZipEntry(entry: String, archive: ZipArchive) extends VirtualFile(entry, entry) {
    override def underlyingSource: Option[ZipArchive] = Some(archive)
  }

  describe("SwankProtocol") {
    it("can convert a Position") {
      val f = new BatchSourceFile(new PlainFile("stuff"), "ABCDEF")
      val p = f.position(2)
      val s = SwankProtocol.SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :offset 2)"""
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a Position in a ZipArchive entry") {
      val a = ZipArchive.fromPath("stuff.zip")
      val f = new BatchSourceFile(new MockZipEntry("stuff", a), "ABCDEF")
      val p = f.position(2)
      val s = SwankProtocol.SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :archive "stuff.zip" :offset 2)"""
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a RangePosition") {
      val f = new BatchSourceFile(new PlainFile("stuff"), "ABCDEF")
      val p = new RangePosition(f, 1, 2, 3)
      val s = SwankProtocol.SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :offset 2 :start 1 :end 3)"""
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }

    it("can convert a RangePosition in a ZipArchive entry") {
      val a = ZipArchive.fromPath("stuff.zip")
      val f = new BatchSourceFile(new MockZipEntry("stuff", a), "ABCDEF")
      val p = new RangePosition(f, 1, 2, 3)
      val s = SwankProtocol.SExpConversion.posToSExp(p)
      val expected = """(:file "stuff" :archive "stuff.zip" :offset 2 :start 1 :end 3)"""
      val got = s.toReadableString(debug = false)
      assert(got == expected, got + " != " + expected)
    }
  }
}
