package org.ensime.server.protocol.swank.sexp.formats

import java.io.File
import java.net.URI
import java.util.Date
import java.util.UUID

import org.ensime.server.protocol.swank.sexp._
import org.ensime.util.RichFile._

class StandardFormatsSpec extends FormatSpec with StandardFormats with BasicFormats {
  describe("StandardFormats") {
    it("should support Option") {
      val some = Some("thing")
      assertFormat(some: Option[String], SexpList(SexpString("thing")))
      assertFormat(None: Option[String], SexpNil)
    }

    it("should support Either") {
      val left = Left(13)
      val right = Right("thirteen")
      assertFormat(left: Either[Int, String],
        SexpData(SexpSymbol(":left") -> SexpNumber(13)))
      assertFormat(right: Either[Int, String],
        SexpData(SexpSymbol(":right") -> SexpString("thirteen")))
    }

    it("should support UUID") {
      val uuid = UUID.randomUUID()
      assertFormat(uuid, SexpString(uuid.toString))
    }

    // it("should support URL") {
    //   val github = "http://github.com/ensime/"
    //   val url = new URL(github)
    //   // hack to avoid calling URL.equals, which talks to the interwebz
    //   assert(url.toSexp === SexpString(github))
    //   assert(SexpString(github).convertTo[URL].toExternalForm === github)
    // }

    it("should support URI") {
      val github = "http://github.com/ensime/"
      val url = new URI(github)
      assertFormat(url, SexpString(github))
    }

    it("should support File") {
      val file = new File("foo")
      assertFormat(file, SexpString("foo"))
    }

    it("should support Date") {
      val date = new Date(1414326493000L)
      assertFormat(date, SexpString("2014-10-26T12:28:13+00:00"))

      val unix = new Date(0L)
      assertFormat(unix, SexpString("1970-01-01T00:00:00+00:00"))
    }
  }
}

class AltStandardFormatsSpec extends FormatSpec
    with StandardFormats with CanonFileFormat {

  describe("StandardFormats with CanonFileFormat") {
    it("should support canonicalised Files") {
      val file = new File("foo")
      val canon = file.canon
      // non-canon files won't deserialise into exactly the same file
      assert(file.toSexp === SexpString(canon.getPath))
      assertFormat(canon, SexpString(canon.getPath))
    }
  }

}
