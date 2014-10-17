package org.ensime.indexer

import java.io.File
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.config._
import org.ensime.test.TestUtil._
import pimpathon.file._
import pimpathon.any._
import org.ensime.util.RichFile._

class SourceResolverSpec extends FunSpec with Matchers {

  def genProject(base: File): EnsimeConfig = {
    basicConfig(base, sources = true, testSources = true) withSideEffect {
      config =>
        // https://github.com/stacycurl/pimpathon/issues/111
        // https://github.com/stacycurl/pimpathon/issues/112
        (config.root / mainSourcePath / "bad-convention.scala").
          outputStream().write(Array.ofDim[Byte](0))
    }
  }

  val base = tempDir().canon
  val config = genProject(base)

  val resolver = new SourceResolver(config)

  def find(pkg: String, file: String) = {
    import org.ensime.util.RichFileObject._
    resolver.resolve(
      PackageName(pkg.split('.').toList), RawSource(Some(file), None)
    ).map(fo => fo.pathWithinArchive match {
        case None => fo.asLocalFile.getAbsolutePath
        case _ => fo.getName.getPath()
      })
  }

  describe("SourceResolver") {
    it("should resolve java sources in J2SE", NotOnTravis) {
      assert(find("java.lang", "String.java") === Some("/java/lang/String.java"))
    }

    it("should resolve scala sources in the project dependencies") {
      assert(find("scala.collection.immutable", "List.scala") === Some("/scala/collection/immutable/List.scala"))
      assert(find("org.scalatest", "FunSpec.scala") === Some("/org/scalatest/FunSpec.scala"))
    }

    it("should resolve sources in the project") {
      assert(find("org.ensime.indexer", "SourceResolver.scala") ===
        Some((base / "src/main/scala" / "org/ensime/indexer/SourceResolver.scala").getAbsolutePath))
      assert(find("org.ensime.indexer", "SourceResolverSpec.scala") ===
        Some((base / "src/test/scala" / "org/ensime/indexer/SourceResolverSpec.scala").getAbsolutePath))
    }

    it("should resolve files in parent directories in the project") {
      assert(find("org.ensime.indexer", "bad-convention.scala") ===
        Some((base / "src/main/scala" / "bad-convention.scala").getAbsolutePath))
    }
  }
}
