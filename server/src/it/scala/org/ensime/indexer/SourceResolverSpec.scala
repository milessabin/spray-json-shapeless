package org.ensime.indexer

import org.ensime.fixture._
import org.scalatest._
import pimpathon.file._

class SourceResolverSpec extends WordSpec with Matchers with SharedEnsimeVFSFixture
    with SharedSourceResolverFixture with SourceResolverTestUtils {

  def original = EnsimeConfigFixture.SimpleTestProject

  "SourceResolver" should {
    "resolve java sources in J2SE" in withSourceResolver { implicit r =>
      find("java.lang", "String.java") shouldBe Some("/java/lang/String.java")
    }

    "resolve scala sources in the project dependencies" in withSourceResolver { implicit r =>
      find("scala.collection.immutable", "List.scala") shouldBe
        Some("/scala/collection/immutable/List.scala")

      find("org.scalatest", "FunSpec.scala") shouldBe
        Some("/org/scalatest/FunSpec.scala")
    }

    "resolve sources in the project" in withSourceResolver { (c, r) =>
      implicit val config = c
      implicit val resolver = r
      find("org.example.Foo", "Foo.scala") shouldBe
        Some((scalaMain / "org/example/Foo.scala").getAbsolutePath)
    }

    "should resolve files in parent directories in the project" in withSourceResolver { (c, r) =>
      implicit val config = c
      implicit val resolver = r
      find("org.example", "bad-convention.scala") shouldBe
        Some((scalaMain / "bad-convention.scala").getAbsolutePath)
    }
  }
}

trait SourceResolverTestUtils {
  def find(pkg: String, file: String)(implicit resolver: SourceResolver) = {
    import org.ensime.util.RichFileObject._
    resolver.resolve(
      PackageName(pkg.split('.').toList), RawSource(Some(file), None)
    ).map(fo => fo.pathWithinArchive match {
        case None => fo.asLocalFile.getAbsolutePath
        case _ => fo.getName.getPath
      })
  }
}
