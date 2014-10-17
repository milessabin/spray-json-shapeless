package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.scalatest.FunSpec
import org.scalatest.Matchers

class ClassfileIndexerSpec extends FunSpec with Matchers with ClassfileIndexer with SLF4JLogging {

  // TODO: some assertions (currently we're just checking that no exceptions are raised!)

  describe("ClassfileIndexer") {
    it("should support Java 6 class files") {
      indexClassfile(vres("jdk6/Test.class"))
    }

    it("should support Java 8 class files") {
      indexClassfile(vres("jdk8/Test.class"))
      indexClassfile(vres("jdk8/MyAnnotation.class"))
      indexClassfile(vres("jdk8/Test$InnerClassWithCtorParam.class"))
    }

    it("should support typical J2SE classes") {
      val (clazz, refs) = indexClassfile(vres("java/lang/String.class"))
      assert(clazz.access === Public)
    }

    it("should support typical Scala classes") {
      indexClassfile(vres("scala/collection/immutable/List.class"))
      indexClassfile(vres("scala/collection/immutable/List$.class"))
    }
  }
}
