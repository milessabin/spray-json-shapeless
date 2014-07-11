package org.ensime.test

import java.io.File
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.ensime.util.ClassHandler
import org.ensime.util.ClassIterator
import scala.collection.mutable

class ClassIteratorSpec extends FunSpec with Matchers {

  describe("ClassIterator") {

    class Collector extends ClassHandler {
      val classes = new mutable.HashSet[String]
      val methods = new mutable.HashSet[String]
      val fields = new mutable.HashSet[String]
      override def onClass(name: String, location: String, flags: Int) {
        classes += name
      }
      override def onMethod(className: String, name: String, location: String, flags: Int) {
        methods += name
      }
      override def onField(className: String, name: String, location: String, flags: Int) {
        fields += name
      }
    }

    it("should find top level symbols in Java 6 class") {
      val f = new File(getClass.getResource("/jdk6/Test.class").getPath)
      val collector = new Collector
      ClassIterator.findPublicSymbols(List(f), collector)
      assert(collector.classes == Set("Test"))
      assert(collector.methods == Set("<init>", "main"))
      assert(collector.fields == Set())
    }

    it("should find top level symbols in Java 8 class") {
      val fs = List(
        "/jdk8/MyAnnotation.class",
        "/jdk8/Test$InnerClassWithCtorParam.class",
        "/jdk8/Test.class").map { s: String => new File(getClass.getResource(s).getPath) }
      val collector = new Collector
      ClassIterator.findPublicSymbols(fs, collector)
      assert(collector.classes ==
        Set("MyAnnotation", "Test$InnerClassWithCtorParam", "Test"))
      assert(collector.methods ==
        Set("<init>", "lambda$closures$0", "closures", "test"))
      assert(collector.fields == Set("field1", "this$0", "field2"))
    }

  }
}
