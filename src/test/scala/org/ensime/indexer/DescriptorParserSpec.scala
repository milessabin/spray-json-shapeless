package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.scalatest.FunSpec
import org.scalatest.Matchers
import DescriptorParser.{ parse, parseType }
import ClassName._

class DescriptorParserSpec extends FunSpec with Matchers with SLF4JLogging {

  private val S = ClassName(PackageName(List("java", "lang")), "String")
  private val A = ArrayDescriptor
  private val D = Descriptor
  private val I = PrimitiveInt
  private val V = PrimitiveVoid
  private val Z = PrimitiveBoolean
  private val root = PackageName(Nil)

  describe("DescriptorParser") {
    it("should fail to parse the empty string") {
      assert(parse("") === None)
    }

    it("should fail to parse a bad string") {
      assert(parse("not valid") === None)
    }

    it("should parse descriptors without parameters") {
      assert(parse("()V") === Some(D(Nil, PrimitiveVoid)))
      assert(parse("()Ljava/lang/String;") === Some(D(Nil, S)))
      assert(parse("()[Ljava/lang/String;") === Some(D(Nil, A(S))))
      assert(parse("()[[Ljava/lang/String;") === Some(D(Nil, A(A(S)))))
      assert(parse("()[[[Ljava/lang/String;") === Some(D(Nil, A(A(A(S))))))
    }

    it("should handle multiple object parameters") {
      assert(parse("(I[IILjava/lang/String;Z)V") === Some(D(List(I, A(I), I, S, Z), V)))
    }

    it("should be invertable") {
      def invert(desc: String) =
        assert(parse(desc).map(_.descriptorString) === Some(desc))

      invert("(I[IILjava/lang/String;Z)V")
    }
  }

  describe("DescriptorParser's JVM internal mode") {
    it("should fail to parse the empty string") {
      assert(parseType("") === None)
    }

    it("should fail to parse a bad string") {
      assert(parseType("not valid") === None)
    }

    it("should handle examples") {
      assert(parseType("Ljava/lang/String;") === Some(S))
      assert(parseType("[Ljava/lang/String;") === Some(A(S)))
      assert(parseType("[[Ljava/lang/String;") === Some(A(A(S))))
      assert(parseType("V") === Some(V))
      assert(parseType("LMyAnnotation;") === Some(ClassName(root, "MyAnnotation")))

      // of course, SUN break their own rules for package names (capitals)
      assert(parseType("Lcom/sun/tools/corba/se/idl/toJavaPortable/NameModifierImpl;").nonEmpty)
    }

    it("should be invertable") {
      def invert(desc: String) =
        assert(parseType(desc).map(_.internalString) === Some(desc))

      invert("Ljava/lang/String;")
      invert("[[Ljava/lang/String;")
    }
  }

}
