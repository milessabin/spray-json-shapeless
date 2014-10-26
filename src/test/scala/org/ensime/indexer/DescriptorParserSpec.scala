package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.scalatest.FunSpec
import org.scalatest.Matchers
import DescriptorParser.{ parse, parseType }
import ClassName._
import scala.util.Try

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
      assert(Try(parse("")).isFailure)
    }

    it("should fail to parse a bad string") {
      assert(Try(parse("not valid")).isFailure)
    }

    it("should parse descriptors without parameters") {
      assert(parse("()V") === D(Nil, PrimitiveVoid))
      assert(parse("()Ljava/lang/String;") === D(Nil, S))
      assert(parse("()[Ljava/lang/String;") === D(Nil, A(S)))
      assert(parse("()[[Ljava/lang/String;") === D(Nil, A(A(S))))
      assert(parse("()[[[Ljava/lang/String;") === D(Nil, A(A(A(S)))))
    }

    it("should handle multiple object parameters") {
      assert(parse("(I[IILjava/lang/String;Z)V") === D(List(I, A(I), I, S, Z), V))
    }

    it("should be invertable") {
      def invert(desc: String) =
        assert(parse(desc).descriptorString === desc)

      invert("(I[IILjava/lang/String;Z)V")
    }
  }

  describe("DescriptorParser's JVM internal mode") {
    it("should fail to parse the empty string") {
      assert(Try(parseType("")).isFailure)
    }

    it("should fail to parse a bad string") {
      assert(Try(parseType("not valid")).isFailure)
    }

    it("should handle examples") {
      assert(parseType("Ljava/lang/String;") === S)
      assert(parseType("[Ljava/lang/String;") === A(S))
      assert(parseType("[[Ljava/lang/String;") === A(A(S)))
      assert(parseType("V") === V)
      assert(parseType("LMyAnnotation;") === ClassName(root, "MyAnnotation"))

      // of course, SUN break their own rules for package names (capitals)
      assert(Try(parseType("Lcom/sun/tools/corba/se/idl/toJavaPortable/NameModifierImpl;")).isSuccess)
    }

    it("should be invertable") {
      def invert(desc: String) =
        assert(parseType(desc).internalString === desc)

      invert("Ljava/lang/String;")
      invert("[[Ljava/lang/String;")
    }
  }

}
