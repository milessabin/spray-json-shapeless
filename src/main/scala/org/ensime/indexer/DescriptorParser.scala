package org.ensime.indexer

import org.ensime.util.ParboiledParser
import org.parboiled.errors.ErrorUtils
import org.parboiled.errors.ParsingException
import org.parboiled.scala._
import ClassName._

/**
 * Parser for Java Descriptors as defined in Section 4.3 of
 * http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html
 */
object DescriptorParser extends ParboiledParser[Descriptor] {
  private val TypeParser = local(ReportingParseRunner(Type))

  def parseType(desc: String): DescriptorType = {
    val parsingResult = TypeParser.get.run(desc)
    parsingResult.result.getOrElse {
      throw new ParsingException(
        "Invalid :\n" + ErrorUtils.printParseErrors(parsingResult)
      )
    }
  }

  protected def Top: Rule1[Descriptor] = rule("Top") {
    "(" ~ zeroOrMore(Type) ~ ")" ~ Type
  } ~~> { (params, ret) => Descriptor(params, ret) }

  private def Type: Rule1[DescriptorType] = rule("Type") {
    (Class | Primitive | Array)
  }

  private def Array: Rule1[DescriptorType] = rule("Array") {
    ch('[') ~ (Class | Primitive | Array)
  } ~~> { c => ArrayDescriptor(c) }

  private def Class: Rule1[DescriptorType] = rule("Class") {
    "L" ~ Package ~ Name ~ ";"
  } ~~> { (p, n) => ClassName(p, n) }

  private def Package: Rule1[PackageName] = rule("Package") {
    zeroOrMore(oneOrMore("a" - "z" | "A" - "Z" | "_" | "0" - "9").save ~ "/")
  } ~~> { PackageName.apply }

  private def Name: Rule1[String] = rule("Name") {
    oneOrMore(noneOf(";/()"))
  } save

  private def Primitive: Rule1[DescriptorType] = rule("Primitive") {
    Boolean | Byte | Char | Short | Int | Long | Float | Double | Void
  }

  private def Boolean: Rule1[ClassName] = rule { ch('Z') as PrimitiveBoolean }
  private def Byte: Rule1[ClassName] = rule { ch('B') as PrimitiveByte }
  private def Char: Rule1[ClassName] = rule { ch('C') as PrimitiveChar }
  private def Short: Rule1[ClassName] = rule { ch('S') as PrimitiveShort }
  private def Int: Rule1[ClassName] = rule { ch('I') as PrimitiveInt }
  private def Long: Rule1[ClassName] = rule { ch('J') as PrimitiveLong }
  private def Float: Rule1[ClassName] = rule { ch('F') as PrimitiveFloat }
  private def Double: Rule1[ClassName] = rule { ch('D') as PrimitiveDouble }
  private def Void: Rule1[ClassName] = rule { ch('V') as PrimitiveVoid }

}
