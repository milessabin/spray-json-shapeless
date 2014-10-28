package org.ensime.indexer

import org.objectweb.asm.Opcodes._
import collection.immutable.Queue

import pimpathon.option._

sealed trait Access
case object Public extends Access
case object Default extends Access
case object Protected extends Access
case object Private extends Access
object Access {
  def apply(code: Int): Access =
    if ((ACC_PUBLIC & code) > 0) Public
    else if ((ACC_PROTECTED & code) > 0) Protected
    else if ((ACC_PRIVATE & code) > 0) Private
    else Default
}

sealed trait FullyQualifiedName {
  def contains(o: FullyQualifiedName): Boolean
  def fqnString: String
}
case class PackageName(path: List[String]) extends FullyQualifiedName {
  def contains(o: FullyQualifiedName) = o match {
    case PackageName(o) => o.startsWith(path)
    case ClassName(p, _) => contains(p)
    case MemberName(c, _) => contains(c)
  }
  def fqnString = path.mkString(".")
  def parent = PackageName(path.init)
}

case class ClassName(pack: PackageName, name: String) extends FullyQualifiedName with DescriptorType {
  def contains(o: FullyQualifiedName) = o match {
    case ClassName(op, on) if pack == op & on.startsWith(name) =>
      (on == name) || on.startsWith(name + "$")
    case MemberName(cn, _) => contains(cn)
    case _ => false
  }
  def fqnString = if (pack.path.isEmpty) name else pack.fqnString + "." + name
  def internalString =
    "L" + (if (pack.path.isEmpty) name else pack.path.mkString("/") + "/" + name) + ";"
}
object ClassName {
  private val Root = PackageName(Nil)
  // we consider Primitives to be ClassNames
  private def Primitive(name: String, desc: String): ClassName = new ClassName(Root, name) {
    override def fqnString = name
    override def internalString = desc
  }

  val PrimitiveBoolean = Primitive("boolean", "Z")
  val PrimitiveByte = Primitive("byte", "B")
  val PrimitiveChar = Primitive("char", "C")
  val PrimitiveShort = Primitive("short", "S")
  val PrimitiveInt = Primitive("int", "I")
  val PrimitiveLong = Primitive("long", "J")
  val PrimitiveFloat = Primitive("float", "F")
  val PrimitiveDouble = Primitive("double", "D")
  val PrimitiveVoid = Primitive("void", "V")

  // must be a single type descriptor
  // strips array reification
  def fromDescriptor(desc: String): ClassName =
    DescriptorParser.parseType(desc) match {
      case c: ClassName => c
      case a: ArrayDescriptor => a.reifier
    }

  // internal name is effectively the FQN with / instead of dots
  def fromInternal(internal: String): ClassName = {
    val parts = internal.split("/")
    val (before, after) = parts.splitAt(parts.length - 1)
    ClassName(PackageName(before.toList), after(0))
  }
}

case class MemberName(
    owner: ClassName,
    name: String) extends FullyQualifiedName {
  def contains(o: FullyQualifiedName) = this == o
  def fqnString = owner.fqnString + "." + name
}

sealed trait DescriptorType {
  def internalString: String
}
// ClassName implements DescriptorType
case class ArrayDescriptor(fqn: DescriptorType) extends DescriptorType {
  def reifier: ClassName = fqn match {
    case c: ClassName => c
    case a: ArrayDescriptor => a.reifier
  }
  def internalString = "[" + fqn.internalString
}
case class Descriptor(params: List[DescriptorType], ret: DescriptorType) {
  def descriptorString =
    "(" + params.map(_.internalString).mkString("") + ")" + ret.internalString
}

// TODO: replace generics Strings with domain objects
case class RawClassfile(
  name: ClassName,
  generics: Option[String],
  superClass: Option[ClassName],
  interfaces: List[ClassName],
  access: Access,
  deprecated: Boolean,
  fields: Queue[RawField],
  methods: Queue[RawMethod],
  source: RawSource)

case class RawSource(
  filename: Option[String],
  line: Option[Int])

case class RawField(
  name: MemberName,
  clazz: ClassName,
  generics: Option[String],
  access: Access)

case class RawMethod(
  name: MemberName,
  access: Access,
  descriptor: Descriptor,
  generics: Option[String],
  line: Option[Int])
