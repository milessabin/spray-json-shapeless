package org.ensime.model

import java.io.File
import org.ensime.util.FileEdit
import scala.collection.mutable

import org.ensime.config._

sealed trait EntityInfo {
  def name: String
  def members: Iterable[EntityInfo]
}

object SourceSymbol {
  val allSymbols: Set[SourceSymbol] = Set(
    ObjectSymbol, ClassSymbol, TraitSymbol, PackageSymbol, ConstructorSymbol, ImportedNameSymbol, TypeParamSymbol,
    ParamSymbol, VarFieldSymbol, ValFieldSymbol, OperatorFieldSymbol, VarSymbol, ValSymbol, FunctionCallSymbol)
}

sealed trait RefactorType

sealed trait SourceSymbol

case object ObjectSymbol extends SourceSymbol
case object ClassSymbol extends SourceSymbol
case object TraitSymbol extends SourceSymbol
case object PackageSymbol extends SourceSymbol
case object ConstructorSymbol extends SourceSymbol
case object ImportedNameSymbol extends SourceSymbol
case object TypeParamSymbol extends SourceSymbol
case object ParamSymbol extends SourceSymbol
case object VarFieldSymbol extends SourceSymbol
case object ValFieldSymbol extends SourceSymbol
case object OperatorFieldSymbol extends SourceSymbol
case object VarSymbol extends SourceSymbol
case object ValSymbol extends SourceSymbol
case object FunctionCallSymbol extends SourceSymbol

sealed trait PosNeeded
case object PosNeededNo extends PosNeeded
case object PosNeededAvail extends PosNeeded
case object PosNeededYes extends PosNeeded

sealed trait SourcePosition
case class EmptySourcePosition() extends SourcePosition
case class OffsetSourcePosition(file: File, offset: Int) extends SourcePosition
case class LineSourcePosition(file: File, line: Int) extends SourcePosition

sealed trait SourceFileInfo {
  def file: File
}
case class FileSourceFileInfo(file: File) extends SourceFileInfo
case class ContentsSourceFileInfo(file: File, contents: String) extends SourceFileInfo
case class ContentsInSourceFileInfo(file: File, contentsIn: File) extends SourceFileInfo

case class PackageInfo(
    name: String,
    fullName: String,
    // n.b. members should be sorted by name for consistency
    members: Seq[EntityInfo]) extends EntityInfo {
  require(members == members.sortBy(_.name), "members should be sorted by name")
}

trait SymbolSearchResult {
  def name: String
  def localName: String
  def declAs: scala.Symbol
  def pos: Option[SourcePosition]
}

case class TypeSearchResult(
  name: String,
  localName: String,
  declAs: scala.Symbol,
  pos: Option[SourcePosition]) extends SymbolSearchResult

case class MethodSearchResult(
  name: String,
  localName: String,
  declAs: scala.Symbol,
  pos: Option[SourcePosition],
  ownerName: String) extends SymbolSearchResult

// what is the point of these types?
case class ImportSuggestions(symLists: List[List[SymbolSearchResult]])
case class SymbolSearchResults(syms: List[SymbolSearchResult])

case class SymbolDesignations(
  file: String,
  syms: List[SymbolDesignation])

case class SymbolDesignation(
  start: Int,
  end: Int,
  symType: SourceSymbol)

case class SymbolInfo(
    name: String,
    localName: String,
    declPos: Option[SourcePosition],
    `type`: TypeInfo,
    isCallable: Boolean,
    ownerTypeId: Option[Int]) {
  def tpe = `type`
}

case class Op(
  op: String,
  description: String)

case class MethodBytecode(
  className: String,
  methodName: String,
  methodSignature: Option[String],
  byteCode: List[Op],
  startLine: Int,
  endLine: Int)

case class CompletionSignature(
  sections: List[List[(String, String)]],
  result: String)

case class CompletionInfo(
  name: String,
  typeSig: CompletionSignature,
  typeId: Int,
  isCallable: Boolean,
  relevance: Int,
  toInsert: Option[String])

case class CompletionInfoList(
  prefix: String,
  completions: List[CompletionInfo])

sealed trait PatchOp {
  def start: Int
}

case class PatchInsert(
  start: Int,
  text: String) extends PatchOp

case class PatchDelete(
  start: Int,
  end: Int) extends PatchOp

case class PatchReplace(
  start: Int,
  end: Int,
  text: String) extends PatchOp

case class Breakpoint(file: File, line: Int)
case class BreakpointList(active: List[Breakpoint], pending: List[Breakpoint])

case class OffsetRange(from: Int, to: Int)

object OffsetRange {
  def apply(fromTo: Int): OffsetRange = new OffsetRange(fromTo, fromTo)
}

sealed trait DebugLocation

case class DebugObjectReference(objectId: Long) extends DebugLocation

case class DebugStackSlot(threadId: String, frame: Int, offset: Int) extends DebugLocation

case class DebugArrayElement(objectId: Long, index: Int) extends DebugLocation

case class DebugObjectField(objectId: Long, field: String) extends DebugLocation

sealed trait DebugValue {
  def typeName: String
}

case class DebugNullValue(
  typeName: String) extends DebugValue

case class DebugPrimitiveValue(
  summary: String,
  typeName: String) extends DebugValue

case class DebugObjectInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: Long) extends DebugValue

case class DebugStringInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: Long) extends DebugValue

case class DebugArrayInstance(
  length: Int,
  typeName: String,
  elementTypeName: String,
  objectId: Long) extends DebugValue

case class DebugClassField(
  index: Int,
  name: String,
  typeName: String,
  summary: String)

case class DebugStackLocal(
  index: Int,
  name: String,
  summary: String,
  typeName: String)

case class DebugStackFrame(
  index: Int,
  locals: List[DebugStackLocal],
  numArgs: Int,
  className: String,
  methodName: String,
  pcLocation: LineSourcePosition,
  thisObjectId: Long)

case class DebugBacktrace(
  frames: List[DebugStackFrame],
  threadId: String,
  threadName: String)

case class NamedTypeMemberInfo(
    name: String,
    `type`: TypeInfo,
    pos: Option[SourcePosition],
    declAs: scala.Symbol) extends EntityInfo {
  override def members = List.empty
  def tpe = `type`
}

case class PackageMemberInfoLight(name: String)

sealed trait TypeInfo extends EntityInfo {
  def name: String
  def typeId: Int
  def declAs: scala.Symbol
  def fullName: String
  def typeArgs: Iterable[TypeInfo]
  def members: Iterable[EntityInfo]
  def pos: Option[SourcePosition]
  def outerTypeId: Option[Int]

  final def declaredAs = declAs
  final def args = typeArgs
}

case class BasicTypeInfo(
  name: String,
  typeId: Int,
  declAs: scala.Symbol,
  fullName: String,
  typeArgs: Iterable[TypeInfo],
  members: Iterable[EntityInfo],
  pos: Option[SourcePosition],
  outerTypeId: Option[Int]) extends TypeInfo

case class ArrowTypeInfo(
    name: String,
    typeId: Int,
    resultType: TypeInfo,
    paramSections: Iterable[ParamSectionInfo]) extends TypeInfo {
  def declAs = 'nil
  def fullName = name
  def typeArgs = List.empty
  def members = List.empty
  def pos = None
  def outerTypeId = None
}

case class CallCompletionInfo(
  resultType: TypeInfo,
  paramSections: Iterable[ParamSectionInfo])

case class ParamSectionInfo(
  params: Iterable[(String, TypeInfo)],
  isImplicit: Boolean)

case class InterfaceInfo(
    `type`: TypeInfo,
    viaView: Option[String]) {
  def tpe = `type`
}

case class TypeInspectInfo(
    `type`: TypeInfo,
    companionId: Option[Int],
    interfaces: Iterable[InterfaceInfo],
    infoType: scala.Symbol = 'typeInspect // redundant field in protocol
    ) {
  def supers = interfaces
}

case class AddUndo(summary: String, changes: List[FileEdit])
case class Undo(id: Int, summary: String, changes: List[FileEdit])
case class UndoResult(id: Int, touchedFiles: List[File])

/** ERangePosition is a mirror of scala compiler internal RangePosition as a case class to */
case class ERangePosition(file: String, offset: Int, start: Int, end: Int)

/**
 * Information necessary to create a javadoc or scaladoc URI for a
 * particular type or type member.
 */
case class DocFqn(pack: String, typeName: String) {
  def mkString: String = if (pack.isEmpty) typeName else pack + "." + typeName
  def inPackage(prefix: String): Boolean = pack == prefix || pack.startsWith(prefix + ".")
  def javaStdLib: Boolean = inPackage("java") || inPackage("javax")
  def scalaStdLib: Boolean = inPackage("scala")
}
case class DocSig(fqn: DocFqn, member: Option[String])

/**
 * We generate DocSigs for java and scala at the same time, since we
 * don't know a priori whether the docs will be in scaladoc or javadoc
 * format.
 */
case class DocSigPair(scala: DocSig, java: DocSig)
