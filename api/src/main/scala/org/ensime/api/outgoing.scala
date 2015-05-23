package org.ensime.api

import java.io.File

// TODO: outgoing messages should be a sealed family

/**
 * There should be exactly one RpcResponse (or RpcError) in response
 * to an RpcRequest.
 *
 * Unfortunately there is no common family for the msg at this time.
 * See JerkFormats or SwankProtocol for a list of what is supported.
 */
case class RpcResponse(callId: Int, msg: Any)

/**
 * An error occurred when processing an RpcRequest. (In reality most
 * unexpected errors will be timeouts and the real cause will be in
 * the server log).
 */
case class RpcError(
  callId: Int,
  detail: String
)

/**
 * A message that the server can send to the client at any time.
 */
sealed trait EnsimeEvent

//////////////////////////////////////////////////////////////////////
// Contents of the payload

case object DebuggerShutdownEvent

sealed abstract class DebugVmStatus

// must have redundant status: String to match legacy API
case class DebugVmSuccess(
  status: String = "success"
) extends DebugVmStatus
case class DebugVmError(
  errorCode: Int,
  details: String,
  status: String = "error"
) extends DebugVmStatus

sealed trait GeneralSwankEvent extends EnsimeEvent
sealed trait DebugEvent extends EnsimeEvent

/**
 * Generic background notification.
 *
 * NOTE: codes will be deprecated, preferring sealed families.
 */
case class SendBackgroundMessageEvent(
  detail: String,
  code: Int = 105
) extends GeneralSwankEvent

/** The presentation compiler is ready to accept requests. */
case object AnalyzerReadyEvent extends GeneralSwankEvent

/** The presentation compiler has finished analysing the entire project. */
case object FullTypeCheckCompleteEvent extends GeneralSwankEvent

/** The search engine has finished indexing the classpath. */
case object IndexerReadyEvent extends GeneralSwankEvent

/** The presentation compiler was restarted. Existing `:type-id`s are invalid. */
case object CompilerRestartedEvent extends GeneralSwankEvent

/** The presentation compiler has invalidated all existing notes.  */
case object ClearAllScalaNotesEvent extends GeneralSwankEvent

/** The presentation compiler is providing notes: e.g. errors, warnings. */
case class NewScalaNotesEvent(
  isFull: Boolean,
  notes: List[Note]
) extends GeneralSwankEvent

/** The debugged VM has stepped to a new location and is now paused awaiting control. */
case class DebugStepEvent(
  threadId: DebugThreadId,
  threadName: String,
  file: File,
  line: Int
) extends DebugEvent

/** The debugged VM has stopped at a breakpoint. */
case class DebugBreakEvent(
  threadId: DebugThreadId,
  threadName: String,
  file: File,
  line: Int
) extends DebugEvent

/** The debugged VM has started. */
case object DebugVMStartEvent extends DebugEvent

/** The debugger has disconnected from the debugged VM. */
case object DebugVMDisconnectEvent extends DebugEvent

/** The debugged VM has thrown an exception and is now paused waiting for control. */
case class DebugExceptionEvent(
  exception: Long,
  threadId: DebugThreadId,
  threadName: String,
  file: Option[File],
  line: Option[Int]
) extends DebugEvent

/** A new thread has started. */
case class DebugThreadStartEvent(threadId: DebugThreadId) extends DebugEvent

/** A thread has died. */
case class DebugThreadDeathEvent(threadId: DebugThreadId) extends DebugEvent

/** Communicates stdout/stderr of debugged VM to client. */
case class DebugOutputEvent(body: String) extends DebugEvent

case object ReloadExistingFilesEvent
case object AskReTypecheck

case object VoidResponse

case class RefactorFailure(
  procedureId: Int,
  reason: String,
  status: scala.Symbol = 'failure // redundant field
)

trait RefactorProcedure {
  def procedureId: Int
  def refactorType: RefactorType
}

case class RefactorEffect(
  procedureId: Int,
  refactorType: RefactorType,
  changes: Seq[FileEdit],
  status: scala.Symbol = 'success // redundant field
) extends RefactorProcedure

case class RefactorResult(
  procedureId: Int,
  refactorType: RefactorType,
  touchedFiles: Seq[File],
  status: scala.Symbol = 'success // redundant field
) extends RefactorProcedure

sealed abstract class RefactorDesc(val refactorType: RefactorType)

case class InlineLocalRefactorDesc(file: File, start: Int, end: Int) extends RefactorDesc(RefactorType.InlineLocal)

case class RenameRefactorDesc(newName: String, file: File, start: Int, end: Int) extends RefactorDesc(RefactorType.Rename)

case class ExtractMethodRefactorDesc(methodName: String, file: File, start: Int, end: Int)
  extends RefactorDesc(RefactorType.ExtractMethod)

case class ExtractLocalRefactorDesc(name: String, file: File, start: Int, end: Int)
  extends RefactorDesc(RefactorType.ExtractLocal)

case class OrganiseImportsRefactorDesc(file: File) extends RefactorDesc(RefactorType.OrganizeImports)

case class AddImportRefactorDesc(qualifiedName: String, file: File)
  extends RefactorDesc(RefactorType.AddImport)

case class SourceFileInfo(
    file: File,
    contents: Option[String] = None,
    contentsIn: Option[File] = None
) {
  // keep the log file sane for unsaved files
  override def toString = s"SourceFileInfo($file,${contents.map(_ => "...")},$contentsIn)"
}

sealed trait PatchOp {
  def start: Int
}

case class PatchInsert(
  start: Int,
  text: String
) extends PatchOp

case class PatchDelete(
  start: Int,
  end: Int
) extends PatchOp

case class PatchReplace(
  start: Int,
  end: Int,
  text: String
) extends PatchOp

sealed trait EntityInfo {
  def name: String
  def members: Iterable[EntityInfo]
}

object SourceSymbol {
  val allSymbols: List[SourceSymbol] = List(
    ObjectSymbol, ClassSymbol, TraitSymbol, PackageSymbol, ConstructorSymbol, ImportedNameSymbol, TypeParamSymbol,
    ParamSymbol, VarFieldSymbol, ValFieldSymbol, OperatorFieldSymbol, VarSymbol, ValSymbol, FunctionCallSymbol,
    ImplicitConversionSymbol, ImplicitParamsSymbol
  )
}

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
case object ImplicitConversionSymbol extends SourceSymbol
case object ImplicitParamsSymbol extends SourceSymbol

sealed trait PosNeeded
case object PosNeededNo extends PosNeeded
case object PosNeededAvail extends PosNeeded
case object PosNeededYes extends PosNeeded

sealed trait SourcePosition
case class EmptySourcePosition() extends SourcePosition
case class OffsetSourcePosition(file: File, offset: Int) extends SourcePosition
case class LineSourcePosition(file: File, line: Int) extends SourcePosition

case class PackageInfo(
    name: String,
    fullName: String,
    // n.b. members should be sorted by name for consistency
    members: Seq[EntityInfo]
) extends EntityInfo {
  require(members == members.sortBy(_.name), "members should be sorted by name")
}

sealed trait SymbolSearchResult {
  def name: String
  def localName: String
  def declAs: DeclaredAs
  def pos: Option[SourcePosition]
}

case class TypeSearchResult(
  name: String,
  localName: String,
  declAs: DeclaredAs,
  pos: Option[SourcePosition]
) extends SymbolSearchResult

case class MethodSearchResult(
  name: String,
  localName: String,
  declAs: DeclaredAs,
  pos: Option[SourcePosition],
  ownerName: String
) extends SymbolSearchResult

// what is the point of these types?
case class ImportSuggestions(symLists: List[List[SymbolSearchResult]])
case class SymbolSearchResults(syms: List[SymbolSearchResult])

case class SymbolDesignations(
  file: File,
  syms: List[SymbolDesignation]
)

case class SymbolDesignation(
  start: Int,
  end: Int,
  symType: SourceSymbol
)

case class SymbolInfo(
    name: String,
    localName: String,
    declPos: Option[SourcePosition],
    `type`: TypeInfo,
    isCallable: Boolean,
    ownerTypeId: Option[Int]
) {
  def tpe = `type`
}

case class Op(
  op: String,
  description: String
)

case class MethodBytecode(
  className: String,
  methodName: String,
  methodSignature: Option[String],
  byteCode: List[Op],
  startLine: Int,
  endLine: Int
)

case class CompletionSignature(
  sections: List[List[(String, String)]],
  result: String
)

case class CompletionInfo(
  name: String,
  typeSig: CompletionSignature,
  typeId: Int,
  isCallable: Boolean,
  relevance: Int,
  toInsert: Option[String]
)

case class CompletionInfoList(
  prefix: String,
  completions: List[CompletionInfo]
)

case class Breakpoint(file: File, line: Int)
case class BreakpointList(active: List[Breakpoint], pending: List[Breakpoint])

case class OffsetRange(from: Int, to: Int)

object OffsetRange extends ((Int, Int) => OffsetRange) {
  def apply(fromTo: Int): OffsetRange = new OffsetRange(fromTo, fromTo)
}

/**
 * A debugger thread id.
 */
case class DebugThreadId(id: Long)

object DebugThreadId {
  /**
   * Create a ThreadId from a String representation
   * @param s A Long encoded as a string
   * @return A ThreadId
   */
  def apply(s: String): DebugThreadId = {
    new DebugThreadId(s.toLong)
  }
}

case class DebugObjectId(id: Long)

object DebugObjectId {
  /**
   * Create a DebugObjectId from a String representation
   * @param s A Long encoded as a string
   * @return A DebugObjectId
   */
  def apply(s: String): DebugObjectId = {
    new DebugObjectId(s.toLong)
  }
}

sealed trait DebugLocation

case class DebugObjectReference(objectId: DebugObjectId) extends DebugLocation

object DebugObjectReference {
  def apply(objId: Long): DebugObjectReference = new DebugObjectReference(DebugObjectId(objId))
}

case class DebugStackSlot(threadId: DebugThreadId, frame: Int, offset: Int) extends DebugLocation

case class DebugArrayElement(objectId: DebugObjectId, index: Int) extends DebugLocation

case class DebugObjectField(objectId: DebugObjectId, field: String) extends DebugLocation

sealed trait DebugValue {
  def typeName: String
}

case class DebugNullValue(
  typeName: String
) extends DebugValue

case class DebugPrimitiveValue(
  summary: String,
  typeName: String
) extends DebugValue

case class DebugObjectInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: DebugObjectId
) extends DebugValue

case class DebugStringInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: DebugObjectId
) extends DebugValue

case class DebugArrayInstance(
  length: Int,
  typeName: String,
  elementTypeName: String,
  objectId: DebugObjectId
) extends DebugValue

case class DebugClassField(
  index: Int,
  name: String,
  typeName: String,
  summary: String
)

case class DebugStackLocal(
  index: Int,
  name: String,
  summary: String,
  typeName: String
)

case class DebugStackFrame(
  index: Int,
  locals: List[DebugStackLocal],
  numArgs: Int,
  className: String,
  methodName: String,
  pcLocation: LineSourcePosition,
  thisObjectId: DebugObjectId
)

case class DebugBacktrace(
  frames: List[DebugStackFrame],
  threadId: DebugThreadId,
  threadName: String
)

case class NamedTypeMemberInfo(
    name: String,
    `type`: TypeInfo,
    pos: Option[SourcePosition],
    signatureString: Option[String],
    declAs: DeclaredAs
) extends EntityInfo {
  override def members = List.empty
  def tpe = `type`
}

sealed trait TypeInfo extends EntityInfo {
  def name: String
  def typeId: Int
  def declAs: DeclaredAs
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
  declAs: DeclaredAs,
  fullName: String,
  typeArgs: Iterable[TypeInfo],
  members: Iterable[EntityInfo],
  pos: Option[SourcePosition],
  outerTypeId: Option[Int]
) extends TypeInfo

case class ArrowTypeInfo(
    name: String,
    typeId: Int,
    resultType: TypeInfo,
    paramSections: Iterable[ParamSectionInfo]
) extends TypeInfo {
  def declAs = DeclaredAs.Nil
  def fullName = name
  def typeArgs = List.empty
  def members = List.empty
  def pos = None
  def outerTypeId = None
}

case class CallCompletionInfo(
  resultType: TypeInfo,
  paramSections: Iterable[ParamSectionInfo]
)

case class ParamSectionInfo(
  params: Iterable[(String, TypeInfo)],
  isImplicit: Boolean
)

case class InterfaceInfo(
    `type`: TypeInfo,
    viaView: Option[String]
) {
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

/** ERangePosition is a mirror of scala compiler internal RangePosition as a case class to */
case class ERangePosition(file: String, offset: Int, start: Int, end: Int)

case class EnsimeImplementation(
  name: String
)
case class ConnectionInfo(
  pid: Option[Int] = None,
  implementation: EnsimeImplementation = EnsimeImplementation("ENSIME"),
  version: String = "0.8.16"
)

sealed trait ImplicitInfo {
  def start: Int
  def end: Int
}

case class ImplicitConversionInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo
) extends ImplicitInfo

case class ImplicitParamInfo(
  start: Int,
  end: Int,
  fun: SymbolInfo,
  params: List[SymbolInfo],
  funIsImplicit: Boolean
) extends ImplicitInfo
