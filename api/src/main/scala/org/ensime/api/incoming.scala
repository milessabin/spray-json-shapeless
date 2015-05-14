package org.ensime.api

import java.io.File

// TODO: resp should be a sealed family
//case class RpcResponseEnvelope(resp: Sexp, callId: Int)
//case class RpcError(code: Int, detail: String, callId: Int)
//case class ProtocolError(code: Int, detail: String)

case class RpcRequestEnvelope(req: RpcRequest, callId: Int)

/**
 * All messages into the ENSIME server from the client are part of
 * this family.
 *
 * TODO: we intend to simplify these messages
 * https://github.com/ensime/ensime-server/issues/845
 */
sealed trait RpcRequest

// queries related to connection startup
sealed trait RpcStartupRequest extends RpcRequest
case object ConnectionInfoReq extends RpcStartupRequest
case object InitProjectReq extends RpcStartupRequest

// related to managing the state of the analyser
sealed trait RpcAnalyserRequest extends RpcRequest
case class RemoveFileReq(file: File) extends RpcAnalyserRequest
case class TypecheckFileReq(fileInfo: SourceFileInfo) extends RpcAnalyserRequest
case object UnloadAllReq extends RpcAnalyserRequest
case object TypecheckAllReq extends RpcAnalyserRequest

// related to source formatting
sealed trait RpcFormatRequest extends RpcRequest
case class FormatSourceReq(files: List[File]) extends RpcFormatRequest
// TODO: format a region of a file

// related to searching the indexer
sealed trait RpcSearchRequest extends RpcRequest
case class PublicSymbolSearchReq(
  keywords: List[String],
  maxResults: Int
) extends RpcSearchRequest
case class ImportSuggestionsReq(
  file: File,
  point: Int,
  names: List[String],
  maxResults: Int
) extends RpcSearchRequest
case class UsesOfSymbolAtPointReq(
  file: File,
  point: Int
) extends RpcSearchRequest

// related to obtaining documentation
sealed trait RpcDocRequest extends RpcRequest
case class DocUriAtPointReq(
  file: File,
  point: OffsetRange
) extends RpcDocRequest
case class DocUriForSymbolReq(
  typeFullName: String,
  memberName: Option[String],
  signatureString: Option[String]
) extends RpcDocRequest

// related to completions of the user's current context
sealed trait RpcCompletionRequest extends RpcRequest
case class CompletionsReq(
  fileInfo: SourceFileInfo,
  point: Int,
  maxResults: Int,
  caseSens: Boolean,
  reload: Boolean
) extends RpcCompletionRequest
case class PackageMemberCompletionReq(
  path: String,
  prefix: String
) extends RpcCompletionRequest
case class CallCompletionReq(id: Int) extends RpcCompletionRequest

// related to discovering the type information of the user's current
// context (this is a great candidate for simplification and much of
// this should probably be listed under the deprecated trait)
sealed trait RpcTypeRequest extends RpcRequest
case class TypeByIdReq(id: Int) extends RpcTypeRequest
case class TypeByNameReq(name: String) extends RpcTypeRequest
case class TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) extends RpcTypeRequest
case class TypeAtPointReq(file: File, range: OffsetRange) extends RpcTypeRequest
case class InspectTypeAtPointReq(file: File, range: OffsetRange) extends RpcTypeRequest
case class InspectTypeByIdReq(id: Int) extends RpcTypeRequest
case class InspectTypeByNameReq(name: String) extends RpcTypeRequest
case class SymbolAtPointReq(file: File, point: Int) extends RpcTypeRequest
case class SymbolByNameReq(
  typeFullName: String,
  memberName: Option[String],
  signatureString: Option[String]
) extends RpcTypeRequest
case class InspectPackageByPathReq(path: String) extends RpcTypeRequest

// related to refactorings
sealed trait RpcRefactorRequest extends RpcRequest
case class PrepareRefactorReq(
  procId: Int,
  tpe: Symbol, // tpe is ignored but part of the legacy wire format
  params: RefactorDesc,
  interactive: Boolean
) extends RpcRefactorRequest
case class ExecRefactorReq(procId: Int, tpe: RefactorType) extends RpcRefactorRequest
case class CancelRefactorReq(procId: Int) extends RpcRefactorRequest

// related to discovery of the user's context
sealed trait RpcContextRequest extends RpcRequest
case class SymbolDesignationsReq(
  file: File,
  start: Int,
  end: Int,
  requestedTypes: List[SourceSymbol]
) extends RpcContextRequest
case class ExpandSelectionReq(file: File, start: Int, end: Int) extends RpcContextRequest

// related to the lifecycle of the debugger
sealed trait RpcDebugLifecycleRequest extends RpcRequest
case object DebugActiveVmReq extends RpcDebugLifecycleRequest
case class DebugStartReq(commandLine: String) extends RpcDebugLifecycleRequest
case class DebugAttachReq(hostname: String, port: String) extends RpcDebugLifecycleRequest
case object DebugStopReq extends RpcDebugLifecycleRequest

// related to controlling the JVM under debug
sealed trait RpcDebugControlRequest extends RpcRequest
case class DebugSetBreakReq(file: File, line: Int) extends RpcDebugControlRequest
case class DebugClearBreakReq(file: File, line: Int) extends RpcDebugControlRequest
case object DebugClearAllBreaksReq extends RpcDebugControlRequest
case object DebugListBreakpointsReq extends RpcDebugControlRequest
case object DebugRunReq extends RpcDebugControlRequest
case class DebugContinueReq(threadId: DebugThreadId) extends RpcDebugControlRequest
case class DebugStepReq(threadId: DebugThreadId) extends RpcDebugControlRequest
case class DebugNextReq(threadId: DebugThreadId) extends RpcDebugControlRequest
case class DebugStepOutReq(threadId: DebugThreadId) extends RpcDebugControlRequest

// related to inspecting the debugging
sealed trait RpcDebugInspectRequest extends RpcRequest
case class DebugLocateNameReq(threadId: DebugThreadId, name: String) extends RpcDebugInspectRequest
case class DebugValueReq(loc: DebugLocation) extends RpcDebugInspectRequest
case class DebugToStringReq(threadId: DebugThreadId, loc: DebugLocation) extends RpcDebugInspectRequest
case class DebugSetValueReq(loc: DebugLocation, newValue: String) extends RpcDebugInspectRequest
case class DebugBacktraceReq(threadId: DebugThreadId, index: Int, count: Int) extends RpcDebugInspectRequest

// marked for removal. Either redundant, legacy or requires a rethink
sealed trait RpcDeprecatedRequest extends RpcRequest
case object PeekUndoReq extends RpcDeprecatedRequest
case class ExecUndoReq(id: Int) extends RpcDeprecatedRequest
case object ReplConfigReq extends RpcDeprecatedRequest
case class PatchSourceReq(file: File, edits: List[PatchOp]) extends RpcDeprecatedRequest
case class FormatOneSourceReq(file: SourceFileInfo) extends RpcDeprecatedRequest
case class TypecheckFilesReq(files: List[File]) extends RpcDeprecatedRequest
case object ShutdownServerReq extends RpcDeprecatedRequest

// TODO: the following are not defined at the endpoint and should be
//       removed via refactoring away their use.
case class DocUriReq(sig: DocSigPair)
case class TypeCompletionsReq(prefix: String, maxResults: Int)
case class DoExecUndo(undo: Undo)
case class SubscribeAsync(handler: EnsimeEvent => Unit)
