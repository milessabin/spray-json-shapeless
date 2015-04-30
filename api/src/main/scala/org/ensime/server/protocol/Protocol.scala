package org.ensime.server.protocol

import java.io._
import org.ensime.core._
import org.ensime.model._
import org.ensime.util.RefactorType

object ProtocolConst {

  val MsgCompilerUnexpectedError = 101
  val MsgMisc = 105

  val ErrExceptionInDebugger = 200
  val ErrExceptionInRPC = 201
  val ErrMalformedRPC = 202
  val ErrUnrecognizedForm = 203

  val ErrPeekUndoFailed = 206
  val ErrExecUndoFailed = 207

  val ErrFormatFailed = 208

  val ErrAnalyzerNotReady = 209
  val ErrExceptionInAnalyzer = 210

  val ErrFileDoesNotExist = 211

  val ErrExceptionInIndexer = 212
}

/**
 * TODO: WireFormat should be replaced with the domain objects, and
 * the list of methods reduced, to allow this to be replaced with
 * alternative wire format representations (e.g. S-Exp / JSON / XML).
 */
trait Protocol[WireFormat] {
  /**
   * Read a message from the socket.
   *
   * @param  reader  The stream from which to read the message.
   * @return         The message, in the intermediate format.
   */
  def readMessage(reader: InputStreamReader): WireFormat

  /**
   * Write a message to the socket.
   *
   * @param  value  The message to write.
   * @param  writer The stream to which to write the message.
   */
  def writeMessage(value: WireFormat, writer: OutputStream): Unit

  /**
   * Send a message in wire format to the client. Message
   * will be sent to the outputPeer, and then written to the
   * output socket.
   *
   * @param  o  The message to send.
   */
  def sendMessage(o: WireFormat): Unit

  /**
   * Handle a message from the client. Generally
   * messages encode RPC calls, and will be delegated
   * to the rpcTarget.
   *
   * @param  msg  The message we've received.
   */
  def handleIncomingMessage(msg: WireFormat): Unit

  /**
   * Notify the client that a message was received
   * that does not conform to the protocol.
   *
   * @param  code  Integer code denoting error type.
   * @param  detail  A message describing the problem.
   */
  def sendProtocolError(code: Int, detail: String): Unit
}

// TODO: resp should be a sealed family
//case class RpcResponseEnvelope(resp: Sexp, callId: Int)
//case class RpcError(code: Int, detail: String, callId: Int)
//case class ProtocolError(code: Int, detail: String)

case class RpcRequestEnvelope(req: RpcRequest, callId: Int)

sealed trait RpcRequest
case object ConnectionInfoReq extends RpcRequest
case object InitProjectReq extends RpcRequest
case object PeekUndoReq extends RpcRequest
case class ExecUndoReq(id: Int) extends RpcRequest
case object ReplConfigReq extends RpcRequest
case class RemoveFileReq(file: File) extends RpcRequest
case class TypecheckFileReq(fileInfo: SourceFileInfo) extends RpcRequest
case class TypecheckFilesReq(files: List[File]) extends RpcRequest
case class PatchSourceReq(file: File, edits: List[PatchOp]) extends RpcRequest
case object UnloadAllReq extends RpcRequest
case object TypecheckAllReq extends RpcRequest
case class FormatSourceReq(files: List[File]) extends RpcRequest
case class FormatOneSourceReq(file: SourceFileInfo) extends RpcRequest
case class PublicSymbolSearchReq(keywords: List[String], maxResults: Int) extends RpcRequest
case class ImportSuggestionsReq(file: File, point: Int, names: List[String], maxResults: Int) extends RpcRequest
case class DocUriAtPointReq(file: File, point: OffsetRange) extends RpcRequest
case class DocUriForSymbolReq(
  typeFullName: String,
  memberName: Option[String],
  signatureString: Option[String]
) extends RpcRequest
case class CompletionsReq(
  fileInfo: SourceFileInfo,
  point: Int,
  maxResults: Int,
  caseSens: Boolean,
  reload: Boolean
) extends RpcRequest
case class PackageMemberCompletionReq(path: String, prefix: String) extends RpcRequest
case class CallCompletionReq(id: Int) extends RpcRequest
case class UsesOfSymbolAtPointReq(file: File, point: Int) extends RpcRequest
case class TypeByIdReq(id: Int) extends RpcRequest
case class TypeByNameReq(name: String) extends RpcRequest
case class TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) extends RpcRequest
case class TypeAtPointReq(file: File, range: OffsetRange) extends RpcRequest
case class InspectTypeAtPointReq(file: File, range: OffsetRange) extends RpcRequest
case class InspectTypeByIdReq(id: Int) extends RpcRequest
case class InspectTypeByNameReq(name: String) extends RpcRequest
case class SymbolAtPointReq(file: File, point: Int) extends RpcRequest
case class SymbolByNameReq(
  typeFullName: String,
  memberName: Option[String],
  signatureString: Option[String]
) extends RpcRequest
case class InspectPackageByPathReq(path: String) extends RpcRequest
case class PrepareRefactorReq(
  procId: Int,
  tpe: Symbol, // tpe is ignored but part of the legacy wire format
  params: RefactorDesc,
  interactive: Boolean
) extends RpcRequest
case class ExecRefactorReq(procId: Int, tpe: RefactorType) extends RpcRequest
case class CancelRefactorReq(procId: Int) extends RpcRequest
case class SymbolDesignationsReq(
  file: File,
  start: Int,
  end: Int,
  requestedTypes: List[SourceSymbol]
) extends RpcRequest
case class ExpandSelectionReq(file: File, start: Int, end: Int) extends RpcRequest
case object DebugActiveVmReq extends RpcRequest
case class DebugStartReq(commandLine: String) extends RpcRequest
case class DebugAttachReq(hostname: String, port: String) extends RpcRequest
case object DebugStopReq extends RpcRequest
case class DebugSetBreakReq(file: File, line: Int) extends RpcRequest
case class DebugClearBreakReq(file: File, line: Int) extends RpcRequest
case object DebugClearAllBreaksReq extends RpcRequest
case object DebugListBreakpointsReq extends RpcRequest
case object DebugRunReq extends RpcRequest
case class DebugContinueReq(threadId: DebugThreadId) extends RpcRequest
case class DebugStepReq(threadId: DebugThreadId) extends RpcRequest
case class DebugNextReq(threadId: DebugThreadId) extends RpcRequest
case class DebugStepOutReq(threadId: DebugThreadId) extends RpcRequest
case class DebugLocateNameReq(threadId: DebugThreadId, name: String) extends RpcRequest
case class DebugValueReq(loc: DebugLocation) extends RpcRequest
case class DebugToStringReq(threadId: DebugThreadId, loc: DebugLocation) extends RpcRequest
case class DebugSetValueReq(loc: DebugLocation, newValue: String) extends RpcRequest
case class DebugBacktraceReq(threadId: DebugThreadId, index: Int, count: Int) extends RpcRequest
case object ShutdownServerReq extends RpcRequest

// TODO: the following are not defined at the endpoint and should be
//       removed via refactoring away their use.
case class DocUriReq(sig: DocSigPair) extends RpcRequest
case class TypeCompletionsReq(prefix: String, maxResults: Int) extends RpcRequest
case class DoExecUndo(undo: Undo) extends RpcRequest
case class SubscribeAsync(handler: EnsimeEvent => Unit) extends RpcRequest
