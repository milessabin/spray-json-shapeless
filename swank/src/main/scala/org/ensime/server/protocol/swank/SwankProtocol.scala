package org.ensime.server.protocol.swank

import scala.util._

import org.ensime.sexp.Sexp
import akka.actor.ActorRef

import org.ensime.EnsimeApi
import org.ensime.sexp._

import org.ensime.core._
import org.ensime.model._
import org.ensime.server.protocol._
import org.ensime.server.protocol.ProtocolConst._
import org.slf4j.LoggerFactory

import scala.util.control.NonFatal

/**
 * Wraps the EnsimeApi with marshallable inputs.
 *
 * TODO: this whole peer thing needs to be re-thought. Indeed, we
 * should really decide if we want an async API (RPC messages in/out)
 * or a synchronous one (EnsimeApi) --- not both.
 *
 * TODO: move into API and replace a refactored `Protocol`, leaving
 * the marshalling to be implemented.
 *
 * TODO: unified error/exception handling would simplify the logical parts below.
 */
class SwankProtocol(
  val peer: ActorRef,
  val syncApi: EnsimeApi
) extends Protocol[Sexp]
    with SwankWireFormatCodec {
  protected val log = LoggerFactory.getLogger(classOf[SwankProtocol])

  import SwankProtocolConversions._
  import SwankProtocolCommon._
  import SwankProtocolRequest._
  import SwankProtocolResponse._

  private implicit def toWF[T: SexpWriter](value: T): Sexp = value.toSexp

  def sendMessage(o: Sexp): Unit = {
    peer ! o
  }

  // TODO: move to marshalling layer
  def handleIncomingMessage(sexp: Sexp): Unit =
    Try(sexp.convertTo[RpcRequestEnvelope]) match {
      case Success(message) =>
        try handleRpcReq(message.req, message.callId)
        catch {
          case rpce: RPCError =>
            log.warn(s"handling $sexp gave ${rpce.detail}")
            sendRPCError(rpce.code, rpce.detail, message.callId)
          case NonFatal(e) =>
            log.error(s"handling ${sexp.compactPrint}", e)
            sendRPCError(ErrExceptionInRPC, e.getMessage, message.callId)
        }
      case Failure(e) =>
        log.error(s"unrecognised input ${sexp.compactPrint}", e)
        sendProtocolError(ErrUnrecognizedForm, sexp.compactPrint)
    }

  /**
   * Send an RPC Return with the given value.
   *
   * @param  value  The value to return.
   * @param  callId The id of the RPC call.
   */
  def sendRPCReturn(value: Sexp, callId: Int): Unit = {
    // TODO: use RpcResponseEnvelope
    val wrapped = SexpList(
      SexpSymbol(":return"),
      SexpList(SexpSymbol(":ok"), value),
      SexpNumber(callId)
    )
    sendMessage(wrapped)
  }

  /**
   * Notify the client that the RPC call could not
   * be handled.
   *
   * @param  code  Integer code denoting error type.
   * @param  detail  A message describing the error.
   * @param  callId The id of the failed RPC call.
   */
  def sendRPCError(code: Int, detail: String, callId: Int): Unit = {
    val wrapped = SexpList(
      SexpSymbol(":return"),
      SexpList(SexpSymbol(":abort"), SexpNumber(code), SexpString(detail)),
      SexpNumber(callId)
    )
    // TODO: use RpcError
    sendMessage(wrapped)
  }

  def sendProtocolError(code: Int, detail: String): Unit = {
    val wrapped = SexpList(
      SexpSymbol(":reader-error"),
      SexpNumber(code),
      SexpString(detail)
    )
    // TODO: use ProtocolError
    sendMessage(wrapped)
  }

  def handleRpcReq(message: RpcRequest, callId: Int): Unit = message match {
    case ConnectionInfoReq =>
      val info = syncApi.connectionInfo()
      sendRPCReturn(info, callId)

    case InitProjectReq =>
      sendRPCReturn(true, callId)
      syncApi.subscribeAsync((e) => { sendMessage(e) })

    case PeekUndoReq =>
      syncApi.peekUndo() match {
        case Some(result) => sendRPCReturn(result, callId)
        case None => sendRPCError(ErrPeekUndoFailed, "No such undo.", callId)
      }

    case ExecUndoReq(id) =>
      syncApi.execUndo(id) match {
        case Right(result) => sendRPCReturn(result, callId)
        case Left(msg) => sendRPCError(ErrExecUndoFailed, msg, callId)
      }

    case ReplConfigReq =>
      val config = syncApi.replConfig()
      sendRPCReturn(config, callId)

    case RemoveFileReq(file) =>
      syncApi.removeFile(file)
      sendRPCReturn(true, callId)

    case TypecheckFileReq(fileInfo) =>
      syncApi.typecheckFile(fileInfo)
      sendRPCReturn(value = true, callId)

    case TypecheckFilesReq(files) =>
      syncApi.typecheckFiles(files)
      sendRPCReturn(true, callId)

    case PatchSourceReq(file, edits) =>
      syncApi.patchSource(file, edits)
      sendRPCReturn(true, callId)

    case UnloadAllReq =>
      syncApi.unloadAll()
      sendRPCReturn(true, callId)

    case TypecheckAllReq =>
      syncApi.typecheckAll()
      sendRPCReturn(true, callId)

    case FormatSourceReq(files) =>
      syncApi.formatFiles(files)
      sendRPCReturn(true, callId)

    case FormatOneSourceReq(fileInfo) =>
      val result = syncApi.formatFile(fileInfo)
      sendRPCReturn(result, callId)

    case PublicSymbolSearchReq(keywords, maxResults) =>
      val result = syncApi.publicSymbolSearch(keywords, maxResults)
      sendRPCReturn(result, callId)

    case ImportSuggestionsReq(file, point, names, maxResults) =>
      val result = syncApi.importSuggestions(file, point, names, maxResults)
      sendRPCReturn(result, callId)

    case DocUriAtPointReq(file, point) =>
      syncApi.docUriAtPoint(file, point) match {
        case Some(value) => sendRPCReturn(value, callId)
        case None => sendRPCReturn(value = false, callId)
      }

    case DocUriForSymbolReq(typeFullName, memberName, signatureString) =>
      syncApi.docUriForSymbol(typeFullName, memberName, signatureString) match {
        case Some(value) => sendRPCReturn(value, callId)
        case None => sendRPCReturn(value = false, callId)
      }

    case CompletionsReq(fileInfo, point, maxResults, caseSens, reload) =>
      val result = syncApi.completionsAtPoint(fileInfo, point, maxResults, caseSens, reload)
      sendRPCReturn(result, callId)

    case PackageMemberCompletionReq(path, prefix) =>
      val members = syncApi.packageMemberCompletion(path, prefix)
      sendRPCReturn(members, callId)

    case CallCompletionReq(id) =>
      val result = syncApi.callCompletion(id)
      sendRPCReturn(result, callId)

    case UsesOfSymbolAtPointReq(file, point) =>
      val result = syncApi.usesOfSymAtPoint(file, point)
      sendRPCReturn(result, callId)

    case TypeByIdReq(id) =>
      val result = syncApi.typeById(id)
      sendRPCReturn(result, callId)

    case TypeByNameReq(name) =>
      val result = syncApi.typeByName(name)
      sendRPCReturn(result, callId)

    case TypeByNameAtPointReq(name, file, range) =>
      val result = syncApi.typeByNameAtPoint(name, file, range)
      sendRPCReturn(result, callId)

    case TypeAtPointReq(file, range) =>
      val result = syncApi.typeAtPoint(file, range)
      sendRPCReturn(result, callId)

    case InspectTypeAtPointReq(file, range) =>
      val result = syncApi.inspectTypeAtPoint(file, range)
      sendRPCReturn(result, callId)

    case InspectTypeByIdReq(id) =>
      val result = syncApi.inspectTypeById(id)
      sendRPCReturn(result, callId)

    case InspectTypeByNameReq(name) =>
      val result = syncApi.inspectTypeByName(name)
      sendRPCReturn(result, callId)

    case SymbolAtPointReq(file, point) =>
      val result = syncApi.symbolAtPoint(file, point)
      sendRPCReturn(result, callId)

    case SymbolByNameReq(typeFullName, memberName, signatureString) =>
      syncApi.symbolByName(typeFullName, memberName, signatureString) match {
        case Some(value) => sendRPCReturn(value, callId)
        case None => sendRPCReturn(false, callId)
      }

    case InspectPackageByPathReq(path) =>
      val result: Option[EntityInfo] = syncApi.inspectPackageByPath(path)
      sendRPCReturn(result, callId)

    case req @ PrepareRefactorReq(procId, _, refactor, interactive) =>
      syncApi.prepareRefactor(procId, refactor) match {
        case Right(r: RefactorEffect) =>
          if (interactive) {
            sendRPCReturn(r, callId)
          } else {
            // could be simplified by forwarding the message on to the relevant handler
            val execResult = syncApi.execRefactor(procId, refactor.refactorType)
            execResult match {
              case Right(result: RefactorResult) => sendRPCReturn(result, callId)
              case Left(failure) => sendRPCReturn(failure, callId)
            }
          }
        case Left(failure) =>
          sendRPCReturn(failure, callId)
      }

    case ExecRefactorReq(procId, tpe) =>
      syncApi.execRefactor(procId, tpe) match {
        case Right(result: RefactorResult) => sendRPCReturn(result, callId)
        case Left(f: RefactorFailure) => sendRPCReturn(f, callId)
      }

    case CancelRefactorReq(procId) =>
      syncApi.cancelRefactor(procId)
      sendRPCReturn(true, callId)

    case SymbolDesignationsReq(filename, start, end, requestedTypes) =>
      val res = syncApi.symbolDesignations(filename, start, end, requestedTypes)
      sendRPCReturn(res, callId)

    case ExpandSelectionReq(filename, start, end) =>
      val result = syncApi.expandSelection(filename, start, end)
      sendRPCReturn(result, callId)

    case DebugActiveVmReq =>
      val result = syncApi.debugActiveVM()
      sendRPCReturn(result, callId)

    case DebugStartReq(commandLine) =>
      val result = syncApi.debugStartVM(commandLine)
      sendRPCReturn(result, callId)

    case DebugAttachReq(hostname, port) =>
      val result = syncApi.debugAttachVM(hostname, port)
      sendRPCReturn(result, callId)

    case DebugStopReq =>
      val result = syncApi.debugStopVM()
      sendRPCReturn(result, callId)

    case DebugSetBreakReq(filename, line) =>
      syncApi.debugSetBreakpoint(filename, line)
      sendRPCReturn(true, callId)

    case DebugClearBreakReq(filename, line) =>
      syncApi.debugClearBreakpoint(filename, line)
      sendRPCReturn(true, callId)

    case DebugClearAllBreaksReq =>
      syncApi.debugClearAllBreakpoints()
      sendRPCReturn(true, callId)

    case DebugListBreakpointsReq =>
      val result = syncApi.debugListBreakpoints()
      sendRPCReturn(result, callId)

    case DebugRunReq =>
      val result = syncApi.debugRun()
      sendRPCReturn(result, callId)

    case DebugContinueReq(threadId) =>
      val result = syncApi.debugContinue(threadId)
      sendRPCReturn(result, callId)

    case DebugStepReq(threadId) =>
      val result = syncApi.debugStep(threadId)
      sendRPCReturn(result, callId)

    case DebugNextReq(threadId) =>
      val result = syncApi.debugNext(threadId)
      sendRPCReturn(result, callId)

    case DebugStepOutReq(threadId) =>
      val result = syncApi.debugStepOut(threadId)
      sendRPCReturn(result, callId)

    case DebugLocateNameReq(threadId, name) =>
      syncApi.debugLocateName(threadId, name) match {
        case Some(loc) =>
          sendRPCReturn(loc, callId)
        case None =>
          sendRPCReturn(value = false, callId)
      }

    case DebugValueReq(loc) =>
      syncApi.debugValue(loc) match {
        case Some(value) => sendRPCReturn(value, callId)
        case None => sendRPCReturn(value = false, callId)
      }

    case DebugToStringReq(threadId, loc) =>
      syncApi.debugToString(threadId, loc) match {
        case Some(value) => sendRPCReturn(value, callId)
        case None => sendRPCReturn(value = false, callId)
      }

    case DebugSetValueReq(loc, newValue) =>
      val result = syncApi.debugSetValue(loc, newValue)
      sendRPCReturn(result, callId)

    case DebugBacktraceReq(threadId, index, count) =>
      val result = syncApi.debugBacktrace(threadId, index, count)
      sendRPCReturn(result, callId)

    case ShutdownServerReq =>
      syncApi.shutdownServer()
      sendRPCReturn(true, callId)

  }

}
