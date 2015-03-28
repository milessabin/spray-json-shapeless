package org.ensime.core

import java.io.File

import org.ensime.config._
import org.ensime.model._
import org.ensime.util._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise }
import scala.util.Try

case class RPCError(code: Int, detail: String) extends RuntimeException("" + code + ": " + detail)
case class AsyncEvent(evt: EnsimeEvent)

case object AnalyzerShutdownEvent
case object ReloadExistingFilesEvent
case object AskReTypecheck

case object VoidResponse

trait RPCRequest

case class ReloadFilesReq(files: List[SourceFileInfo], async: Boolean) extends RPCRequest
case object ReloadAllReq extends RPCRequest
case object UnloadAllReq extends RPCRequest
case class PatchSourceReq(file: File, edits: List[PatchOp]) extends RPCRequest
case class RemoveFileReq(file: File) extends RPCRequest
case class CompletionsReq(fileInfo: SourceFileInfo, point: Int, maxResults: Int, caseSens: Boolean) extends RPCRequest
case class TypeCompletionsReq(prefix: String, maxResults: Int) extends RPCRequest
case class ImportSuggestionsReq(file: File, point: Int, names: List[String], maxResults: Int) extends RPCRequest
case class PublicSymbolSearchReq(names: List[String], maxResults: Int) extends RPCRequest
case class UsesOfSymAtPointReq(file: File, point: Int) extends RPCRequest
case class PackageMemberCompletionReq(path: String, prefix: String) extends RPCRequest
case class SymbolAtPointReq(file: File, point: Int) extends RPCRequest
case class InspectTypeReq(file: File, range: OffsetRange) extends RPCRequest
case class InspectTypeByIdReq(id: Int) extends RPCRequest
case class InspectTypeByNameReq(name: String) extends RPCRequest
case class InspectPackageByPathReq(path: String) extends RPCRequest
case class TypeByIdReq(id: Int) extends RPCRequest
case class TypeByNameReq(name: String) extends RPCRequest
case class TypeByNameAtPointReq(name: String, file: File, range: OffsetRange) extends RPCRequest
case class CallCompletionReq(id: Int) extends RPCRequest
case class TypeAtPointReq(file: File, range: OffsetRange) extends RPCRequest
case class SymbolDesignationsReq(file: File, start: Int, end: Int, tpes: Set[SourceSymbol]) extends RPCRequest
case class ExecUndoReq(undo: Undo) extends RPCRequest
case class FormatFilesReq(filenames: List[String]) extends RPCRequest
case class FormatFileReq(fileInfo: SourceFileInfo) extends RPCRequest
case class ExpandSelectionReq(filename: String, start: Int, stop: Int) extends RPCRequest
case class DocUriReq(sig: DocSigPair) extends RPCRequest
case class DocSignatureAtPointReq(file: File, range: OffsetRange) extends RPCRequest
case class DocSignatureForSymbolReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) extends RPCRequest
case class SymbolByNameReq(typeFullName: String, memberName: Option[String], signatureString: Option[String]) extends RPCRequest

case class SubscribeAsync(handler: EnsimeEvent => Unit) extends RPCRequest
