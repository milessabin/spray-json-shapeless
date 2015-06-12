package org.ensime.jerk

import java.io.File
import scala.util._

import spray.json._
import org.ensime.json._
import shapeless._

import org.ensime.api._

import pimpathon.file._

object JerkFormats extends DefaultJsonProtocol with FamilyFormats {
  // wtf?? why is this needed, why does it even work? Miles??
  implicit val symbolFormat = SymbolJsonFormat

  // FIXME: (reminder) canonise File with a shapeless Poly1 in endpoints
  implicit object FileFormat extends JsonFormat[File] {
    def read(j: JsValue): File = j match {
      case JsString(path) => file(path)
      case other => unexpectedJson[File](other)
    }
    def write(f: File): JsValue = JsString(f.getPath)
  }

  implicit object DebugThreadIdFormat extends JsonFormat[DebugThreadId] {
    def read(j: JsValue): DebugThreadId = j match {
      case JsNumber(id) => new DebugThreadId(id.longValue)
      case other => unexpectedJson[DebugThreadId](other)
    }
    def write(dtid: DebugThreadId): JsValue = JsNumber(dtid.id)
  }

  // some of the case classes use the keyword `type`, so we need a better default
  override implicit def coproductHint[T: Typeable]: CoproductHint[T] = new FlatCoproductHint[T]("typehint")

  val RpcRequestFormat = JsonFormat[RpcRequest]

  // FIXME: introduce a family tree that means we don't have to have
  // so many specific converters. e.g. a single sealed family in and
  // and single sealed family out
  val EnsimeEventFormat = RootJsonFormat[EnsimeEvent]

  // everything below this line is crap, slowing down the compile
  val SourcePositionFormat = RootJsonFormat[SourcePosition]
  val DebugLocationFormat = RootJsonFormat[DebugLocation]
  val DebugValueFormat = RootJsonFormat[DebugValue]
  val DebugClassFieldFormat = RootJsonFormat[DebugClassField]
  val DebugStackLocalFormat = RootJsonFormat[DebugStackLocal]
  val DebugStackFrameFormat = RootJsonFormat[DebugStackFrame]
  val DebugBacktraceFormat = RootJsonFormat[DebugBacktrace]
  val BreakpointFormat = RootJsonFormat[Breakpoint]
  val BreakpointListFormat = RootJsonFormat[BreakpointList]
  val DebugVmStatusFormat = RootJsonFormat[DebugVmStatus]
  val NoteFormat = RootJsonFormat[Note]
  val CompletionInfoFormat = RootJsonFormat[CompletionInfo]
  val CompletionInfoListFormat = RootJsonFormat[CompletionInfoList]
  val SymbolInfoFormat = RootJsonFormat[SymbolInfo]
  val EntityInfoFormat = RootJsonFormat[EntityInfo]
  val CallCompletionInfoFormat = RootJsonFormat[CallCompletionInfo]
  val InterfaceInfoFormat = RootJsonFormat[InterfaceInfo]
  val TypeInspectInfoFormat = RootJsonFormat[TypeInspectInfo]
  val SymbolSearchResultFormat = RootJsonFormat[SymbolSearchResult]
  val SymbolSearchResultsFormat = RootJsonFormat[SymbolSearchResults]
  val ImportSuggestionsFormat = RootJsonFormat[ImportSuggestions]
  val ERangePositionFormat = RootJsonFormat[ERangePosition]
  val FileRangeFormat = RootJsonFormat[FileRange]
  val SymbolDesignationsFormat = RootJsonFormat[SymbolDesignations]
  val RefactorFailureFormat = RootJsonFormat[RefactorFailure]
  val RefactorEffectFormat = RootJsonFormat[RefactorEffect]
  val RefactorResultFormat = RootJsonFormat[RefactorResult]
  val UndoFormat = RootJsonFormat[Undo]
  val UndoResultFormat = RootJsonFormat[UndoResult]

}

// Isolated just to help me see exactly what is needed
object JerkEndpoints {
  implicit val RpcRequestFormat = JerkFormats.RpcRequestFormat

  implicit val EnsimeEventFormat = JerkFormats.EnsimeEventFormat
  // *sigh*
  implicit val SourcePositionFormat = JerkFormats.SourcePositionFormat
  implicit val DebugLocationFormat = JerkFormats.DebugLocationFormat
  implicit val DebugValueFormat = JerkFormats.DebugValueFormat
  implicit val DebugClassFieldFormat = JerkFormats.DebugClassFieldFormat
  implicit val DebugStackLocalFormat = JerkFormats.DebugStackLocalFormat
  implicit val DebugStackFrameFormat = JerkFormats.DebugStackFrameFormat
  implicit val DebugBacktraceFormat = JerkFormats.DebugBacktraceFormat
  implicit val BreakpointFormat = JerkFormats.BreakpointFormat
  implicit val BreakpointListFormat = JerkFormats.BreakpointListFormat
  implicit val DebugVmStatusFormat = JerkFormats.DebugVmStatusFormat
  implicit val NoteFormat = JerkFormats.NoteFormat
  implicit val CompletionInfoFormat = JerkFormats.CompletionInfoFormat
  implicit val CompletionInfoListFormat = JerkFormats.CompletionInfoListFormat
  implicit val SymbolInfoFormat = JerkFormats.SymbolInfoFormat
  implicit val EntityInfoFormat = JerkFormats.EntityInfoFormat
  implicit val CallCompletionInfoFormat = JerkFormats.CallCompletionInfoFormat
  implicit val InterfaceInfoFormat = JerkFormats.InterfaceInfoFormat
  implicit val TypeInspectInfoFormat = JerkFormats.TypeInspectInfoFormat
  implicit val SymbolSearchResultFormat = JerkFormats.SymbolSearchResultFormat
  implicit val SymbolSearchResultsFormat = JerkFormats.SymbolSearchResultsFormat
  implicit val ImportSuggestionsFormat = JerkFormats.ImportSuggestionsFormat
  implicit val ERangePositionFormat = JerkFormats.ERangePositionFormat
  implicit val FileRangeFormat = JerkFormats.FileRangeFormat
  implicit val SymbolDesignationsFormat = JerkFormats.SymbolDesignationsFormat
  implicit val RefactorFailureFormat = JerkFormats.RefactorFailureFormat
  implicit val RefactorEffectFormat = JerkFormats.RefactorEffectFormat
  implicit val RefactorResultFormat = JerkFormats.RefactorResultFormat
  implicit val UndoFormat = JerkFormats.UndoFormat
  implicit val UndoResultFormat = JerkFormats.UndoResultFormat
}
