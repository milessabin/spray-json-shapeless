package org.ensime.server.protocol.swank

import org.scalatest._

import org.ensime.sexp._
import org.ensime.api._
import org.ensime.util._

import UnitTestUtils._

class SwankFormatsSpec extends FlatSpec with Matchers with EnsimeTestData {
  import SwankProtocolRequest.RpcRequestFormat
  import SwankProtocolResponse.unhappyFamily
  import SwankProtocolResponse.EnsimeEventFormat

  // API backcompat for outgoing messages
  def toWF(value: Any): Sexp = unhappyFamily(value)
  def toWF(value: EnsimeEvent): Sexp = value.toSexp

  implicit class WireStringSexp(sexp: Sexp) {
    def toWireString = sexp.compactPrint
  }

  // TODO: use the tests below for incoming/outgoing to align with the
  //       JERK tests.
  //
  // TODO: for the incoming messages, we only care about unmarshalling
  //       so new test cases need to be written, probably by
  //       implementing roundtripping.

  // def marshalls(value: Any, via: Option[String] = None): Unit = {
  //   val sexp = value match {
  //     case r: RpcRequest => r.toSexp
  //     case e: EnsimeEvent => e.toSexp
  //     case other => unhappyFamily(other)
  //   }
  //   val string = sexp.compactPrint

  //   via match {
  //     case None =>
  //       println(s"$value = $string")
  //     //fail("no test")
  //     case Some(expected) => string shouldBe expected
  //   }

  // }

  // "SWANK Formats" should "marshalls inbound messages" in {
  //   marshalls(
  //     ConnectionInfoReq: RpcRequest
  //   )

  //   marshalls(
  //     InitProjectReq: RpcRequest
  //   )

  //   marshalls(
  //     RemoveFileReq(file1): RpcRequest
  //   )

  //   marshalls(
  //     TypecheckFileReq(sourceFileInfo): RpcRequest
  //   )

  //   marshalls(
  //     TypecheckFilesReq(List(file1, file2)): RpcRequest
  //   )

  //   marshalls(
  //     PatchSourceReq(file1, List(
  //       PatchInsert(1, "foo"),
  //       PatchDelete(1, 10),
  //       PatchReplace(10, 20, "bar")
  //     )): RpcRequest
  //   )

  //   marshalls(
  //     UnloadAllReq: RpcRequest
  //   )

  //   marshalls(
  //     TypecheckAllReq: RpcRequest
  //   )

  //   marshalls(
  //     FormatSourceReq(List(file1, file2)): RpcRequest
  //   )

  //   marshalls(
  //     FormatOneSourceReq(sourceFileInfo): RpcRequest
  //   )

  //   marshalls(
  //     PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest
  //   )

  //   marshalls(
  //     ImportSuggestionsReq(file1, 1, List("foo", "bar"), 10): RpcRequest
  //   )

  //   marshalls(
  //     DocUriAtPointReq(file1, OffsetRange(1, 10)): RpcRequest
  //   )

  //   marshalls(
  //     DocUriForSymbolReq("foo.bar", Some("Baz"), None): RpcRequest
  //   )

  //   marshalls(
  //     CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest
  //   )

  //   marshalls(
  //     PackageMemberCompletionReq("foo", "bar"): RpcRequest
  //   )

  //   marshalls(
  //     CallCompletionReq(13): RpcRequest
  //   )

  //   marshalls(
  //     UsesOfSymbolAtPointReq(file1, 100): RpcRequest
  //   )

  //   marshalls(
  //     TypeByIdReq(13): RpcRequest
  //   )

  //   marshalls(
  //     TypeByNameReq("foo.bar"): RpcRequest
  //   )

  //   marshalls(
  //     TypeByNameAtPointReq("foo.bar", file1, OffsetRange(1, 10)): RpcRequest
  //   )

  //   marshalls(
  //     TypeAtPointReq(file1, OffsetRange(1, 100)): RpcRequest
  //   )

  //   marshalls(
  //     InspectTypeAtPointReq(file1, OffsetRange(1, 100)): RpcRequest
  //   )

  //   marshalls(
  //     InspectTypeByIdReq(13): RpcRequest
  //   )

  //   marshalls(
  //     InspectTypeByNameReq("foo.Bar"): RpcRequest
  //   )

  //   marshalls(
  //     SymbolAtPointReq(file1, 101): RpcRequest
  //   )

  //   marshalls(
  //     SymbolByNameReq("foo.Bar", Some("baz"), None): RpcRequest
  //   )

  //   marshalls(
  //     InspectPackageByPathReq("foo.bar"): RpcRequest
  //   )

  //   marshalls(
  //     PrepareRefactorReq(1, 'ignored, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest
  //   )

  //   marshalls(
  //     ExecRefactorReq(1, RefactorType.Rename): RpcRequest
  //   )

  //   marshalls(
  //     CancelRefactorReq(1): RpcRequest
  //   )

  //   marshalls(
  //     SymbolDesignationsReq(
  //       file1, 1, 100,
  //       List(ObjectSymbol, ValSymbol)
  //     ): RpcRequest
  //   )

  //   marshalls(
  //     ExpandSelectionReq(file1, 100, 200): RpcRequest
  //   )

  //   marshalls(
  //     DebugActiveVmReq: RpcRequest
  //   )

  //   marshalls(
  //     DebugStartReq("blah blah blah"): RpcRequest
  //   )

  //   marshalls(
  //     DebugAttachReq("mylovelyhorse", "13"): RpcRequest
  //   )

  //   marshalls(
  //     DebugStopReq: RpcRequest
  //   )

  //   marshalls(
  //     DebugSetBreakReq(file1, 13): RpcRequest
  //   )

  //   marshalls(
  //     DebugClearBreakReq(file1, 13): RpcRequest
  //   )

  //   marshalls(
  //     DebugClearAllBreaksReq: RpcRequest
  //   )

  //   marshalls(
  //     DebugListBreakpointsReq: RpcRequest
  //   )

  //   marshalls(
  //     DebugRunReq: RpcRequest
  //   )

  //   marshalls(
  //     DebugContinueReq(dtid): RpcRequest
  //   )

  //   marshalls(
  //     DebugStepReq(dtid): RpcRequest
  //   )

  //   marshalls(
  //     DebugNextReq(dtid): RpcRequest
  //   )

  //   marshalls(
  //     DebugStepOutReq(dtid): RpcRequest
  //   )

  //   marshalls(
  //     DebugLocateNameReq(dtid, "foo"): RpcRequest
  //   )

  //   marshalls(
  //     DebugValueReq(debugLocationArray): RpcRequest
  //   )

  //   marshalls(
  //     DebugToStringReq(dtid, debugLocationArray): RpcRequest
  //   )

  //   marshalls(
  //     DebugSetValueReq(debugLocationArray, "bar"): RpcRequest
  //   )

  //   marshalls(
  //     DebugBacktraceReq(dtid, 100, 200): RpcRequest
  //   )

  //   marshalls(
  //     ShutdownServerReq: RpcRequest
  //   )

  // }

  // it should "marshalls EnsimeGeneralEvent as EnsimeEvent" in {
  //   marshalls(
  //     SendBackgroundMessageEvent("ABCDEF", 1): EnsimeEvent
  //   )

  //   marshalls(
  //     AnalyzerReadyEvent: EnsimeEvent
  //   )

  //   marshalls(
  //     FullTypeCheckCompleteEvent: EnsimeEvent
  //   )

  //   marshalls(
  //     IndexerReadyEvent: EnsimeEvent
  //   )

  //   marshalls(
  //     NewScalaNotesEvent(
  //       isFull = false,
  //       List(new Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
  //     ): EnsimeEvent
  //   )

  //   marshalls(
  //     ClearAllScalaNotesEvent: EnsimeEvent
  //   )
  // }

  // it should "marshalls DebugEvent as EnsimeEvent" in {
  //   marshalls(
  //     DebugOutputEvent("XXX"): EnsimeEvent
  //   )

  //   marshalls(
  //     DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent
  //   )

  //   marshalls(
  //     DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent
  //   )

  //   marshalls(
  //     DebugVMStartEvent: EnsimeEvent
  //   )
  //   marshalls(
  //     DebugVMDisconnectEvent: EnsimeEvent
  //   )
  //   marshalls(
  //     DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)): EnsimeEvent
  //   )
  //   marshalls(
  //     DebugExceptionEvent(33L, dtid, "threadNameStr", None, None): EnsimeEvent
  //   )

  //   marshalls(
  //     DebugThreadStartEvent(dtid): EnsimeEvent
  //   )
  //   marshalls(
  //     DebugThreadDeathEvent(dtid): EnsimeEvent
  //   )
  // }

  // it should "marshalls DebugLocation" in {
  //   marshalls(
  //     DebugObjectReference(57L): DebugLocation
  //   )

  //   marshalls(
  //     DebugArrayElement(DebugObjectId(58L), 2): DebugLocation
  //   )

  //   marshalls(
  //     DebugObjectField(DebugObjectId(58L), "fieldName"): DebugLocation
  //   )

  //   marshalls(
  //     DebugStackSlot(DebugThreadId(27), 12, 23): DebugLocation
  //   )
  // }

  // it should "marshalls DebugValue" in {
  //   marshalls(
  //     DebugPrimitiveValue("summaryStr", "typeNameStr"): DebugValue
  //   )

  //   marshalls(
  //     DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue
  //   )

  //   marshalls(
  //     DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue
  //   )

  //   marshalls(
  //     DebugNullValue("typeNameStr"): DebugValue
  //   )

  //   marshalls(
  //     DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)): DebugValue
  //   )

  //   marshalls(
  //     debugClassField: DebugClassField
  //   )

  //   marshalls(
  //     debugStackLocal1: DebugStackLocal
  //   )

  //   marshalls(
  //     debugStackFrame: DebugStackFrame
  //   )

  //   marshalls(
  //     DebugBacktrace(List(debugStackFrame), dtid, "thread1"): DebugBacktrace
  //   )

  //   marshalls(
  //     sourcePos1: SourcePosition
  //   )
  //   marshalls(
  //     sourcePos2: SourcePosition
  //   )
  //   marshalls(
  //     sourcePos3: SourcePosition
  //   )
  //   marshalls(
  //     sourcePos4: SourcePosition
  //   )

  //   marshalls(
  //     breakPoint1: Breakpoint
  //   )

  //   marshalls(
  //     BreakpointList(List(breakPoint1), List(breakPoint2)): BreakpointList
  //   )

  //   marshalls(
  //     DebugVmSuccess(): DebugVmStatus
  //   )

  //   marshalls(
  //     DebugVmError(303, "xxxx"): DebugVmStatus
  //   )
  // }

  // it should "marshalls various informational types" in {
  //   marshalls(
  //     note1: Note
  //   )

  //   marshalls(
  //     completionInfo: CompletionInfo
  //   )

  //   marshalls(
  //     completionInfo2: CompletionInfo
  //   )

  //   marshalls(
  //     CompletionInfoList("fooBar", List(completionInfo)): CompletionInfoList
  //   )

  //   marshalls(
  //     new SymbolInfo("name", "localName", None, typeInfo, false, Some(2)): SymbolInfo
  //   )

  //   marshalls(
  //     new NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EntityInfo
  //   )

  //   marshalls(
  //     entityInfo: EntityInfo
  //   )

  //   marshalls(
  //     typeInfo: EntityInfo
  //   )

  //   marshalls(
  //     packageInfo: EntityInfo
  //   )

  //   marshalls(
  //     new CallCompletionInfo(typeInfo, List(paramSectionInfo)): CallCompletionInfo
  //   )

  //   marshalls(
  //     interfaceInfo: InterfaceInfo
  //   )

  //   marshalls(
  //     new TypeInspectInfo(typeInfo, Some(1), List(interfaceInfo)): TypeInspectInfo
  //   )
  // }

  // it should "support search related responses" in {
  //   marshalls(
  //     new SymbolSearchResults(List(methodSearchRes, typeSearchRes)): SymbolSearchResults
  //   )

  //   marshalls(
  //     new ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): ImportSuggestions
  //   )

  //   marshalls(
  //     methodSearchRes: SymbolSearchResult
  //   )

  //   marshalls(
  //     typeSearchRes: SymbolSearchResult
  //   )
  // }

  // it should "support ranges and semantic highlighting" in {
  //   marshalls(
  //     new ERangePosition(batchSourceFile, 75, 70, 90): ERangePosition
  //   )

  //   marshalls(
  //     FileRange("/abc", 7, 9): FileRange
  //   )

  //   marshalls(
  //     SymbolDesignations(
  //       symFile, List(
  //       SymbolDesignation(7, 9, VarFieldSymbol),
  //       SymbolDesignation(11, 22, ClassSymbol)
  //     )
  //     ): SymbolDesignations
  //   )
  // }

  // it should "refactoring messages" in {
  //   marshalls(
  //     RefactorFailure(7, "message"): RefactorFailure
  //   )

  //   marshalls(
  //     refactorEffect: RefactorEffect
  //   )

  //   marshalls(
  //     refactorResult: RefactorResult
  //   )

  // }

  import SwankTestData._

  "SwankFormatsSpec" should "encode all message types correctly" in {
    assert(toWF(SendBackgroundMessageEvent("ABCDEF", 1)).toWireString === """(:background-message 1 "ABCDEF")""")
    assert(toWF(AnalyzerReadyEvent).toWireString === "(:compiler-ready)")
    assert(toWF(FullTypeCheckCompleteEvent).toWireString === "(:full-typecheck-finished)")
    assert(toWF(IndexerReadyEvent).toWireString === "(:indexer-ready)")

    assert(toWF(NewScalaNotesEvent(
      isFull = false,
      List(new Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
    )).toWireString === """(:scala-notes (:is-full nil :notes ((:file "foo.scala" :msg "testMsg" :severity warn :beg 50 :end 55 :line 77 :col 5))))""")

    assert(toWF(ClearAllScalaNotesEvent).toWireString === "(:clear-all-scala-notes)")

    // toWF(evt: DebugEvent): WireFormat
    assert(toWF(DebugOutputEvent("XXX")).toWireString === """(:debug-event (:type output :body "XXX"))""")
    assert(toWF(DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line)).toWireString ===
      s"""(:debug-event (:type step :thread-id "207" :thread-name "threadNameStr" :file $file1_str :line 57))""")
    assert(toWF(DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line)).toWireString ===
      s"""(:debug-event (:type breakpoint :thread-id "209" :thread-name "threadNameStr" :file $file1_str :line 57))""")
    assert(toWF(DebugVMStartEvent).toWireString === """(:debug-event (:type start))""")
    assert(toWF(DebugVMDisconnectEvent).toWireString === """(:debug-event (:type disconnect))""")

    assert(toWF(DebugExceptionEvent(33L, DebugThreadId(209), "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line))).toWireString ===
      """(:debug-event (:type exception :exception 33 :thread-id "209" :thread-name "threadNameStr" :file """ + file1_str + """ :line 57))""")

    assert(toWF(DebugExceptionEvent(33L, DebugThreadId(209), "threadNameStr", None, None)).toWireString ===
      """(:debug-event (:type exception :exception 33 :thread-id "209" :thread-name "threadNameStr" :file nil :line nil))""")

    assert(toWF(DebugThreadStartEvent(DebugThreadId(907))).toWireString === """(:debug-event (:type threadStart :thread-id "907"))""")
    assert(toWF(DebugThreadDeathEvent(DebugThreadId(907))).toWireString === """(:debug-event (:type threadDeath :thread-id "907"))""")

    // toWF(obj: DebugLocation)
    assert(toWF(DebugObjectReference(57L).asInstanceOf[DebugLocation]).toWireString ===
      """(:type reference :object-id "57")""")
    assert(toWF(DebugArrayElement(DebugObjectId(58L), 2).asInstanceOf[DebugLocation]).toWireString ===
      """(:type element :object-id "58" :index 2)""")
    assert(toWF(DebugObjectField(DebugObjectId(58L), "fieldName").asInstanceOf[DebugLocation]).toWireString ===
      """(:type field :object-id "58" :field "fieldName")""")
    assert(toWF(DebugStackSlot(DebugThreadId(27), 12, 23).asInstanceOf[DebugLocation]).toWireString ===
      """(:type slot :thread-id "27" :frame 12 :offset 23)""")

    // toWF(obj: DebugValue)

    // toWF(evt: DebugPrimitiveValue)
    assert(toWF(DebugPrimitiveValue("summaryStr", "typeNameStr").asInstanceOf[DebugValue]).toWireString ===
      """(:val-type prim :summary "summaryStr" :type-name "typeNameStr")""")

    // toWF(evt: DebugClassField)
    assert(toWF(debugClassField).toWireString === """(:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")""")

    // toWF(obj: DebugStringInstance)
    assert(toWF(DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)).asInstanceOf[DebugValue]).toWireString ===
      """(:val-type str :summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id "5")""")

    // toWF(evt: DebugObjectInstance)
    assert(toWF(DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)).asInstanceOf[DebugValue]).toWireString ===
      """(:val-type obj :summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id "5")""")

    // toWF(evt: DebugNullValue)
    assert(toWF(DebugNullValue("typeNameStr").asInstanceOf[DebugValue]).toWireString ===
      """(:val-type null :type-name "typeNameStr")""")

    // toWF(evt: DebugArrayInstance)
    assert(toWF(DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)).asInstanceOf[DebugValue]).toWireString ===
      """(:val-type arr :length 3 :type-name "typeName" :element-type-name "elementType" :object-id "5")""")

    // toWF(evt: DebugStackLocal)
    assert(toWF(debugStackLocal1).toWireString === """(:index 3 :name "name1" :summary "summary1" :type-name "type1")""")

    // toWF(evt: DebugStackFrame)
    assert(toWF(debugStackFrame).toWireString === s"""(:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file $file1_str :line 57) :this-object-id "7")""")

    // toWF(evt: DebugBacktrace)
    assert(toWF(DebugBacktrace(List(debugStackFrame), DebugThreadId(17), "thread1")).toWireString === s"""(:frames ((:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file $file1_str :line 57) :this-object-id "7")) :thread-id "17" :thread-name "thread1")""")

    // toWF(pos: LineSourcePosition)
    assert(toWF(sourcePos1).toWireString === s"""(:type line :file $file1_str :line 57)""")

    // toWF(pos: EmptySourcePosition)
    assert(toWF(sourcePos3: SourcePosition).toWireString === "(:type empty)")

    // toWF(pos: OffsetSourcePosition)
    assert(toWF(sourcePos4).toWireString === """(:type offset :file """ + file1_str + """ :offset 456)""")

    // toWF(bp: Breakpoint)
    assert(toWF(breakPoint1).toWireString === """(:file """ + file1_str + """ :line 57)""")

    // toWF(config: BreakpointList)
    assert(toWF(BreakpointList(List(breakPoint1), List(breakPoint2))).toWireString === """(:active ((:file """ + file1_str + """ :line 57)) :pending ((:file """ + file1_str + """ :line 59)))""")

    // toWF(config: ProjectConfig)
    //assert(toWF(ProjectConfig()).toWireString === """(:project-name nil :source-roots ())""")
    //assert(toWF(ProjectConfig(name = Some("Project1"), sourceRoots = List(file2))).toWireString === """(:project-name "Project1" :source-roots (""" + file2_str + """))""")

    // toWF(value: Boolean)
    assert(toWF(value = true).toWireString === """t""")
    assert(toWF(value = false).toWireString === """nil""")

    // toWF(value: Note)
    assert(toWF(note1).toWireString === note1Str)

    // toWF(value: CompletionInfo)
    assert(toWF(completionInfo).toWireString === """(:name "name" :type-sig (((("abc" "def") ("hij" "lmn"))) "ABC") :type-id 88 :is-callable nil :relevance 90 :to-insert "BAZ")""")

    // toWF(value: CompletionInfo)
    assert(toWF(completionInfo2).toWireString === """(:name "name2" :type-sig (((("abc" "def"))) "ABC") :type-id 90 :is-callable t :relevance 91 :to-insert nil)""")

    // toWF(value: CompletionInfoList)
    assert(toWF(CompletionInfoList("fooBar", List(completionInfo))).toWireString === """(:prefix "fooBar" :completions ((:name "name" :type-sig (((("abc" "def") ("hij" "lmn"))) "ABC") :type-id 88 :is-callable nil :relevance 90 :to-insert "BAZ")))""")

    // toWF(value: SymbolInfo)
    assert(toWF(new SymbolInfo("name", "localName", None, typeInfo, false, Some(2))).toWireString === """(:name "name" :local-name "localName" :decl-pos nil :type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :is-callable nil :owner-type-id 2)""")

    // toWF(value: NamedTypeMemberInfo)
    assert(toWF(new NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method)).toWireString === """(:name "typeX" :type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :pos nil :signature-string nil :decl-as method)""")

    // toWF(value: EntityInfo)
    assert(toWF(entityInfo).toWireString === entityInfoStr)

    // toWF(value: TypeInfo)
    assert(toWF(typeInfo).toWireString === typeInfoStr)

    // toWF(value: PackageInfo)
    assert(toWF(packageInfo).toWireString === """(:info-type package :name "name" :full-name "fullName" :members nil)""")

    // toWF(value: CallCompletionInfo)
    assert(toWF(new CallCompletionInfo(typeInfo, List(paramSectionInfo))).toWireString === """(:result-type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :param-sections ((:params (("ABC" (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8))) :is-implicit nil)))""")

    // toWF(value: InterfaceInfo)
    assert(toWF(interfaceInfo).toWireString === """(:type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :via-view "DEF")""")

    // toWF(value: TypeInspectInfo)
    assert(toWF(new TypeInspectInfo(typeInfo, Some(1), List(interfaceInfo))).toWireString === """(:type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :companion-id 1 :interfaces ((:type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :via-view "DEF")) :info-type typeInspect)""")

    // toWF(value: SymbolSearchResults)
    assert(toWF(new SymbolSearchResults(List(methodSearchRes, typeSearchRes))).toWireString === s"""((:type method :name "abc" :local-name "a" :decl-as method :pos (:type line :file $abd_str :line 10) :owner-name "ownerStr") (:type type :name "abc" :local-name "a" :decl-as trait :pos (:type line :file $abd_str :line 10)))""")

    // toWF(value: ImportSuggestions)
    assert(toWF(new ImportSuggestions(List(List(methodSearchRes, typeSearchRes)))).toWireString ===
      s"""(((:type method :name "abc" :local-name "a" :decl-as method :pos (:type line :file $abd_str :line 10) :owner-name "ownerStr") (:type type :name "abc" :local-name "a" :decl-as trait :pos (:type line :file $abd_str :line 10))))""")

    // toWF(value: SymbolSearchResult)
    assert(toWF(methodSearchRes).toWireString === s"""(:type method :name "abc" :local-name "a" :decl-as method :pos (:type line :file $abd_str :line 10) :owner-name "ownerStr")""")
    assert(toWF(typeSearchRes).toWireString === s"""(:type type :name "abc" :local-name "a" :decl-as trait :pos (:type line :file $abd_str :line 10))""")

    assert(toWF(new ERangePosition(batchSourceFile, 75, 70, 90)).toWireString === """(:file """ + batchSourceFile_str + """ :offset 75 :start 70 :end 90)""")

    assert(toWF(FileRange("/abc", 7, 9)).toWireString === """(:file "/abc" :start 7 :end 9)""")

    // TODO Add all the other symbols
    assert(toWF(SymbolDesignations(symFile, List(
      SymbolDesignation(7, 9, VarFieldSymbol),
      SymbolDesignation(11, 22, ClassSymbol)
    ))).toWireString === s"""(:file ${fileToWireString(symFile)} :syms ((varField 7 9) (class 11 22)))""")

    assert(toWF(RefactorFailure(7, "message")).toWireString === """(:procedure-id 7 :reason "message" :status failure)""")

    assert(toWF(refactorEffect).toWireString === refactorEffectStr)

    assert(toWF(refactorResult).toWireString === refactorResultStr)

    assert(toWF(DebugVmSuccess()).toWireString === """(:type success :status "success")""")
    assert(toWF(DebugVmError(303, "xxxx")).toWireString === """(:type error :error-code 303 :details "xxxx" :status "error")""")
  }
}
