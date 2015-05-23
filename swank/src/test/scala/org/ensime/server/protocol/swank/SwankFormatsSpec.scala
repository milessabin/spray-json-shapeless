package org.ensime.server.protocol.swank

import java.io.File
import org.scalatest._

import org.ensime.sexp._
import org.ensime.api._
import org.ensime.util._

import pimpathon.file._

class SwankFormatsSpec extends FlatSpec with Matchers with EnsimeTestData {
  import SwankProtocolRequest.RpcRequestFormat
  import SwankProtocolResponse.unhappyFamily
  import SwankProtocolResponse.EnsimeEventFormat

  import SwankTestData._

  def marshal(value: Any, via: Option[String] = None): Unit = {
    val sexp = value match {
      case e: EnsimeEvent => e.toSexp
      case other => unhappyFamily(other)
    }
    val string = sexp.compactPrint
    via match {
      case None => println(s"$value = $string")
      case Some(expected) => string shouldBe expected
    }
  }
  def marshal(value: Any, via: String): Unit = marshal(value, Some(via))

  def unmarshal(from: String, to: RpcRequest): Unit = {
    from.parseSexp.convertTo[RpcRequest] shouldBe to
  }

  "SWANK Formats" should "unmarshal startup messages" in {
    unmarshal(
      "(swank:connection-info)",
      ConnectionInfoReq: RpcRequest
    )
  }

  it should "unmarshal RpcSearchRequests" in {
    unmarshal(
      """(swank:public-symbol-search ("foo" "bar") 10)""",
      PublicSymbolSearchReq(List("foo", "bar"), 10): RpcRequest
    )

    unmarshal(
      s"""(swank:import-suggestions $file1_str 1 ("foo" "bar") 10)""",
      ImportSuggestionsReq(file1, 1, List("foo", "bar"), 10): RpcRequest
    )
  }

  it should "unmarshal RpcAnalyserRequests" in {
    unmarshal(
      s"""(swank:remove-file $file1_str)""",
      RemoveFileReq(file1): RpcRequest
    )

    unmarshal(
      s"""(swank:typecheck-file (:file $file1_str :contents "{/* code here */}" :contents-in $file2_str))""",
      TypecheckFileReq(sourceFileInfo): RpcRequest
    )

    unmarshal(
      s"""(swank:typecheck-files ($file1_str $file2_str))""",
      TypecheckFilesReq(List(file1, file2)): RpcRequest
    )

    unmarshal(
      """(swank:unload-all)""",
      UnloadAllReq: RpcRequest
    )

    unmarshal(
      """(swank:typecheck-all)""",
      TypecheckAllReq: RpcRequest
    )

    unmarshal(
      s"""(swank:format-source ($file1_str $file2_str))""",
      FormatSourceReq(List(file1, file2)): RpcRequest
    )

    unmarshal(
      s"""(swank:format-one-source (:file $file1_str :contents "{/* code here */}" :contents-in $file2_str))""",
      FormatOneSourceReq(sourceFileInfo): RpcRequest
    )

    unmarshal(
      s"""(swank:doc-uri-at-point $file1_str (1 10)))""",
      DocUriAtPointReq(file1, OffsetRange(1, 10)): RpcRequest
    )

    unmarshal(
      s"""(swank:doc-uri-for-symbol "foo.bar" "Baz" nil)""",
      DocUriForSymbolReq("foo.bar", Some("Baz"), None): RpcRequest
    )

    unmarshal(
      s"""(swank:completions (:file $file1_str :contents "{/* code here */}" :contents-in $file2_str) 10 100 t nil)""",
      CompletionsReq(sourceFileInfo, 10, 100, true, false): RpcRequest
    )

    unmarshal(
      """(swank:package-member-completion "foo" "bar")""",
      PackageMemberCompletionReq("foo", "bar"): RpcRequest
    )

    unmarshal(
      """(swank:call-completion 13)""",
      CallCompletionReq(13): RpcRequest
    )

    unmarshal(
      s"""(swank:uses-of-symbol-at-point $file1_str 100)""",
      UsesOfSymbolAtPointReq(file1, 100): RpcRequest
    )

    unmarshal(
      s"""(swank:type-by-id 13)""",
      TypeByIdReq(13): RpcRequest
    )

    unmarshal(
      s"""(swank:type-by-name "foo.bar")""",
      TypeByNameReq("foo.bar"): RpcRequest
    )

    unmarshal(
      s"""(swank:type-by-name-at-point "foo.bar" $file1_str (1 10))""",
      TypeByNameAtPointReq("foo.bar", file1, OffsetRange(1, 10)): RpcRequest
    )

    unmarshal(
      s"""(swank:type-at-point $file1_str (1 100))""",
      TypeAtPointReq(file1, OffsetRange(1, 100)): RpcRequest
    )

    unmarshal(
      s"""(swank:inspect-type-at-point $file1_str (1 100))""",
      InspectTypeAtPointReq(file1, OffsetRange(1, 100)): RpcRequest
    )

    unmarshal(
      s"""(swank:inspect-type-by-id 13)""",
      InspectTypeByIdReq(13): RpcRequest
    )

    unmarshal(
      s"""(swank:inspect-type-by-name "foo.Bar")""",
      InspectTypeByNameReq("foo.Bar"): RpcRequest
    )

    unmarshal(
      s"""(swank:symbol-at-point $file1_str 101)""",
      SymbolAtPointReq(file1, 101): RpcRequest
    )

    unmarshal(
      s"""(swank:symbol-by-name "foo.Bar" "baz" nil)""",
      SymbolByNameReq("foo.Bar", Some("baz"), None): RpcRequest
    )

    unmarshal(
      s"""(swank:inspect-package-by-path "foo.bar")""",
      InspectPackageByPathReq("foo.bar"): RpcRequest
    )

    unmarshal(
      s"""(swank:prepare-refactor 1 ignored (end 100 file $file1_str newName "bar" start 1) nil)""",
      PrepareRefactorReq(1, 'ignored, RenameRefactorDesc("bar", file1, 1, 100), false): RpcRequest
    )

    unmarshal(
      s"""(swank:exec-refactor 1 rename)""",
      ExecRefactorReq(1, RefactorType.Rename): RpcRequest
    )

    unmarshal(
      """(swank:cancel-refactor 1)""",
      CancelRefactorReq(1): RpcRequest
    )

    unmarshal(
      s"""(swank:symbol-designations $file1_str 1 100 (object val))""",
      SymbolDesignationsReq(
        file1, 1, 100,
        List(ObjectSymbol, ValSymbol)
      ): RpcRequest
    )

    unmarshal(
      s"""(swank:expand-selection $file1_str 100 200)""",
      ExpandSelectionReq(file1, 100, 200): RpcRequest
    )

    unmarshal(
      s"""(swank:implicit-info $file1_str (0 123))""",
      ImplicitInfoReq(file1, OffsetRange(0, 123))
    )

  }

  it should "unmarshal RpcDebugRequests" in {
    unmarshal(
      """(swank:debug-active-vm)""",
      DebugActiveVmReq: RpcRequest
    )

    unmarshal(
      """(swank:debug-start "blah blah blah")""",
      DebugStartReq("blah blah blah"): RpcRequest
    )

    unmarshal(
      """(swank:debug-attach "mylovelyhorse" "13")""",
      DebugAttachReq("mylovelyhorse", "13"): RpcRequest
    )

    unmarshal(
      """(swank:debug-stop)""",
      DebugStopReq: RpcRequest
    )

    unmarshal(
      s"""(swank:debug-set-break $file1_str 13)""",
      DebugSetBreakReq(file1, 13): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-clear-break $file1_str 13)""",
      DebugClearBreakReq(file1, 13): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-clear-all-breaks)""",
      DebugClearAllBreaksReq: RpcRequest
    )

    unmarshal(
      s"""(swank:debug-list-breakpoints)""",
      DebugListBreakpointsReq: RpcRequest
    )

    unmarshal(
      s"""(swank:debug-run)""",
      DebugRunReq: RpcRequest
    )

    unmarshal(
      s"""(swank:debug-continue "13")""",
      DebugContinueReq(dtid): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-step "13")""",
      DebugStepReq(dtid): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-next "13")""",
      DebugNextReq(dtid): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-step-out "13")""",
      DebugStepOutReq(dtid): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-locate-name "13" "foo")""",
      DebugLocateNameReq(dtid, "foo"): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-value (:type element :object-id "13" :index 14))""",
      DebugValueReq(debugLocationArray): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-to-string "13" (:type element :object-id "13" :index 14))""",
      DebugToStringReq(dtid, debugLocationArray): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-set-value (:type element :object-id "13" :index 14) "bar")""",
      DebugSetValueReq(debugLocationArray, "bar"): RpcRequest
    )

    unmarshal(
      s"""(swank:debug-backtrace "13" 100 200)""",
      DebugBacktraceReq(dtid, 100, 200): RpcRequest
    )

  }

  it should "marshal EnsimeGeneralEvent as EnsimeEvent" in {
    marshal(
      SendBackgroundMessageEvent("ABCDEF", 1): EnsimeEvent,
      """(:background-message 1 "ABCDEF")"""
    )

    marshal(
      AnalyzerReadyEvent: EnsimeEvent,
      "(:compiler-ready)"
    )

    marshal(
      FullTypeCheckCompleteEvent: EnsimeEvent,
      "(:full-typecheck-finished)"
    )

    marshal(
      IndexerReadyEvent: EnsimeEvent,
      "(:indexer-ready)"
    )

    marshal(
      NewScalaNotesEvent(
        isFull = false,
        List(new Note("foo.scala", "testMsg", NoteWarn, 50, 55, 77, 5))
      ): EnsimeEvent,
      """(:scala-notes (:is-full nil :notes ((:file "foo.scala" :msg "testMsg" :severity warn :beg 50 :end 55 :line 77 :col 5))))"""
    )

    marshal(
      ClearAllScalaNotesEvent: EnsimeEvent,
      "(:clear-all-scala-notes)"
    )
  }

  it should "marshal DebugEvent as EnsimeEvent" in {
    marshal(
      DebugOutputEvent("XXX"): EnsimeEvent,
      """(:debug-event (:type output :body "XXX"))"""
    )

    marshal(
      DebugStepEvent(DebugThreadId(207), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:debug-event (:type step :thread-id "207" :thread-name "threadNameStr" :file $file1_str :line 57))"""
    )

    marshal(
      DebugBreakEvent(DebugThreadId(209), "threadNameStr", sourcePos1.file, sourcePos1.line): EnsimeEvent,
      s"""(:debug-event (:type breakpoint :thread-id "209" :thread-name "threadNameStr" :file $file1_str :line 57))"""
    )

    marshal(
      DebugVMStartEvent: EnsimeEvent,
      """(:debug-event (:type start))"""
    )
    marshal(
      DebugVMDisconnectEvent: EnsimeEvent,
      """(:debug-event (:type disconnect))"""
    )
    marshal(
      DebugExceptionEvent(33L, dtid, "threadNameStr", Some(sourcePos1.file), Some(sourcePos1.line)): EnsimeEvent,
      s"""(:debug-event (:type exception :exception 33 :thread-id "13" :thread-name "threadNameStr" :file $file1_str :line 57))"""
    )
    marshal(
      DebugExceptionEvent(33L, dtid, "threadNameStr", None, None): EnsimeEvent,
      """(:debug-event (:type exception :exception 33 :thread-id "13" :thread-name "threadNameStr" :file nil :line nil))"""
    )

    marshal(
      DebugThreadStartEvent(dtid): EnsimeEvent,
      """(:debug-event (:type threadStart :thread-id "13"))"""
    )
    marshal(
      DebugThreadDeathEvent(dtid): EnsimeEvent,
      """(:debug-event (:type threadDeath :thread-id "13"))"""
    )
  }

  it should "marshal DebugLocation" in {
    marshal(
      DebugObjectReference(57L): DebugLocation,
      """(:type reference :object-id "57")"""
    )

    marshal(
      DebugArrayElement(DebugObjectId(58L), 2): DebugLocation,
      """(:type element :object-id "58" :index 2)"""
    )

    marshal(
      DebugObjectField(DebugObjectId(58L), "fieldName"): DebugLocation,
      """(:type field :object-id "58" :field "fieldName")"""
    )

    marshal(
      DebugStackSlot(DebugThreadId(27), 12, 23): DebugLocation,
      """(:type slot :thread-id "27" :frame 12 :offset 23)"""
    )
  }

  it should "marshal DebugValue" in {
    marshal(
      DebugPrimitiveValue("summaryStr", "typeNameStr"): DebugValue,
      """(:val-type prim :summary "summaryStr" :type-name "typeNameStr")"""
    )

    marshal(
      DebugStringInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:val-type str :summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id "5")"""
    )

    marshal(
      DebugObjectInstance("summaryStr", List(debugClassField), "typeNameStr", DebugObjectId(5L)): DebugValue,
      """(:val-type obj :summary "summaryStr" :fields ((:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")) :type-name "typeNameStr" :object-id "5")"""
    )

    marshal(
      DebugNullValue("typeNameStr"): DebugValue,
      """(:val-type null :type-name "typeNameStr")"""
    )

    marshal(
      DebugArrayInstance(3, "typeName", "elementType", DebugObjectId(5L)): DebugValue,
      """(:val-type arr :length 3 :type-name "typeName" :element-type-name "elementType" :object-id "5")"""
    )

    marshal(
      debugClassField: DebugClassField,
      """(:index 19 :name "nameStr" :type-name "typeNameStr" :summary "summaryStr")"""
    )

    marshal(
      debugStackLocal1: DebugStackLocal,
      """(:index 3 :name "name1" :summary "summary1" :type-name "type1")"""
    )

    marshal(
      debugStackFrame: DebugStackFrame,
      s"""(:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file $file1_str :line 57) :this-object-id "7")"""
    )

    marshal(
      DebugBacktrace(List(debugStackFrame), dtid, "thread1"): DebugBacktrace,
      s"""(:frames ((:index 7 :locals ((:index 3 :name "name1" :summary "summary1" :type-name "type1") (:index 4 :name "name2" :summary "summary2" :type-name "type2")) :num-args 4 :class-name "class1" :method-name "method1" :pc-location (:file $file1_str :line 57) :this-object-id "7")) :thread-id "13" :thread-name "thread1")"""
    )

    marshal(
      sourcePos1: SourcePosition,
      s"""(:type line :file $file1_str :line 57)"""
    )
    marshal(
      sourcePos2: SourcePosition,
      s"""(:type line :file $file1_str :line 59)"""
    )
    marshal(
      sourcePos3: SourcePosition,
      "(:type empty)"
    )
    marshal(
      sourcePos4: SourcePosition,
      s"""(:type offset :file $file1_str :offset 456)"""
    )

    marshal(
      breakPoint1: Breakpoint,
      s"""(:file $file1_str :line 57)"""
    )

    marshal(
      BreakpointList(List(breakPoint1), List(breakPoint2)): BreakpointList,
      s"""(:active ((:file $file1_str :line 57)) :pending ((:file $file1_str :line 59)))"""
    )

    marshal(
      DebugVmSuccess(): DebugVmStatus,
      """(:type success :status "success")"""
    )

    marshal(
      DebugVmError(303, "xxxx"): DebugVmStatus,
      """(:type error :error-code 303 :details "xxxx" :status "error")"""
    )
  }

  it should "marshal various informational types" in {
    marshal(
      note1: Note,
      note1Str
    )

    marshal(
      completionInfo: CompletionInfo,
      """(:name "name" :type-sig (((("abc" "def") ("hij" "lmn"))) "ABC") :type-id 88 :is-callable nil :relevance 90 :to-insert "BAZ")"""
    )

    marshal(
      completionInfo2: CompletionInfo,
      """(:name "name2" :type-sig (((("abc" "def"))) "ABC") :type-id 90 :is-callable t :relevance 91 :to-insert nil)"""
    )

    marshal(
      CompletionInfoList("fooBar", List(completionInfo)): CompletionInfoList,
      """(:prefix "fooBar" :completions ((:name "name" :type-sig (((("abc" "def") ("hij" "lmn"))) "ABC") :type-id 88 :is-callable nil :relevance 90 :to-insert "BAZ")))"""
    )

    marshal(
      new SymbolInfo("name", "localName", None, typeInfo, false, Some(2)): SymbolInfo,
      """(:name "name" :local-name "localName" :decl-pos nil :type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :is-callable nil :owner-type-id 2)"""
    )

    marshal(
      new NamedTypeMemberInfo("typeX", typeInfo, None, None, DeclaredAs.Method): EntityInfo,
      """(:name "typeX" :type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :pos nil :signature-string nil :decl-as method)"""
    )

    marshal(
      entityInfo: EntityInfo,
      entityInfoStr
    )

    marshal(
      typeInfo: EntityInfo,
      typeInfoStr
    )

    marshal(
      packageInfo: EntityInfo,
      """(:info-type package :name "name" :full-name "fullName" :members nil)"""
    )

    marshal(
      new CallCompletionInfo(typeInfo, List(paramSectionInfo)): CallCompletionInfo,
      """(:result-type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :param-sections ((:params (("ABC" (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8))) :is-implicit nil)))"""
    )

    marshal(
      interfaceInfo: InterfaceInfo,
      """(:type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :via-view "DEF")"""
    )

    marshal(
      new TypeInspectInfo(typeInfo, Some(1), List(interfaceInfo)): TypeInspectInfo,
      """(:type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :companion-id 1 :interfaces ((:type (:arrow-type nil :name "type1" :type-id 7 :decl-as method :full-name "FOO.type1" :type-args nil :members nil :pos nil :outer-type-id 8) :via-view "DEF")) :info-type typeInspect)"""
    )
  }

  it should "marshal search related responses" in {
    marshal(
      new SymbolSearchResults(List(methodSearchRes, typeSearchRes)): SymbolSearchResults,
      s"""((:type method :name "abc" :local-name "a" :decl-as method :pos (:type line :file $abd_str :line 10) :owner-name "ownerStr") (:type type :name "abc" :local-name "a" :decl-as trait :pos (:type line :file $abd_str :line 10)))"""
    )

    marshal(
      new ImportSuggestions(List(List(methodSearchRes, typeSearchRes))): ImportSuggestions,
      s"""(((:type method :name "abc" :local-name "a" :decl-as method :pos (:type line :file $abd_str :line 10) :owner-name "ownerStr") (:type type :name "abc" :local-name "a" :decl-as trait :pos (:type line :file $abd_str :line 10))))"""
    )

    marshal(
      methodSearchRes: SymbolSearchResult,
      s"""(:type method :name "abc" :local-name "a" :decl-as method :pos (:type line :file $abd_str :line 10) :owner-name "ownerStr")"""
    )

    marshal(
      typeSearchRes: SymbolSearchResult,
      s"""(:type type :name "abc" :local-name "a" :decl-as trait :pos (:type line :file $abd_str :line 10))"""
    )
  }

  it should "marshal ranges and semantic highlighting" in {
    marshal(
      new ERangePosition(batchSourceFile, 75, 70, 90): ERangePosition,
      """(:file """ + batchSourceFile_str + """ :offset 75 :start 70 :end 90)"""
    )

    marshal(
      FileRange("/abc", 7, 9): FileRange,
      """(:file "/abc" :start 7 :end 9)"""
    )

    marshal(
      SymbolDesignations(
        symFile, List(
        SymbolDesignation(7, 9, VarFieldSymbol),
        SymbolDesignation(11, 22, ClassSymbol)
      )
      ): SymbolDesignations,
      s"""(:file ${fileToWireString(symFile)} :syms ((varField 7 9) (class 11 22)))"""
    )

    marshal(
      List(ImplicitConversionInfo(5, 6, symbolInfo)): List[ImplicitInfo],
      s"""((:type conversion :start 5 :end 6 :fun $symbolInfoStr))"""
    )

    marshal(
      List(ImplicitParamInfo(5, 6, symbolInfo, List(symbolInfo, symbolInfo), true)): List[ImplicitInfo],
      s"""((:type param :start 5 :end 6 :fun $symbolInfoStr :params ($symbolInfoStr $symbolInfoStr) :fun-is-implicit t))"""
    )
  }

  it should "marshal refactoring messages" in {
    marshal(
      RefactorFailure(7, "message"): RefactorFailure,
      """(:procedure-id 7 :reason "message" :status failure)"""
    )

    marshal(
      refactorEffect: RefactorEffect,
      s"""(:procedure-id 9 :refactor-type addImport :changes ((:type edit :file $file3_str :from 5 :to 7 :text "aaa")) :status success)"""
    )

    marshal(
      refactorResult: RefactorResult,
      s"""(:procedure-id 7 :refactor-type addImport :touched-files ($file3_str $file1_str) :status success)"""
    )
  }

}
