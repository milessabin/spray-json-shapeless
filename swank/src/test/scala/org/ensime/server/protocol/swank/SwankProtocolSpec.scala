package org.ensime.server.protocol.swank

import java.io._
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ CountDownLatch, TimeUnit }

import org.ensime.api._

import org.ensime.sexp._
import org.ensime.EnsimeApi
import org.ensime.util.UnitTestUtils._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ BeforeAndAfterAll, FunSpec, ShouldMatchers }

import pimpathon.file._

class SwankProtocolSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll with MockFactory {

  import org.ensime.server.protocol.swank.SwankTestData._

  // transitionary methods
  def ContentsSourceFileInfo(file: File, contents: String) =
    SourceFileInfo(file, Some(contents))
  def ContentsInSourceFileInfo(file: File, contentsIn: File) =
    SourceFileInfo(file, contentsIn = Some(contentsIn))

  describe("SwankProtocol") {
    trait MsgHandler {
      def send(s: String)
    }

    // allows us to backtrack from a TimedOutException and show failed expectations
    // https://github.com/paulbutcher/ScalaMock/issues/98
    autoVerify = false

    val nextId = new AtomicInteger(1)
    def testWithResponse(msg: String)(expectation: (EnsimeApi, MsgHandler, Int) => Unit): Unit = {
      var timedOut = false
      withExpectations {
        withActorSystem { actorSystem =>
          val t = mock[EnsimeApi]
          val out = mock[MsgHandler]

          val latch = new CountDownLatch(1)

          val prot = new SwankProtocol(null, t) {
            override def sendMessage(o: Sexp): Unit = {
              out.send(o.compactPrint)
              latch.countDown()
            }
          }

          val rpcId = nextId.getAndIncrement
          expectation(t, out, rpcId)

          val sexp = SexpParser.parse("(:swank-rpc " + msg + " " + rpcId + ")")

          prot.handleIncomingMessage(sexp)

          if (!latch.await(1000, TimeUnit.MILLISECONDS))
            timedOut = true
        }
      }
      if (timedOut) fail("timed out waiting for responses")
    }

    it("should understand swank:peek-undo - success") {
      val file3 = file("/foo/abc").canon
      val file3_str = fileToWireString(file3)

      testWithResponse("""(swank:peek-undo)""") { (t, m, id) =>
        (t.peekUndo _).expects().returns(Some(Undo(3, "Undoing stuff", List(TextEdit(file3, 5, 7, "aaa")))))
        (m.send _).expects(s"""(:return (:ok (:id 3 :summary "Undoing stuff" :changes ((:type edit :file $file3_str :from 5 :to 7 :text "aaa")))) $id)""")
      }
    }

    it("should understand swank:peek-undo - fail") {
      testWithResponse("""(swank:peek-undo)""") { (t, m, id) =>
        (t.peekUndo _).expects().returns(None)
        (m.send _).expects(s"""(:return (:abort 206 "No such undo.") $id)""")
      }
    }

    it("should understand swank:exec-undo") {
      testWithResponse("""(swank:exec-undo 1)""") { (t, m, id) =>
        (t.execUndo _).expects(1).returns(Right(undoResult))
        (m.send _).expects(s"""(:return (:ok $undoResultStr) $id)""")

      }
    }

    it("should understand connection-info request") {
      testWithResponse("(swank:connection-info)") { (t, m, id) =>
        (t.connectionInfo _).expects().returns(new ConnectionInfo())
        (m.send _).expects(s"""(:return (:ok (:pid nil :implementation (:name "ENSIME") :version "0.8.14")) $id)""")
      }
    }

    it("should understand swank:repl-config") {
      testWithResponse("(swank:repl-config)") { (t, m, id) =>
        (t.replConfig _).expects().returns(replConfig)
        (m.send _).expects(s"""(:return (:ok $replConfigStr) $id)""")
      }
    }

    it("should understand swank:remove-file") {
      testWithResponse("""(swank:remove-file "Analyzer.scala")""") { (t, m, id) =>
        (t.removeFile _).expects(file("Analyzer.scala").canon)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:typecheck-file with SourceFileInfo") {
      testWithResponse("""(swank:typecheck-file (:file "Analyzer.scala"))""") { (t, m, id) =>
        (t.typecheckFile _).expects(SourceFileInfo(analyzerFile))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:typecheck-file with ContentsSourceFileInfo") {
      testWithResponse("""(swank:typecheck-file (:file "Analyzer.scala" :contents "abc"))""") { (t, m, id) =>
        (t.typecheckFile _).expects(ContentsSourceFileInfo(analyzerFile, "abc"))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:typecheck-file with ContentsInSourceFileInfo") {
      testWithResponse("""(swank:typecheck-file (:file "Analyzer.scala" :contents-in "Foo.scala"))""") { (t, m, id) =>
        (t.typecheckFile _).expects(ContentsInSourceFileInfo(analyzerFile, fooFile))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:typecheck-files") {
      testWithResponse("""(swank:typecheck-files ("Analyzer.scala" "Foo.scala"))""") { (t, m, id) =>
        (t.typecheckFiles _).expects(List(analyzerFile, fooFile))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:patch-source") {
      testWithResponse("""(swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("-" 7127 7128) ("*" 7200 7300 "Bob")))""") { (t, m, id) =>
        (t.patchSource _).expects(file("Analyzer.scala").canon, List(PatchInsert(6461, "Inc"), PatchDelete(7127, 7128), PatchReplace(7200, 7300, "Bob")))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:typecheck-all") {
      testWithResponse("""(swank:typecheck-all)""") { (t, m, id) =>
        (t.typecheckAll _).expects()
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:unload-all") {
      testWithResponse("""(swank:unload-all)""") { (t, m, id) =>
        (t.unloadAll _).expects()
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:format-source") {
      testWithResponse("""(swank:format-source ("/ensime/src/Test.scala"))""") { (t, m, id) =>
        (t.formatFiles _).expects(List(file("/ensime/src/Test.scala").canon))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    // TODO add test for exception handling in above

    it("should understand swank:format-one-source") {
      testWithResponse("""(swank:format-one-source (:file "Analyzer.scala"))""") { (t, m, id) =>
        (t.formatFile _).expects(SourceFileInfo(analyzerFile)).returns("Some string")
        (m.send _).expects(s"""(:return (:ok "Some string") $id)""")
      }
    }

    it("should understand swank:public-symbol-search") {
      testWithResponse("""(swank:public-symbol-search ("java" "io" "File") 50)""") { (t, m, id) =>
        (t.publicSymbolSearch _).expects(List("java", "io", "File"), 50).returns(symbolSearchResults)
        (m.send _).expects(s"""(:return (:ok $symbolSearchResultsStr) $id)""")
      }
    }

    it("should understand swank:import-suggestions") {
      testWithResponse("""(swank:import-suggestions "/ensime/src/main/scala/org/ensime/server/Analyzer.scala" 2300 ("Actor") 10)""") { (t, m, id) =>
        (t.importSuggestions _).expects(file("/ensime/src/main/scala/org/ensime/server/Analyzer.scala").canon, 2300,
          List("Actor"), 10).returns(importSuggestions)
        (m.send _).expects(s"""(:return (:ok $importSuggestionsStr) $id)""")
      }
    }

    it("should understand swank:completions with SourceFileInfo") {
      val f = file("ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala").canon
      testWithResponse(
        s"""(swank:completions (:file ${fileToWireString(f)}) 22626 0 t t)"""
      ) { (t, m, id) =>
          (t.completionsAtPoint _).expects(
            SourceFileInfo(f), 22626, 0, true, true
          ).returns(completionInfoCList)
          (m.send _).expects(s"""(:return (:ok $completionInfoCListStr) $id)""")
        }
    }

    it("should understand swank:completions with ContentsSourceFileInfo") {
      val f = file("ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala").canon
      testWithResponse(
        s"""(swank:completions (:file ${fileToWireString(f)} :contents "zz") 22626 0 t t)"""
      ) { (t, m, id) =>
          (t.completionsAtPoint _).expects(
            ContentsSourceFileInfo(f, "zz"), 22626, 0, true, true
          ).returns(completionInfoCList)
          (m.send _).expects(s"""(:return (:ok $completionInfoCListStr) $id)""")
        }
    }

    it("should understand swank:completions with ContentsInSourceFileInfo") {
      val f = file("ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala").canon
      testWithResponse(
        s"""(swank:completions (:file ${fileToWireString(f)} :contents-in "Foo.scala") 22626 0 t t)"""
      ) { (t, m, id) =>
          (t.completionsAtPoint _).expects(
            ContentsInSourceFileInfo(f, fooFile), 22626, 0, true, true
          ).returns(completionInfoCList)
          (m.send _).expects(s"""(:return (:ok $completionInfoCListStr) $id)""")
        }
    }

    it("should understand swank:package-member-completion") {
      testWithResponse("""(swank:package-member-completion "org.ensime.server" "Server")""") { (t, m, id) =>
        (t.packageMemberCompletion _).expects("org.ensime.server", "Server").returns(completionInfoList)
        (m.send _).expects(s"""(:return (:ok $completionInfoListStr) $id)""")
      }
    }

    it("should understand swank:call-completion") {
      testWithResponse("""(swank:call-completion 1)""") { (t, m, id) =>
        (t.callCompletion _).expects(1).returns(Some(callCompletionInfo))
        (m.send _).expects(s"""(:return (:ok $callCompletionInfoStr) $id)""")
      }
    }

    it("should understand swank:uses-of-symbol-at-point") {
      testWithResponse("""(swank:uses-of-symbol-at-point "Test.scala" 11334)""") { (t, m, id) =>
        (t.usesOfSymAtPoint _).expects(file("Test.scala").canon, 11334).returns(List(rangePos1, rangePos2))
        (m.send _).expects(s"""(:return (:ok ($rangePos1Str $rangePos2Str)) $id)""")
      }
    }

    it("should understand swank:symbol-by-name") {
      testWithResponse("""(swank:symbol-by-name "org.example.A" "x" nil)""") { (t, m, id) =>
        (t.symbolByName _).expects("org.example.A", Some("x"), None).returns(Some(symbolInfo))
        (m.send _).expects(s"""(:return (:ok $symbolInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-id") {
      testWithResponse("""(swank:type-by-id 7)""") { (t, m, id) =>
        (t.typeById _).expects(7).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-name") {
      testWithResponse("""(swank:type-by-name "java.lang.String")""") { (t, m, id) =>
        (t.typeByName _).expects("java.lang.String").returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-name-at-point at point") {
      testWithResponse("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.typeByNameAtPoint _).expects("String", file("SwankProtocol.scala").canon, OffsetRange(31680)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-name-at-point at range") {
      testWithResponse("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.typeByNameAtPoint _).expects("String", file("SwankProtocol.scala").canon, OffsetRange(31680, 31691)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-at-point at point") {
      testWithResponse("""(swank:type-at-point "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.typeAtPoint _).expects(file("SwankProtocol.scala").canon, OffsetRange(31680)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-at-point at range") {
      testWithResponse("""(swank:type-at-point "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.typeAtPoint _).expects(file("SwankProtocol.scala").canon, OffsetRange(31680, 31691)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-at-point at point") {
      testWithResponse("""(swank:inspect-type-at-point "SwankProtocol.scala" 32736)""") { (t, m, id) =>
        (t.inspectTypeAtPoint _).expects(file("SwankProtocol.scala").canon, OffsetRange(32736)).returns(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-at-point at range") {
      testWithResponse("""(swank:inspect-type-at-point "SwankProtocol.scala" (32736 32740))""") { (t, m, id) =>
        (t.inspectTypeAtPoint _).expects(file("SwankProtocol.scala").canon, OffsetRange(32736, 32740)).returns(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-by-id") {
      testWithResponse("""(swank:inspect-type-by-id 232)""") { (t, m, id) =>
        (t.inspectTypeById _).expects(232).returning(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-by-name") {
      testWithResponse("""(swank:inspect-type-by-name "abc.d")""") { (t, m, id) =>
        (t.inspectTypeByName _).expects("abc.d").returning(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand symbol-at-point") {
      testWithResponse("""(swank:symbol-at-point "SwankProtocol.scala" 36483)""") { (t, m, id) =>
        (t.symbolAtPoint _).expects(file("SwankProtocol.scala").canon, 36483).returns(Some(symbolInfo))
        (m.send _).expects(s"""(:return (:ok $symbolInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-package-by-path") {
      testWithResponse("""(swank:inspect-package-by-path "org.ensime.util")""") { (t, m, id) =>
        (t.inspectPackageByPath _).expects("org.ensime.util").returns(Some(packageInfo))
        (m.send _).expects(s"""(:return (:ok $packageInfoStr) $id)""")
      }
    }

    it("should understand swank:prepare-refactor - inline local - prepare failure") {
      testWithResponse("""(swank:prepare-refactor 7 inlineLocal (file "SwankProtocol.scala" start 100 end 200) t)""") { (t, m, id) =>

        //        Either[RefactorFailure, RefactorEffect]
        (t.prepareRefactor _).expects(7, InlineLocalRefactorDesc(file("SwankProtocol.scala").canon, 100, 200)).returns(
          Left(refactorFailure)
        )
        (m.send _).expects(s"""(:return (:ok $refactorFailureStr) $id)""")
      }
    }
    it("should understand swank:prepare-refactor - inline local - prepare - interactive") {
      testWithResponse("""(swank:prepare-refactor 7 inlineLocal (file "SwankProtocol.scala" start 100 end 200) t)""") { (t, m, id) =>
        (t.prepareRefactor _).expects(7, InlineLocalRefactorDesc(file("SwankProtocol.scala").canon, 100, 200)).returns(
          Right(refactorRenameEffect)
        )
        (m.send _).expects(s"""(:return (:ok $refactorRenameEffectStr) $id)""")
      }
    }

    it("should understand swank:prepare-refactor - inline local - non-interactive - failure") {
      testWithResponse("""(swank:prepare-refactor 7 inlineLocal (file "SwankProtocol.scala" start 100 end 200) nil)""") { (t, m, id) =>

        (t.prepareRefactor _).expects(7, InlineLocalRefactorDesc(file("SwankProtocol.scala").canon, 100, 200)).returns(Right(refactorRenameEffect))
        (t.execRefactor _).expects(7, RefactorType.InlineLocal).returns(Left(refactorFailure))

        (m.send _).expects(s"""(:return (:ok $refactorFailureStr) $id)""")
      }
    }

    def testRefactorMethod(msg: String, desc: RefactorDesc): Unit = {

      val refactorEffect = new RefactorEffect(7, desc.refactorType, List(TextEdit(file3, 5, 7, "aaa")))
      val refactorResult = new RefactorResult(7, desc.refactorType, List(file3, file1))
      val refactorResultStr = s"""(:procedure-id 7 :refactor-type ${desc.refactorType.symbol.name} :touched-files ($file3_str $file1_str) :status success)"""

      testWithResponse("""(swank:prepare-refactor 7""" + msg + " nil)") { (t, m, id) =>
        (t.prepareRefactor _).expects(7, desc).returns(Right(refactorEffect))
        (t.execRefactor _).expects(7, desc.refactorType).returns(Right(refactorResult))
        (m.send _).expects(s"""(:return (:ok $refactorResultStr) $id)""")
      }
    }

    it("should understand swank:prepare-refactor - inline local - non-interactive") {
      testRefactorMethod(
        """inlineLocal (file "SwankProtocol.scala" start 100 end 200)""",
        InlineLocalRefactorDesc(file("SwankProtocol.scala").canon, 100, 200)
      )
    }

    it("should understand swank:prepare-refactor - rename") {
      testRefactorMethod(
        """rename (file "SwankProtocol.scala" start 39504 end 39508 newName "dude")""",
        RenameRefactorDesc("dude", file("SwankProtocol.scala").canon, 39504, 39508)
      )
    }

    it("should understand swank:prepare-refactor - extractMethod") {
      testRefactorMethod(
        """extractMethod (methodName "foo" file "SwankProtocol.scala" start 39504 end 39508)""",
        ExtractMethodRefactorDesc("foo", file("SwankProtocol.scala").canon, 39504, 39508)
      )
    }

    it("should understand swank:prepare-refactor - extractLocal") {
      testRefactorMethod(
        """extractLocal (name "foo" file "SwankProtocol.scala" start 39504 end 39508)""",
        ExtractLocalRefactorDesc("foo", file("SwankProtocol.scala").canon, 39504, 39508)
      )
    }

    it("should understand swank:prepare-refactor - organiseImports") {
      testRefactorMethod(
        """organiseImports (file "SwankProtocol.scala")""",
        OrganiseImportsRefactorDesc(file("SwankProtocol.scala").canon)
      )
    }

    it("should not fail on swank:prepareRefactor from bug 573") {
      testRefactorMethod(
        """organizeImports (file "src/main/scala/org/ensime/indexer/DatabaseService.scala")""",
        OrganiseImportsRefactorDesc(file("src/main/scala/org/ensime/indexer/DatabaseService.scala").canon)
      )
    }

    it("should understand swank:prepare-refactor - addImport") {
      testRefactorMethod(
        """addImport (qualifiedName "com.bar.Foo" file "SwankProtocol.scala" start 39504 end 39508)""",
        AddImportRefactorDesc("com.bar.Foo", file("SwankProtocol.scala").canon)
      )

      testRefactorMethod(
        """addImport (qualifiedName "com.bar.Foo" file "SwankProtocol.scala")""",
        AddImportRefactorDesc("com.bar.Foo", file("SwankProtocol.scala").canon)
      )
    }

    it("should understand swank:exec-refactor - refactor failure") {
      testWithResponse("""(swank:exec-refactor 7 rename)""") { (t, m, id) =>
        (t.execRefactor _).expects(7, RefactorType.Rename).returns(Left(refactorFailure))
        (m.send _).expects(s"""(:return (:ok $refactorFailureStr) $id)""")
      }
    }

    it("should understand swank:exec-refactor - refactor result") {
      testWithResponse("""(swank:exec-refactor 7 rename)""") { (t, m, id) =>
        (t.execRefactor _).expects(7, RefactorType.Rename).returns(Right(refactorResult))
        (m.send _).expects(s"""(:return (:ok $refactorResultStr) $id)""")
      }
    }

    it("should understand swank:cancel-refactor") {
      testWithResponse("""(swank:cancel-refactor 1)""") { (t, m, id) =>
        (t.cancelRefactor _).expects(1)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:symbol-designations") {
      testWithResponse("""(swank:symbol-designations "SwankProtocol.scala" 0 46857 (var val varField valField))""") {
        (t, m, id) =>
          (t.symbolDesignations _).expects(file("SwankProtocol.scala").canon, 0, 46857,
            List[SourceSymbol](VarSymbol, ValSymbol, VarFieldSymbol, ValFieldSymbol)).returns(symbolDesignations)
          (m.send _).expects(s"""(:return (:ok $symbolDesignationsStr) $id)""")
      }
    }

    it("should understand swank:expand-selection") {
      testWithResponse("""(swank:expand-selection "Model.scala" 4648 4721)""") { (t, m, id) =>
        (t.expandSelection _).expects(file("Model.scala").canon, 4648, 4721).returns(fileRange)
        (m.send _).expects(s"""(:return (:ok $fileRangeStr) $id)""")
      }
    }

    it("should understand swank:debug-active-vm - good") {
      testWithResponse("""(swank:debug-active-vm)""") { (t, m, id) =>
        (t.debugActiveVM _).expects().returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-active-vm - bad") {
      testWithResponse("""(swank:debug-active-vm)""") { (t, m, id) =>
        (t.debugActiveVM _).expects().returns(false)
        (m.send _).expects(s"""(:return (:ok nil) $id)""")
      }
    }

    it("should understand swank:debug-start - success") {
      testWithResponse("""(swank:debug-start "org.hello.HelloWorld arg")""") { (t, m, id) =>
        (t.debugStartVM _).expects("org.hello.HelloWorld arg").returns(DebugVmSuccess())
        (m.send _).expects(s"""(:return (:ok (:type success :status "success")) $id)""")
      }
    }

    it("should understand swank:debug-start - failure") {
      testWithResponse("""(swank:debug-start "org.hello.HelloWorld arg")""") { (t, m, id) =>
        (t.debugStartVM _).expects("org.hello.HelloWorld arg").returns(DebugVmError(303, "xxxx"))
        (m.send _).expects(s"""(:return (:ok (:type error :error-code 303 :details "xxxx" :status "error")) $id)""")
      }
    }

    it("should understand swank:debug-attach - success") {
      testWithResponse("""(swank:debug-attach "localhost" "9000")""") { (t, m, id) =>
        (t.debugAttachVM _).expects("localhost", "9000").returns(DebugVmSuccess())
        (m.send _).expects(s"""(:return (:ok (:type success :status "success")) $id)""")
      }
    }

    it("should understand swank:debug-attach - failure") {
      testWithResponse("""(swank:debug-attach "localhost" "9000")""") { (t, m, id) =>
        (t.debugAttachVM _).expects("localhost", "9000").returns(DebugVmError(303, "xxxx"))
        (m.send _).expects(s"""(:return (:ok (:type error :error-code 303 :details "xxxx" :status "error")) $id)""")
      }
    }

    it("should understand swank:debug-stop - good") {
      testWithResponse("""(swank:debug-stop)""") { (t, m, id) =>
        (t.debugStopVM _).expects().returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-stop - bad") {
      testWithResponse("""(swank:debug-stop)""") { (t, m, id) =>
        (t.debugStopVM _).expects().returns(false)
        (m.send _).expects(s"""(:return (:ok nil) $id)""")
      }
    }

    it("should understand swank:debug-set-break") {
      testWithResponse("""(swank:debug-set-break "hello.scala" 12)""") { (t, m, id) =>
        (t.debugSetBreakpoint _).expects(file("hello.scala").canon, 12)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-clear-break") {
      testWithResponse("""(swank:debug-clear-break "hello.scala" 12)""") { (t, m, id) =>
        (t.debugClearBreakpoint _).expects(file("hello.scala").canon, 12)
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("should understand swank:debug-clear-all-breaks") {
      testWithResponse("""(swank:debug-clear-all-breaks)""") { (t, m, id) =>
        (t.debugClearAllBreakpoints _).expects()
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("debug-list-breakpoints test") {
      testWithResponse("""(swank:debug-list-breakpoints)""") { (t, m, id) =>
        (t.debugListBreakpoints _).expects().returns(breakpointList)
        (m.send _).expects(s"""(:return (:ok $breakpointListStr) $id)""")
      }
    }

    it("should understand swank:debug-run") {
      testWithResponse("""(swank:debug-run)""") { (t, m, id) =>
        (t.debugRun _).expects().returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-continue") {
      testWithResponse("""(swank:debug-continue "1")""") { (t, m, id) =>
        (t.debugContinue _).expects(DebugThreadId(1)).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-step") {
      testWithResponse("""(swank:debug-step "982398123")""") { (t, m, id) =>
        (t.debugStep _).expects(DebugThreadId(982398123)).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-next") {
      testWithResponse("""(swank:debug-next "982398123")""") { (t, m, id) =>
        (t.debugNext _).expects(DebugThreadId(982398123)).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-step-out") {
      testWithResponse("""(swank:debug-step-out "982398123")""") { (t, m, id) =>
        (t.debugStepOut _).expects(DebugThreadId(982398123)).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-locate-name") {
      testWithResponse("""(swank:debug-locate-name "7" "apple")""") { (t, m, id) =>
        (t.debugLocateName _).expects(DebugThreadId(7), "apple").returns(Some(debugLocObjectRef))
        (m.send _).expects("(:return (:ok " + debugLocObjectRefStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - array") {
      testWithResponse("""(swank:debug-value (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.debugValue _).expects(DebugArrayElement(DebugObjectId(23L), 2)).returns(Some(debugNullValue))
        (m.send _).expects("(:return (:ok " + debugNullValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - object") {
      testWithResponse("""(swank:debug-value (:type reference :object-id "23"))""") { (t, m, id) =>
        (t.debugValue _).expects(DebugObjectReference(23L)).returns(Some(debugArrayInstValue))
        (m.send _).expects("(:return (:ok " + debugArrayInstValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - object field") {
      testWithResponse("""(swank:debug-value (:type field :object-id "23" :field "fred"))""") { (t, m, id) =>
        (t.debugValue _).expects(DebugObjectField(DebugObjectId(23L), "fred")).returns(Some(debugPrimitiveValue))
        (m.send _).expects("(:return (:ok " + debugPrimitiveValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - stack slot") {
      testWithResponse("""(swank:debug-value (:type slot :thread-id "23" :frame 7 :offset 25))""") { (t, m, id) =>
        (t.debugValue _).expects(DebugStackSlot(DebugThreadId(23), 7, 25)).returns(Some(debugStringValue))
        (m.send _).expects("(:return (:ok " + debugStringValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-to-string - array element") {
      testWithResponse("""(swank:debug-to-string "2" (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.debugToString _).expects(DebugThreadId(2), DebugArrayElement(DebugObjectId(23L), 2)).returns(Some("null"))
        (m.send _).expects("(:return (:ok \"null\") " + id + ")")
      }
    }

    it("should understand swank:debug-set-value - array element") {
      testWithResponse("""(swank:debug-set-value (:type element :object-id "23" :index 2) "1")""") { (t, m, id) =>
        (t.debugSetValue _).expects(DebugArrayElement(DebugObjectId(23L), 2), "1").returns(true)
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("should understand swank:debug-backtrace") {
      testWithResponse("""(swank:debug-backtrace "23" 0 2)""") { (t, m, id) =>
        (t.debugBacktrace _).expects(DebugThreadId(23), 0, 2).returns(debugBacktrace)
        (m.send _).expects("(:return (:ok " + debugBacktraceStr + ") " + id + ")")
      }
    }

    it("should understand swank:shutdown-server") {
      testWithResponse("(swank:shutdown-server)") { (t, m, id) =>
        (t.shutdownServer _).expects()
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("should rejected an invalid range expression") {
      testWithResponse("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" (broken range expression))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Invalid rpc request (swank:type-by-name-at-point \\"String\\" \\"SwankProtocol.scala\\" (broken range expression))") $id)""")
      }
    }

    it("should rejected an invalid patch expression") {
      testWithResponse("""(swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("???" 7127 7128)))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Invalid rpc request (swank:patch-source \\"Analyzer.scala\\" ((\\"+\\" 6461 \\"Inc\\") (\\"???\\" 7127 7128)))") $id)""")
      }
    }

    it("should rejected an invalid symbol list expression") {
      testWithResponse("""(swank:symbol-designations "SwankProtocol.scala" 0 46857 (var "fred"))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Invalid rpc request (swank:symbol-designations \\"SwankProtocol.scala\\" 0 46857 (var \\"fred\\"))") $id)""")
      }
    }

    it("should rejected an invalid debug location expression") {
      testWithResponse("""(swank:debug-value (:type unknown :object-id "23" :index 2))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Invalid rpc request (swank:debug-value (:type unknown :object-id \\"23\\" :index 2))") $id)""")
      }
    }

    it("should return error for rpc calls that throw an exception") {
      testWithResponse("(swank:shutdown-server)") { (t, m, id) =>
        (t.shutdownServer _).expects().throws(new RuntimeException("Bang"))
        (m.send _).expects(s"""(:return (:abort 201 \"Bang\") $id)""")
      }
    }

    it("should return error for unknown refactor") {
      testWithResponse("""(swank:prepare-refactor 9 unknownRefactor (foo bar) t)""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Invalid rpc request (swank:prepare-refactor 9 unknownRefactor (foo bar) t)") $id)""")
      }
    }

    it("should handle parse fails gracefully") {
      testWithResponse("(swank:debug-value nil)") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Invalid rpc request (swank:debug-value nil)") $id)""")
      }
    }

  }
}
