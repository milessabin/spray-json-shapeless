package org.ensime.server.protocol.swank

import java.io.{ InputStreamReader, ByteArrayInputStream, ByteArrayOutputStream }
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import java.util.concurrent.atomic.AtomicInteger

import org.ensime.EnsimeApi
import org.ensime.core._
import org.ensime.model._
import org.ensime.server._
import org.ensime.util._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{ BeforeAndAfterAll, FunSpec, ShouldMatchers }
import org.slf4j.{ LoggerFactory, Logger }

import scala.reflect.io.ZipArchive
import scala.tools.nsc.io._

class SwankProtocolSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll with MockFactory {

  import SwankTestData._

  class MockZipEntry(entry: String, archive: ZipArchive) extends VirtualFile(entry, entry) {
    override def underlyingSource: Option[ZipArchive] = Some(archive)
  }

  describe("SwankProtocol") {
    it("can encode and decode sexp to wire") {
      TestUtil.withActorSystem { actorSystem =>
        val msg = SExpParser.read("""(:XXXX "abc") """)
        val protocol = new SwankWireFormatCodec {
          override val log: Logger = LoggerFactory.getLogger(this.getClass)
        }

        val os = new ByteArrayOutputStream()
        protocol.writeMessage(msg, os)

        val reader = new InputStreamReader(new ByteArrayInputStream(os.toByteArray))
        val incomingMsg = protocol.readMessage(reader).asInstanceOf[SExp]

        assert(msg == incomingMsg)
      }
    }

    trait MsgHandler {
      def send(s: String)
    }

    val nextId = new AtomicInteger(1)
    def testWithResponse(msg: String)(expectation: (EnsimeApi, MsgHandler, Int) => Unit): Unit = {
      TestUtil.withActorSystem { actorSystem =>
        val t = mock[EnsimeApi]
        val out = mock[MsgHandler]

        val latch = new CountDownLatch(1)

        val prot = new SwankProtocol(actorSystem, null, t) {
          override def sendMessage(o: WireFormat): Unit = {
            out.send(o.toString)
            latch.countDown()
          }
        }

        val rpcId = nextId.getAndIncrement
        expectation(t, out, rpcId)

        val sexp = SExpParser.read("(:swank-rpc " + msg + " " + rpcId + ")")
        assert(sexp != NilAtom)

        prot.handleIncomingMessage(sexp)

        if (!latch.await(1000, TimeUnit.MILLISECONDS))
          fail("Waited too long for expectation")
      }
    }

    it("should understand swank:peek-undo - success") {
      val file3 = CanonFile("/foo/abc")
      val file3_str = TestUtil.fileToWireString(file3)

      testWithResponse("""(swank:peek-undo)""") { (t, m, id) =>
        (t.rpcPeekUndo _).expects().returns(Right(Undo(3, "Undoing stuff", List(TextEdit(file3, 5, 7, "aaa")))))
        (m.send _).expects("""(:return (:ok (:id 3 :changes ((:file """ + file3_str + """ :text "aaa" :from 5 :to 7)) :summary "Undoing stuff")) """ + id + ")")
      }
    }

    it("should understand swank:peek-undo - fail") {
      testWithResponse("""(swank:peek-undo)""") { (t, m, id) =>
        (t.rpcPeekUndo _).expects().returns(Left("ErrorError"))
        (m.send _).expects(s"""(:return (:abort 206 "ErrorError") $id)""")
      }
    }

    it("should understand swank:exec-undo") {
      testWithResponse("""(swank:exec-undo 1)""") { (t, m, id) =>
        (t.rpcExecUndo _).expects(1).returns(Right(undoResult))
        (m.send _).expects(s"""(:return (:ok $undoResultStr) $id)""")

      }
    }

    it("should understand connection-info request") {
      testWithResponse("(swank:connection-info)") { (t, m, id) =>
        (t.rpcConnectionInfo _).expects().returns(new ConnectionInfo())
        (m.send _).expects(s"""(:return (:ok (:pid nil :implementation (:name "ENSIME-ReferenceServer") :version "0.8.10")) $id)""")
      }
    }

    //    it("should understand swank:init-project") {
    //      val configFragment = """(:use-sbt t :compiler-args ("-Ywarn-dead-code" "-Ywarn-catches" "-Xstrict-warnings") :root-dir "/Users/aemon/projects/ensime/")"""
    //      testWithResponse(s"""(swank:init-project $configFragment)""") { (t, id) =>
    //        (t.rcpInitProject _).expects(SExpParser.read(configFragment)).returns(EnsimeConfig(...))
    //        (m.send _).expects(s"""(:return (:ok $ensimeConfigStr) $id)""")
    //      }
    //    }

    it("should understand swank:repl-config") {
      testWithResponse("(swank:repl-config)") { (t, m, id) =>
        (t.rpcReplConfig _).expects().returns(replConfig)
        (m.send _).expects(s"""(:return (:ok $replConfigStr) $id)""")
      }
    }

    it("should understand swank:remove-file") {
      testWithResponse("""(swank:remove-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcRemoveFile _).expects("Analyzer.scala")
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:swank:typecheck-file") {
      testWithResponse("""(swank:typecheck-file "Analyzer.scala")""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile)))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:swank:typecheck-file with content") {
      testWithResponse("""(swank:typecheck-file "Analyzer.scala" "contents\\n\\ncontents")""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile, Some("contents\\n\\ncontents"))))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:typecheck-files") {
      testWithResponse("""(swank:typecheck-files ("Analyzer.scala" "Foo.scala"))""") { (t, m, id) =>
        (t.rpcTypecheckFiles _).expects(List(SourceFileInfo(analyzerFile), SourceFileInfo(fooFile)))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:atch-source") {
      testWithResponse("""(swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("-" 7127 7128) ("*" 7200 7300 "Bob")))""") { (t, m, id) =>
        (t.rpcPatchSource _).expects("Analyzer.scala", List(PatchInsert(6461, "Inc"), PatchDelete(7127, 7128), PatchReplace(7200, 7300, "Bob")))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:typecheck-all") {
      testWithResponse("""(swank:typecheck-all)""") { (t, m, id) =>
        (t.rpcTypecheckAll _).expects()
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:unload-all") {
      testWithResponse("""(swank:unload-all)""") { (t, m, id) =>
        (t.rpcUnloadAll _).expects()
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:format-source") {
      testWithResponse("""(swank:format-source ("/ensime/src/Test.scala"))""") { (t, m, id) =>
        (t.rpcFormatFiles _).expects(List("/ensime/src/Test.scala"))
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    // TODO add test for exception handling in above

    it("should understand swank:public-symbol-search") {
      testWithResponse("""(swank:public-symbol-search ("java" "io" "File") 50)""") { (t, m, id) =>
        (t.rpcPublicSymbolSearch _).expects(List("java", "io", "File"), 50).returns(symbolSearchResults)
        (m.send _).expects(s"""(:return (:ok $symbolSearchResultsStr) $id)""")
      }
    }

    it("should understand swank:import-suggestions") {
      testWithResponse("""(swank:import-suggestions "/ensime/src/main/scala/org/ensime/server/Analyzer.scala" 2300 ("Actor") 10)""") { (t, m, id) =>
        (t.rpcImportSuggestions _).expects("/ensime/src/main/scala/org/ensime/server/Analyzer.scala", 2300,
          List("Actor"), 10).returns(importSuggestions)
        (m.send _).expects(s"""(:return (:ok $importSuggestionsStr) $id)""")
      }
    }

    it("should understand swank:completions") {
      testWithResponse(
        """(swank:completions
              | "/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala"
              | 22626 0 t t)""".stripMargin) { (t, m, id) =>
          (t.rpcCompletionsAtPoint _).expects(
            "/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala", 22626, 0, true, true).returns(completionInfoCList)
          (m.send _).expects(s"""(:return (:ok $completionInfoCListStr) $id)""")
        }
    }

    it("should understand swank:package-member-completion") {
      testWithResponse("""(swank:package-member-completion "org.ensime.server" "Server")""") { (t, m, id) =>
        (t.rpcPackageMemberCompletion _).expects("org.ensime.server", "Server").returns(completionInfoList)
        (m.send _).expects(s"""(:return (:ok $completionInfoListStr) $id)""")
      }
    }

    it("should understand swank:call-completion") {
      testWithResponse("""(swank:call-completion 1)""") { (t, m, id) =>
        (t.rpcCallCompletion _).expects(1).returns(Some(callCompletionInfo))
        (m.send _).expects(s"""(:return (:ok $callCompletionInfoStr) $id)""")
      }
    }

    it("should understand swank:uses-of-symbol-at-point") {
      testWithResponse("""(swank:uses-of-symbol-at-point "Test.scala" 11334)""") { (t, m, id) =>
        (t.rpcUsesOfSymAtPoint _).expects("Test.scala", 11334).returns(List(rangePos1, rangePos2))
        (m.send _).expects(s"""(:return (:ok ($rangePos1Str $rangePos2Str)) $id)""")
      }
    }

    it("should understand swank:member-by-name") {
      testWithResponse("""(swank:member-by-name "org.example.A" "x" t)""") { (t, m, id) =>
        (t.rpcMemberByName _).expects("org.example.A", "x", true).returns(Some(symbolInfo))
        (m.send _).expects(s"""(:return (:ok $symbolInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-id") {
      testWithResponse("""(swank:type-by-id 7)""") { (t, m, id) =>
        (t.rpcTypeById _).expects(7).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-name") {
      testWithResponse("""(swank:type-by-name "java.lang.String")""") { (t, m, id) =>
        (t.rpcTypeByName _).expects("java.lang.String").returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-name-at-point at point") {
      testWithResponse("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.rpcTypeByNameAtPoint _).expects("String", "SwankProtocol.scala", OffsetRange(31680)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-by-name-at-point at range") {
      testWithResponse("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.rpcTypeByNameAtPoint _).expects("String", "SwankProtocol.scala", OffsetRange(31680, 31691)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-at-point at point") {
      testWithResponse("""(swank:type-at-point "SwankProtocol.scala" 31680)""") { (t, m, id) =>
        (t.rpcTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(31680)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:type-at-point at range") {
      testWithResponse("""(swank:type-at-point "SwankProtocol.scala" (31680 31691))""") { (t, m, id) =>
        (t.rpcTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(31680, 31691)).returns(Some(typeInfo))
        (m.send _).expects(s"""(:return (:ok $typeInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-at-point at point") {
      testWithResponse("""(swank:inspect-type-at-point "SwankProtocol.scala" 32736)""") { (t, m, id) =>
        (t.rpcInspectTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(32736)).returns(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-at-point at range") {
      testWithResponse("""(swank:inspect-type-at-point "SwankProtocol.scala" (32736 32740))""") { (t, m, id) =>
        (t.rpcInspectTypeAtPoint _).expects("SwankProtocol.scala", OffsetRange(32736, 32740)).returns(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-by-id") {
      testWithResponse("""(swank:inspect-type-by-id 232)""") { (t, m, id) =>
        (t.rpcInspectTypeById _).expects(232).returning(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-type-by-name") {
      testWithResponse("""(swank:inspect-type-by-name "abc.d")""") { (t, m, id) =>
        (t.rpcInspectTypeByName _).expects("abc.d").returning(Some(typeInspectInfo))
        (m.send _).expects(s"""(:return (:ok $typeInspectInfoStr) $id)""")
      }
    }

    it("should understand symbol-at-point") {
      testWithResponse("""(swank:symbol-at-point "SwankProtocol.scala" 36483)""") { (t, m, id) =>
        (t.rpcSymbolAtPoint _).expects("SwankProtocol.scala", 36483).returns(Some(symbolInfo))
        (m.send _).expects(s"""(:return (:ok $symbolInfoStr) $id)""")
      }
    }

    it("should understand swank:inspect-package-by-path") {
      testWithResponse("""(swank:inspect-package-by-path "org.ensime.util")""") { (t, m, id) =>
        (t.rpcInspectPackageByPath _).expects("org.ensime.util").returns(Some(packageInfo))
        (m.send _).expects(s"""(:return (:ok $packageInfoStr) $id)""")
      }
    }

    it("should understand swank:prepare-refactor - inline local - prepare failure") {
      testWithResponse("""(swank:prepare-refactor 7 inlineLocal (file "SwankProtocol.scala" start 100 end 200) t)""") { (t, m, id) =>

        //        Either[RefactorFailure, RefactorEffect]
        (t.rpcPrepareRefactor _).expects(7, InlineLocalRefactorDesc("SwankProtocol.scala", 100, 200)).returns(
          Left(refactorFailure))
        (m.send _).expects(s"""(:return (:ok $refactorFailureStr) $id)""")
      }
    }
    it("should understand swank:prepare-refactor - inline local - prepare - interactive") {
      testWithResponse("""(swank:prepare-refactor 7 inlineLocal (file "SwankProtocol.scala" start 100 end 200) t)""") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(7, InlineLocalRefactorDesc("SwankProtocol.scala", 100, 200)).returns(
          Right(refactorRenameEffect))
        (m.send _).expects(s"""(:return (:ok $refactorRenameEffectStr) $id)""")
      }
    }

    it("should understand swank:prepare-refactor - inline local - non-interactive - failure") {
      testWithResponse("""(swank:prepare-refactor 7 inlineLocal (file "SwankProtocol.scala" start 100 end 200) nil)""") { (t, m, id) =>

        (t.rpcPrepareRefactor _).expects(7, InlineLocalRefactorDesc("SwankProtocol.scala", 100, 200)).returns(Right(refactorRenameEffect))
        (t.rpcExecRefactor _).expects(7, Symbols.InlineLocal).returns(Left(refactorFailure))

        (m.send _).expects(s"""(:return (:ok $refactorFailureStr) $id)""")
      }
    }

    def testRefactorMethod(msg: String, desc: RefactorDesc): Unit = {

      val refactorEffect = new RefactorEffect(7, desc.refactorType, List(TextEdit(file3, 5, 7, "aaa")))
      val refactorResult = new RefactorResult(7, desc.refactorType, List(file3, file1))
      val refactorResultStr = """(:procedure-id 7 :refactor-type """ + desc.refactorType.name + """ :status success :touched-files (""" + file3_str + " " + file1_str + """))"""

      testWithResponse("""(swank:prepare-refactor 7""" + msg + " nil)") { (t, m, id) =>
        (t.rpcPrepareRefactor _).expects(7, desc).returns(Right(refactorEffect))
        (t.rpcExecRefactor _).expects(7, desc.refactorType).returns(Right(refactorResult))
        (m.send _).expects(s"""(:return (:ok $refactorResultStr) $id)""")
      }
    }

    it("should understand swank:prepare-refactor - inline local - non-interactive") {
      testRefactorMethod("""inlineLocal (file "SwankProtocol.scala" start 100 end 200)""",
        InlineLocalRefactorDesc("SwankProtocol.scala", 100, 200))
    }

    it("should understand swank:prepare-refactor - rename") {
      testRefactorMethod("""rename (file "SwankProtocol.scala" start 39504 end 39508 newName "dude")""",
        RenameRefactorDesc("dude", "SwankProtocol.scala", 39504, 39508))
    }

    it("should understand swank:prepare-refactor - extractMethod") {
      testRefactorMethod("""extractMethod (methodName "foo" file "SwankProtocol.scala" start 39504 end 39508)""",
        ExtractMethodRefactorDesc("foo", "SwankProtocol.scala", 39504, 39508))
    }

    it("should understand swank:prepare-refactor - extractLocal") {
      testRefactorMethod("""extractLocal (name "foo" file "SwankProtocol.scala" start 39504 end 39508)""",
        ExtractLocalRefactorDesc("foo", "SwankProtocol.scala", 39504, 39508))
    }

    it("should understand swank:prepare-refactor - organiseImports") {
      testRefactorMethod("""organiseImports (file "SwankProtocol.scala")""",
        OrganiseImportsRefactorDesc("SwankProtocol.scala"))
    }

    it("should not fail on swank:prepareRefactor from bug 573") {
      testRefactorMethod("""organizeImports (file "/home/fommil/Projects/ensime-server/src/main/scala/org/ensime/indexer/DatabaseService.scala")""",
        OrganiseImportsRefactorDesc("/home/fommil/Projects/ensime-server/src/main/scala/org/ensime/indexer/DatabaseService.scala"))
    }

    it("should understand swank:prepare-refactor - addImport") {
      testRefactorMethod("""addImport (qualifiedName "com.bar.Foo" file "SwankProtocol.scala" start 39504 end 39508)""",
        AddImportRefactorDesc("com.bar.Foo", "SwankProtocol.scala"))

      testRefactorMethod("""addImport (qualifiedName "com.bar.Foo" file "SwankProtocol.scala")""",
        AddImportRefactorDesc("com.bar.Foo", "SwankProtocol.scala"))

    }

    it("should understand swank:exec-refactor - refactor failure") {
      testWithResponse("""(swank:exec-refactor 7 rename)""") { (t, m, id) =>
        (t.rpcExecRefactor _).expects(7, Symbol("rename")).returns(Left(refactorFailure))
        (m.send _).expects(s"""(:return (:ok $refactorFailureStr) $id)""")
      }
    }

    it("should understand swank:exec-refactor - refactor result") {
      testWithResponse("""(swank:exec-refactor 7 rename)""") { (t, m, id) =>
        (t.rpcExecRefactor _).expects(7, Symbol("rename")).returns(Right(refactorResult))
        (m.send _).expects(s"""(:return (:ok $refactorResultStr) $id)""")
      }
    }

    it("should understand swank:cancel-refactor") {
      testWithResponse("""(swank:cancel-refactor 1)""") { (t, m, id) =>
        (t.rpcCancelRefactor _).expects(1)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:symbol-designations") {
      testWithResponse("""(swank:symbol-designations "SwankProtocol.scala" 0 46857 (var val varField valField))""") {
        (t, m, id) =>
          (t.rpcSymbolDesignations _).expects("SwankProtocol.scala", 0, 46857,
            Set[SourceSymbol](VarSymbol, ValSymbol, VarFieldSymbol, ValFieldSymbol)).returns(symbolDesignations)
          (m.send _).expects(s"""(:return (:ok $symbolDesignationsStr) $id)""")
      }
    }

    it("should understand swank:expand-selection") {
      testWithResponse("""(swank:expand-selection "Model.scala" 4648 4721)""") { (t, m, id) =>
        (t.rpcExpandSelection _).expects("Model.scala", 4648, 4721).returns(fileRange)
        (m.send _).expects(s"""(:return (:ok $fileRangeStr) $id)""")
      }
    }

    it("should understand swank:debug-active-vm - good") {
      testWithResponse("""(swank:debug-active-vm)""") { (t, m, id) =>
        (t.rpcDebugActiveVM _).expects().returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-active-vm - bad") {
      testWithResponse("""(swank:debug-active-vm)""") { (t, m, id) =>
        (t.rpcDebugActiveVM _).expects().returns(false)
        (m.send _).expects(s"""(:return (:ok nil) $id)""")
      }
    }

    it("should understand swank:debug-start - success") {
      testWithResponse("""(swank:debug-start "org.hello.HelloWorld arg")""") { (t, m, id) =>
        (t.rpcDebugStartVM _).expects("org.hello.HelloWorld arg").returns(DebugVmSuccess)
        (m.send _).expects(s"""(:return (:ok (:status "success")) $id)""")
      }
    }

    it("should understand swank:debug-start - failure") {
      testWithResponse("""(swank:debug-start "org.hello.HelloWorld arg")""") { (t, m, id) =>
        (t.rpcDebugStartVM _).expects("org.hello.HelloWorld arg").returns(DebugVmError(303, "xxxx"))
        (m.send _).expects(s"""(:return (:ok (:status "error" :error-code 303 :details "xxxx")) $id)""")
      }
    }

    it("should understand swank:debug-attach - success") {
      testWithResponse("""(swank:debug-attach "localhost" "9000")""") { (t, m, id) =>
        (t.rpcDebugAttachVM _).expects("localhost", "9000").returns(DebugVmSuccess)
        (m.send _).expects(s"""(:return (:ok (:status "success")) $id)""")
      }
    }

    it("should understand swank:debug-attach - failure") {
      testWithResponse("""(swank:debug-attach "localhost" "9000")""") { (t, m, id) =>
        (t.rpcDebugAttachVM _).expects("localhost", "9000").returns(DebugVmError(303, "xxxx"))
        (m.send _).expects(s"""(:return (:ok (:status "error" :error-code 303 :details "xxxx")) $id)""")
      }
    }

    it("should understand swank:debug-stop - good") {
      testWithResponse("""(swank:debug-stop)""") { (t, m, id) =>
        (t.rpcDebugStopVM _).expects().returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-stop - bad") {
      testWithResponse("""(swank:debug-stop)""") { (t, m, id) =>
        (t.rpcDebugStopVM _).expects().returns(false)
        (m.send _).expects(s"""(:return (:ok nil) $id)""")
      }
    }

    it("should understand swank:debug-set-break") {
      testWithResponse("""(swank:debug-set-break "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcDebugSetBreakpoint _).expects("hello.scala", 12)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-clear-break") {
      testWithResponse("""(swank:debug-clear-break "hello.scala" 12)""") { (t, m, id) =>
        (t.rpcDebugClearBreakpoint _).expects("hello.scala", 12)
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("should understand swank:debug-clear-all-breaks") {
      testWithResponse("""(swank:debug-clear-all-breaks)""") { (t, m, id) =>
        (t.rpcDebugClearAllBreakpoints _).expects()
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("debug-list-breakpoints test") {
      testWithResponse("""(swank:debug-list-breakpoints)""") { (t, m, id) =>
        (t.rpcDebugListBreakpoints _).expects().returns(breakpointList)
        (m.send _).expects(s"""(:return (:ok $breakpointListStr) $id)""")
      }
    }

    it("should understand swank:debug-run") {
      testWithResponse("""(swank:debug-run)""") { (t, m, id) =>
        (t.rpcDebugRun _).expects().returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-continue") {
      testWithResponse("""(swank:debug-continue "1")""") { (t, m, id) =>
        (t.rpcDebugContinue _).expects(1L).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-step") {
      testWithResponse("""(swank:debug-step "982398123")""") { (t, m, id) =>
        (t.rpcDebugStep _).expects(982398123L).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-next") {
      testWithResponse("""(swank:debug-next "982398123")""") { (t, m, id) =>
        (t.rpcDebugNext _).expects(982398123L).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-step-out") {
      testWithResponse("""(swank:debug-step-out "982398123")""") { (t, m, id) =>
        (t.rpcDebugStepOut _).expects(982398123L).returns(true)
        (m.send _).expects(s"""(:return (:ok t) $id)""")
      }
    }

    it("should understand swank:debug-locate-name") {
      testWithResponse("""(swank:debug-locate-name "7" "apple")""") { (t, m, id) =>
        (t.rpcDebugLocateName _).expects(7L, "apple").returns(Some(debugLocObjectRef))
        (m.send _).expects("(:return (:ok " + debugLocObjectRefStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - array") {
      testWithResponse("""(swank:debug-value (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugArrayElement(23L, 2)).returns(Some(debugNullValue))
        (m.send _).expects("(:return (:ok " + debugNullValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - object") {
      testWithResponse("""(swank:debug-value (:type reference :object-id "23"))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugObjectReference(23L)).returns(Some(debugArrayInstValue))
        (m.send _).expects("(:return (:ok " + debugArrayInstValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - object field") {
      testWithResponse("""(swank:debug-value (:type field :object-id "23" :field "fred"))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugObjectField(23L, "fred")).returns(Some(debugPrimitiveValue))
        (m.send _).expects("(:return (:ok " + debugPrimitiveValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-value - stack slot") {
      testWithResponse("""(swank:debug-value (:type slot :thread-id "23" :frame 7 :offset 25))""") { (t, m, id) =>
        (t.rpcDebugValue _).expects(DebugStackSlot(23L, 7, 25)).returns(Some(debugStringValue))
        (m.send _).expects("(:return (:ok " + debugStringValueStr + ") " + id + ")")
      }
    }

    it("should understand swank:debug-to-string - array element") {
      testWithResponse("""(swank:debug-to-string "2" (:type element :object-id "23" :index 2))""") { (t, m, id) =>
        (t.rpcDebugToString _).expects(2L, DebugArrayElement(23L, 2)).returns(Some("null"))
        (m.send _).expects("(:return (:ok \"null\") " + id + ")")
      }
    }

    it("should understand swank:debug-set-value - array element") {
      testWithResponse("""(swank:debug-set-value (:type element :object-id "23" :index 2) "1")""") { (t, m, id) =>
        (t.rpcDebugSetValue _).expects(DebugArrayElement(23L, 2), "1").returns(true)
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("should understand swank:debug-backtrace") {
      testWithResponse("""(swank:debug-backtrace "23" 0 2)""") { (t, m, id) =>
        (t.rpcDebugBacktrace _).expects(23L, 0, 2).returns(debugBacktrace)
        (m.send _).expects("(:return (:ok " + debugBacktraceStr + ") " + id + ")")
      }
    }

    it("should understand swank:shutdown-server") {
      testWithResponse("(swank:shutdown-server)") { (t, m, id) =>
        (t.rpcShutdownServer _).expects()
        (m.send _).expects("(:return (:ok t) " + id + ")")
      }
    }

    it("should rejected an invalid range expression") {
      testWithResponse("""(swank:type-by-name-at-point "String" "SwankProtocol.scala" (broken range expression))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:type-by-name-at-point call: (swank:type-by-name-at-point \\"String\\" \\"SwankProtocol.scala\\" (broken range expression))") $id)""")
      }
    }

    it("should rejected an invalid patch expression") {
      testWithResponse("""(swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("???" 7127 7128)))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:patch-source call: (swank:patch-source \\"Analyzer.scala\\" ((\\"+\\" 6461 \\"Inc\\") (\\"???\\" 7127 7128)))") $id)""")
      }
    }

    it("should rejected an invalid symbol list expression") {
      testWithResponse("""(swank:symbol-designations "SwankProtocol.scala" 0 46857 (var "fred"))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:symbol-designations call: (swank:symbol-designations \\"SwankProtocol.scala\\" 0 46857 (var \\"fred\\"))") $id)""")
      }
    }

    it("should rejected an invalid debug location expression") {
      testWithResponse("""(swank:debug-value (:type unknown :object-id "23" :index 2))""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed swank:debug-value call: (swank:debug-value (:type unknown :object-id \\"23\\" :index 2))") $id)""")
      }
    }

    it("should return error for rpc calls that throw an exception") {
      testWithResponse("(swank:shutdown-server)") { (t, m, id) =>
        (t.rpcShutdownServer _).expects().throws(new RuntimeException("Bang"))
        (m.send _).expects(s"""(:return (:abort 201 \"Bang\") $id)""")
      }
    }

    it("should return error for unknown refactor") {
      testWithResponse("""(swank:prepare-refactor 9 unknownRefactor (file "SwankProtocol.scala") t)""") { (t, m, id) =>
        (m.send _).expects(s"""(:return (:abort 202 "Malformed prepare-refactor - Incorrect arguments or unknown refactor type: unknownRefactor: (swank:prepare-refactor 9 unknownRefactor (file \\"SwankProtocol.scala\\") t)") $id)""")
      }
    }
  }
}
