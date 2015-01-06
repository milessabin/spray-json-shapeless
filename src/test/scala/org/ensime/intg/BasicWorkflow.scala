package org.ensime.intg

import org.ensime.model._
import org.ensime.core.{ FullTypeCheckCompleteEvent, ClearAllScalaNotesEvent }
import org.ensime.core.{ RefactorResult, RefactorEffect, RenameRefactorDesc }
import org.ensime.util._
import org.scalatest.{ FunSpec, Matchers }
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import TestUtil.SlowTest
import pimpathon.file._

class BasicWorkflow extends FunSpec with Matchers {

  val log = LoggerFactory.getLogger(this.getClass)

  describe("Server") {
    it("should open the test project", SlowTest) {

      IntgUtil.withTestProject("src/example-simple") { (config, project, asyncHelper) =>

        val sourceRoot = config.modules.values.head.sourceRoots.head
        val fooFilePath = (sourceRoot / "org/example/Foo.scala").getCanonicalPath
        val fooFile = (sourceRoot / "org/example/Foo.scala").getCanonicalFile

        // trigger typeCheck
        project.rpcTypecheckFiles(List(SourceFileInfo(fooFile)))

        asyncHelper.expectAsync(30 seconds, ClearAllScalaNotesEvent)
        asyncHelper.expectAsync(30 seconds, FullTypeCheckCompleteEvent)

        //-----------------------------------------------------------------------------------------------
        // semantic highlighting
        val designations = project.rpcSymbolDesignations(fooFilePath, -1, 299, SourceSymbol.allSymbols)
        assert(designations.file == fooFilePath)
        assert(designations.syms.contains(SymbolDesignation(12, 19, PackageSymbol)))
        // expected Symbols
        // ((package 12 19) (package 8 11) (trait 40 43) (valField 69 70) (class 100 103) (param 125 126) (class 128 131) (param 133 134) (class 136 142) (operator 156 157) (param 154 155) (functionCall 160 166) (param 158 159) (valField 183 186) (class 193 199) (class 201 204) (valField 214 217) (class 224 227) (functionCall 232 239) (operator 250 251) (valField 256 257) (valField 252 255) (functionCall 261 268) (functionCall 273 283) (valField 269 272)))

        //-----------------------------------------------------------------------------------------------
        // symbolAtPoint
        val symbolAtPointOpt: Option[SymbolInfo] = project.rpcSymbolAtPoint(fooFilePath, 128)

        val intTypeId = symbolAtPointOpt match {
          case Some(SymbolInfo("scala.Int", "Int", None, BasicTypeInfo("Int", typeId, 'class, "scala.Int", List(), List(), _, None), false, Some(ownerTypeId))) =>
            typeId
          case _ =>
            fail("Symbol at point does not match expectations, expected Int symbol got: " + symbolAtPointOpt)
        }

        val fooClassByNameOpt = project.rpcTypeByName("org.example.Foo")
        val fooClassId = fooClassByNameOpt match {
          case Some(BasicTypeInfo("Foo", fooTypeIdVal, 'class, "org.example.Foo", List(), List(), _, None)) =>
            fooTypeIdVal
          case _ =>
            fail("type by name for Foo class does not match expectations, got: " + fooClassByNameOpt)
        }

        val fooObjectByNameOpt = project.rpcTypeByName("org.example.Foo$")
        val fooObjectId = fooObjectByNameOpt match {
          case Some(BasicTypeInfo("Foo$", fooObjectIdVal, 'object, "org.example.Foo$", List(), List(), Some(OffsetSourcePosition(`fooFile`, 28)), None)) =>
            fooObjectIdVal
          case _ =>
            fail("type by name for Foo object does not match expectations, got: " + fooObjectByNameOpt)
        }

        //-----------------------------------------------------------------------------------------------
        // public symbol search - java.io.File

        val javaSearchSymbol = project.rpcPublicSymbolSearch(List("java", "io", "File"), 2)
        javaSearchSymbol match {
          case SymbolSearchResults(List(
            TypeSearchResult("java.awt.image.ImageObserver", "ImageObserver", 'class,
              Some(_)),
            TypeSearchResult("java.io.File", "File", 'class,
              Some(_)),
            TypeSearchResult("org.ensime.util.FileUtilSpec", "FileUtilSpec", 'class, None))) =>

          case _ =>
            fail("Public symbol search does not match expectations, got: " + javaSearchSymbol)
        }

        //-----------------------------------------------------------------------------------------------
        // public symbol search - scala.util.Random
        val scalaSearchSymbol = project.rpcPublicSymbolSearch(List("scala", "util", "Random"), 2)
        scalaSearchSymbol match {
          case SymbolSearchResults(List(
            TypeSearchResult("scala.util.Random", "Random", 'class, None),
            TypeSearchResult("scala.util.Random$", "Random$", 'class, None))) =>
          case _ =>
            fail("Public symbol search does not match expectations, got: " + scalaSearchSymbol)
        }

        //-----------------------------------------------------------------------------------------------
        // type by id

        val typeByIdOpt: Option[TypeInfo] = project.rpcTypeById(intTypeId)
        val intTypeInspectInfo = typeByIdOpt match {
          case Some(ti @ BasicTypeInfo("Int", `intTypeId`, 'class, "scala.Int", List(), List(), None, None)) =>
            // TODO here pos is None - in inspectType it is Some(EmptySourcePosition()) hack to make flow work
            ti.copy(pos = Some(EmptySourcePosition()))
          case _ =>
            fail("type by id does not match expectations, got " + typeByIdOpt)
        }

        //-----------------------------------------------------------------------------------------------
        // inspect type by id
        val inspectByIdOpt: Option[TypeInspectInfo] = project.rpcInspectTypeById(intTypeId)

        inspectByIdOpt match {
          case Some(TypeInspectInfo(`intTypeInspectInfo`, Some(intCompanionId), supers)) =>
          case _ =>
            fail("inspect by id does not match expectations, got: " + inspectByIdOpt)
        }

        //-----------------------------------------------------------------------------------------------
        // uses of symbol at point

        log.info("------------------------------------222-")

        val useOfSymbolAtPoint: List[ERangePosition] = project.rpcUsesOfSymAtPoint(fooFilePath, 121)
        useOfSymbolAtPoint match {
          case List(ERangePosition(`fooFilePath`, 114, 110, 172), ERangePosition(`fooFilePath`, 273, 269, 283)) =>
          case _ =>
            fail("rpcUsesOfSymAtPoint not match expectations, got: " + useOfSymbolAtPoint)
        }

        log.info("------------------------------------222-")

        // note that the line numbers appear to have been stripped from the
        // scala library classfiles, so offset/line comes out as zero unless
        // loaded by the pres compiler
        val testMethodSymbolInfo = project.rpcSymbolAtPoint(fooFilePath, 276)
        testMethodSymbolInfo match {
          case Some(SymbolInfo("testMethod", "testMethod", Some(OffsetSourcePosition(`fooFile`, 114)), ArrowTypeInfo("(i: Int, s: String)Int", 126, BasicTypeInfo("Int", 1, 'class, "scala.Int", List(), List(), None, None), List(ParamSectionInfo(List((i, BasicTypeInfo("Int", 1, 'class, "scala.Int", List(), List(), None, None)), (s, BasicTypeInfo("String", 39, 'class, "java.lang.String", List(), List(), None, None))), false))), true, Some(_))) =>
          case _ =>
            fail("symbol at point (local test method), got: " + testMethodSymbolInfo)
        }

        // M-.  external symbol
        val genericMethodSymbolAtPointRes = project.rpcSymbolAtPoint(fooFilePath, 190)
        genericMethodSymbolAtPointRes match {

          case Some(SymbolInfo("apply", "apply", None,
            ArrowTypeInfo("[A, B](elems: (A, B)*)CC[A,B]", _,
              BasicTypeInfo("CC", _, 'nil, "scala.collection.generic.CC",
                List(
                  BasicTypeInfo("A", _, 'nil, "scala.collection.generic.A", List(), List(), None, None),
                  BasicTypeInfo("B", _, 'nil, "scala.collection.generic.B", List(), List(), None, None)
                  ), List(), None, None),
              List(ParamSectionInfo(List(
                ("elems", BasicTypeInfo("<repeated>", _, 'class, "scala.<repeated>", List(
                  BasicTypeInfo("Tuple2", _, 'class, "scala.Tuple2", List(
                    BasicTypeInfo("A", _, 'nil, "scala.collection.generic.A", List(), List(), None, None),
                    BasicTypeInfo("B", _, 'nil, "scala.collection.generic.B", List(), List(), None, None)
                    ), List(), None, None)), List(), None, None))), false))), true, Some(_))) =>
          case _ =>
            fail("symbol at point (local test method), got: " + genericMethodSymbolAtPointRes)
        }

        // C-c C-v p Inspect source of current package
        val insPacByPathResOpt = project.rpcInspectPackageByPath("org.example")
        insPacByPathResOpt match {
          case Some(PackageInfo("example", "org.example", List(BasicTypeInfo("Foo", `fooClassId`, 'class, "org.example.Foo", List(), List(), Some(EmptySourcePosition()), None), BasicTypeInfo("Foo$", `fooObjectId`, 'object, "org.example.Foo$", List(), List(), Some(EmptySourcePosition()), None)))) =>
          case _ =>
            fail("inspect package by path failed, got: " + insPacByPathResOpt)
        }

        // expand selection around 'val foo'
        val expandRange1: FileRange = project.rpcExpandSelection(fooFilePath, 215, 215)
        assert(expandRange1 == FileRange(fooFilePath, 214, 217))

        val expandRange2: FileRange = project.rpcExpandSelection(fooFilePath, 214, 217)
        assert(expandRange2 == FileRange(fooFilePath, 210, 229))

        // TODO get the before content of the file

        // rename var
        val prepareRefactorRes = project.rpcPrepareRefactor(1234, RenameRefactorDesc("bar", fooFilePath, 215, 215))
        log.info("PREPARE REFACTOR = " + prepareRefactorRes)
        prepareRefactorRes match {
          case Right(RefactorEffect(1234, 'rename, List(
            TextEdit(`fooFile`, 214, 217, "bar"),
            TextEdit(`fooFile`, 252, 255, "bar"),
            TextEdit(`fooFile`, 269, 272, "bar")))) =>
          case _ =>
            fail("Prepare refactor result does not match, got: " + prepareRefactorRes)
        }

        val execRefactorRes = project.rpcExecRefactor(1234, Symbols.Rename)
        execRefactorRes match {
          case Right(RefactorResult(1234, 'rename, List(`fooFile`))) =>
          case _ =>
            fail("exec refactor does not match expectation: " + execRefactorRes)
        }

        // TODO Check the after refactoring file is different

        val peekUndoRes = project.rpcPeekUndo()
        val undoId = peekUndoRes match {
          case Right(Undo(undoIdVal, "Refactoring of type: 'rename", List(TextEdit(`fooFile`, 214, 217, "foo"), TextEdit(`fooFile`, 252, 255, "foo"), TextEdit(`fooFile`, 269, 272, "foo")))) =>
            undoIdVal
          case _ =>
            fail("unexpected peek undo result: " + peekUndoRes)

        }

        val execUndoRes = project.rpcExecUndo(undoId)
        execUndoRes match {
          case Right(UndoResult(1, List(`fooFile`))) =>
          case _ =>
            fail("unexpected exec undo result: " + execUndoRes)
        }

        // TODO Check the file has reverted to original

        val packageMemberCompRes = project.rpcPackageMemberCompletion("scala.collection.mutable", "Ma")
        packageMemberCompRes match {
          case List(
            CompletionInfo("Map", CompletionSignature(List(), ""), -1, false, 50, None),
            CompletionInfo("Map$", CompletionSignature(List(), ""), -1, false, 50, None),
            CompletionInfo("MapBuilder", CompletionSignature(List(), ""), -1, false, 50, None),
            CompletionInfo("MapBuilder$", CompletionSignature(List(), ""), -1, false, 50, None),
            CompletionInfo("MapLike", CompletionSignature(List(), ""), -1, false, 50, None),
            CompletionInfo("MapLike$", CompletionSignature(List(), ""), -1, false, 50, None),
            CompletionInfo("MapProxy", CompletionSignature(List(), ""), -1, false, 50, None),
            CompletionInfo("MapProxy$", CompletionSignature(List(), ""), -1, false, 50, None)) =>
          case _ =>
            fail("package name completion result: " + packageMemberCompRes)
        }
      }
    }
  }
}
