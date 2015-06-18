package org.ensime.intg

import akka.event.slf4j.SLF4JLogging
import org.ensime.model._
import org.ensime.api._
import org.ensime.util._
import org.scalatest.WordSpec
import org.scalatest.Matchers

import scala.concurrent.duration._
import pimpathon.file._

import org.ensime.fixture._

class BasicWorkflow extends WordSpec with Matchers
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.SimpleTestProject

  "Server" should {
    "open the test project" in {
      withEnsimeConfig { implicit config =>
        withTestKit { implicit testkit =>
          withProject { (project, asyncHelper) =>
            import testkit._

            val sourceRoot = scalaMain(config)
            val fooFile = sourceRoot / "org/example/Foo.scala"
            val fooFilePath = fooFile.getAbsolutePath

            // trigger typeCheck
            project ! TypecheckFilesReq(List(fooFile))
            expectMsg(VoidResponse)

            asyncHelper.receiveN(2) should contain only (
              ClearAllScalaNotesEvent,
              FullTypeCheckCompleteEvent
            )

            //-----------------------------------------------------------------------------------------------
            // semantic highlighting
            project ! SymbolDesignationsReq(fooFile, -1, 299, SourceSymbol.allSymbols)
            val designations = expectMsgType[SymbolDesignations]
            designations.file shouldBe fooFile
            designations.syms should contain(SymbolDesignation(12, 19, PackageSymbol))
            // expected Symbols
            // ((package 12 19) (package 8 11) (trait 40 43) (valField 69 70) (class 100 103) (param 125 126) (class 128 131) (param 133 134) (class 136 142) (operator 156 157) (param 154 155) (functionCall 160 166) (param 158 159) (valField 183 186) (class 193 199) (class 201 204) (valField 214 217) (class 224 227) (functionCall 232 239) (operator 250 251) (valField 256 257) (valField 252 255) (functionCall 261 268) (functionCall 273 283) (valField 269 272)))

            //-----------------------------------------------------------------------------------------------
            // symbolAtPoint
            project ! SymbolAtPointReq(fooFile, 128)
            val symbolAtPointOpt: Option[SymbolInfo] = expectMsgType[Option[SymbolInfo]]

            val intTypeId = symbolAtPointOpt match {
              case Some(SymbolInfo("scala.Int", "Int", Some(_), BasicTypeInfo("Int", typeId, DeclaredAs.Class, "scala.Int", List(), List(), _, None), false, Some(ownerTypeId))) =>
                typeId
              case _ =>
                fail("Symbol at point does not match expectations, expected Int symbol got: " + symbolAtPointOpt)
            }

            project ! TypeByNameReq("org.example.Foo")
            val fooClassByNameOpt = expectMsgType[Option[TypeInfo]]
            val fooClassId = fooClassByNameOpt match {
              case Some(BasicTypeInfo("Foo", fooTypeIdVal, DeclaredAs.Class, "org.example.Foo", List(), List(), _, None)) =>
                fooTypeIdVal
              case _ =>
                fail("type by name for Foo class does not match expectations, got: " + fooClassByNameOpt)
            }

            project ! TypeByNameReq("org.example.Foo$")
            val fooObjectByNameOpt = expectMsgType[Option[TypeInfo]]
            val fooObjectId = fooObjectByNameOpt match {
              case Some(BasicTypeInfo("Foo$", fooObjectIdVal, DeclaredAs.Object, "org.example.Foo$", List(), List(), Some(OffsetSourcePosition(`fooFile`, 28)), None)) =>
                fooObjectIdVal
              case _ =>
                fail("type by name for Foo object does not match expectations, got: " + fooObjectByNameOpt)
            }

            //-----------------------------------------------------------------------------------------------
            // public symbol search - java.io.File

            project ! PublicSymbolSearchReq(List("java", "io", "File"), 30)
            val javaSearchSymbol = expectMsgType[SymbolSearchResults]
            assert(javaSearchSymbol.syms.exists {
              case TypeSearchResult("java.io.File", "File", DeclaredAs.Class, Some(_)) => true
              case _ => false
            })

            //-----------------------------------------------------------------------------------------------
            // public symbol search - scala.util.Random
            project ! PublicSymbolSearchReq(List("scala", "util", "Random"), 2)
            val scalaSearchSymbol = expectMsgType[SymbolSearchResults]
            scalaSearchSymbol match {
              case SymbolSearchResults(List(
                TypeSearchResult("scala.util.Random", "Random", DeclaredAs.Class, Some(_)),
                TypeSearchResult("scala.util.Random$", "Random$", DeclaredAs.Class, Some(_)))) =>
              case _ =>
                fail("Public symbol search does not match expectations, got: " + scalaSearchSymbol)
            }

            //-----------------------------------------------------------------------------------------------
            // type by id

            project ! TypeByIdReq(intTypeId)
            val typeByIdOpt = expectMsgType[Option[TypeInfo]]
            val intTypeInspectInfo = typeByIdOpt match {
              case Some(ti @ BasicTypeInfo("Int", `intTypeId`, DeclaredAs.Class, "scala.Int", List(), List(), Some(_), None)) =>
                ti
              case _ =>
                fail("type by id does not match expectations, got " + typeByIdOpt)
            }

            //-----------------------------------------------------------------------------------------------
            // inspect type by id
            project ! InspectTypeByIdReq(intTypeId)
            val inspectByIdOpt = expectMsgType[Option[TypeInspectInfo]]

            inspectByIdOpt match {
              case Some(TypeInspectInfo(`intTypeInspectInfo`, Some(intCompanionId), supers, _)) =>
              case _ =>
                fail("inspect by id does not match expectations, got: " + inspectByIdOpt)
            }

            //-----------------------------------------------------------------------------------------------
            // uses of symbol at point

            log.info("------------------------------------222-")

            // FIXME: doing a fresh typecheck is needed to pass the next few tests. Why?
            project ! TypecheckFilesReq(List(fooFile))
            expectMsg(VoidResponse)

            asyncHelper.receiveN(2) should contain only (
              ClearAllScalaNotesEvent,
              FullTypeCheckCompleteEvent
            )

            project ! UsesOfSymbolAtPointReq(fooFile, 119) // point on testMethod
            val useOfSymbolAtPoint = expectMsgType[List[ERangePosition]]
            useOfSymbolAtPoint match {
              case List(ERangePosition(`fooFilePath`, 114, 110, 172), ERangePosition(`fooFilePath`, 273, 269, 283)) =>
              case _ =>
                fail("rpcUsesOfSymAtPoint not match expectations, got: " + useOfSymbolAtPoint)
            }

            log.info("------------------------------------222-")

            // note that the line numbers appear to have been stripped from the
            // scala library classfiles, so offset/line comes out as zero unless
            // loaded by the pres compiler
            project ! SymbolAtPointReq(fooFile, 276)
            val testMethodSymbolInfo = expectMsgType[Option[SymbolInfo]]
            testMethodSymbolInfo match {
              case Some(SymbolInfo("testMethod", "testMethod", Some(OffsetSourcePosition(`fooFile`, 114)), ArrowTypeInfo("(i: Int, s: String)Int", 126, BasicTypeInfo("Int", 1, DeclaredAs.Class, "scala.Int", List(), List(), None, None), List(ParamSectionInfo(List((i, BasicTypeInfo("Int", 1, DeclaredAs.Class, "scala.Int", List(), List(), None, None)), (s, BasicTypeInfo("String", 39, DeclaredAs.Class, "java.lang.String", List(), List(), None, None))), false))), true, Some(_))) =>
              case _ =>
                fail("symbol at point (local test method), got: " + testMethodSymbolInfo)
            }

            // M-.  external symbol
            project ! SymbolAtPointReq(fooFile, 190)
            val genericMethodSymbolAtPointRes = expectMsgType[Option[SymbolInfo]]
            genericMethodSymbolAtPointRes match {

              case Some(SymbolInfo("apply", "apply", Some(_),
                ArrowTypeInfo("[A, B](elems: (A, B)*)CC[A,B]", _,
                  BasicTypeInfo("CC", _, DeclaredAs.Nil, "scala.collection.generic.CC",
                    List(
                      BasicTypeInfo("A", _, DeclaredAs.Nil, "scala.collection.generic.A", List(), List(), None, None),
                      BasicTypeInfo("B", _, DeclaredAs.Nil, "scala.collection.generic.B", List(), List(), None, None)
                      ), List(), None, None),
                  List(ParamSectionInfo(List(
                    ("elems", BasicTypeInfo("<repeated>", _, DeclaredAs.Class, "scala.<repeated>", List(
                      BasicTypeInfo("Tuple2", _, DeclaredAs.Class, "scala.Tuple2", List(
                        BasicTypeInfo("A", _, DeclaredAs.Nil, "scala.collection.generic.A", List(), List(), None, None),
                        BasicTypeInfo("B", _, DeclaredAs.Nil, "scala.collection.generic.B", List(), List(), None, None)
                        ), List(), None, None)), List(), None, None))), false))), true, Some(_))) =>
              case _ =>
                fail("symbol at point (local test method), got: " + genericMethodSymbolAtPointRes)
            }

            // C-c C-v p Inspect source of current package
            project ! InspectPackageByPathReq("org.example")
            val insPacByPathResOpt = expectMsgType[Option[PackageInfo]]
            insPacByPathResOpt match {
              case Some(PackageInfo("example", "org.example", List(
                BasicTypeInfo("Bloo", _: Int, DeclaredAs.Class, "org.example.Bloo", List(), List(), Some(_), None),
                BasicTypeInfo("Bloo$", _: Int, DeclaredAs.Object, "org.example.Bloo$", List(), List(), Some(_), None),
                BasicTypeInfo("CaseClassWithCamelCaseName", _: Int, DeclaredAs.Class, "org.example.CaseClassWithCamelCaseName", List(), List(), Some(_), None),
                BasicTypeInfo("CaseClassWithCamelCaseName$", _: Int, DeclaredAs.Object, "org.example.CaseClassWithCamelCaseName$", List(), List(), Some(_), None),
                BasicTypeInfo("Foo", _: Int, DeclaredAs.Class, "org.example.Foo", List(), List(), None, None),
                BasicTypeInfo("Foo$", _: Int, DeclaredAs.Object, "org.example.Foo$", List(), List(), Some(_), None),
                BasicTypeInfo("package$", _: Int, DeclaredAs.Object, "org.example.package$", List(), List(), None, None),
                BasicTypeInfo("package$", _: Int, DeclaredAs.Object, "org.example.package$", List(), List(), None, None)))) =>
              case _ =>
                fail("inspect package by path failed, got: " + insPacByPathResOpt)
            }

            // expand selection around 'val foo'
            project ! ExpandSelectionReq(fooFile, 215, 215)
            val expandRange1 = expectMsgType[FileRange]
            assert(expandRange1 == FileRange(fooFilePath, 214, 217))

            project ! ExpandSelectionReq(fooFile, 214, 217)
            val expandRange2 = expectMsgType[FileRange]
            assert(expandRange2 == FileRange(fooFilePath, 210, 229))

            // TODO get the before content of the file

            project ! PrepareRefactorReq(1234, null, RenameRefactorDesc("bar", fooFile, 215, 215), false)
            expectMsgPF() {
              case RefactorEffect(
                1234,
                RefactorType.Rename,
                List(
                  TextEdit(`fooFile`, 214, 217, "bar"),
                  TextEdit(`fooFile`, 252, 255, "bar"),
                  TextEdit(`fooFile`, 269, 272, "bar")
                  ), _) =>
            }

            project ! ExecRefactorReq(1234, RefactorType.Rename)
            expectMsgPF() {
              case RefactorResult(1234, RefactorType.Rename, List(`fooFile`), _) =>
            }
          }
        }
      }
    }
  }

}
