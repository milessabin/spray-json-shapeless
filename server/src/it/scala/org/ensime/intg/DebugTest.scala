package org.ensime.intg

import java.io.File

import org.ensime.core.{ DebugVMDisconnectEvent, DebugBreakEvent, DebugVmSuccess }
import org.ensime.model._
import org.ensime.server.Server
import org.scalatest.FunSpec
import pimpathon.file._
import pimpathon.option._

import org.scalatest._
import scala.concurrent.duration._

import org.ensime.util.RichFile._

import org.ensime.fixture._

// must be refreshing as the tests don't clean up after themselves properly
class DebugTest extends WordSpec with Matchers with Inside
    with IsolatedServerFixture with DebugTestUtils {

  val original = EnsimeConfigFixture.DebugTestProject.copy(
    javaLibs = Nil // no need to index the JRE
  )

  "Debug - stepping" should {
    // TODO This is broken because step in our case is stepping into
    // the classloader for inner class rather than doing a user
    // visible step.
    "handle basic stepping" ignore withDebugSession(
      "stepping.ForComprehensionListString",
      "stepping/ForComprehensionListString.scala",
      9
    ) { (server, _, _) =>
        implicit val s = server
        checkTopStackFrame("stepping.ForComprehensionListString$", "main", 9)
        server.project.rpcDebugNext("1")
        checkTopStackFrame("stepping.ForComprehensionListString$$anonfun$main$1", "apply", 10)
      }
  }

  "Breakpoints" should {
    "trigger/continue" in withDebugSession(
      "breakpoints.Breakpoints",
      "breakpoints/Breakpoints.scala",
      32
    ) { (server, asyncHelper, breakpointsFile) =>
        implicit val s = server
        val project = server.project
        val breakpointsPath = breakpointsFile.getAbsolutePath

        project.rpcDebugBacktrace("1", 0, 3) should matchPattern {
          case DebugBacktrace(List(
            DebugStackFrame(0, List(), 0, "breakpoints.Breakpoints", "mainTest",
              LineSourcePosition(`breakpointsFile`, 32), _),
            DebugStackFrame(1, List(
              DebugStackLocal(0, "args", "Array[]", "java.lang.String[]")
              ), 1, "breakpoints.Breakpoints$", "main",
              LineSourcePosition(`breakpointsFile`, 41), _),
            DebugStackFrame(2, List(), 1, "breakpoints.Breakpoints", "main",
              LineSourcePosition(`breakpointsFile`, _), _)
            ), "1", "main") =>
        }

        //            val bp11 = session.addLineBreakpoint(BP_TYPENAME, 11)
        project.rpcDebugSetBreakpoint(breakpointsPath, 11)
        //            val bp13 = session.addLineBreakpoint(BP_TYPENAME, 13)
        project.rpcDebugSetBreakpoint(breakpointsPath, 13)
        try {
          //              session.waitForBreakpointsToBeEnabled(bp11, bp13)

          //              session.resumetoSuspension()
          //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)

          project.rpcDebugContinue("1")
          asyncHelper.expectAsync(30.seconds, DebugBreakEvent("1", "main", breakpointsFile, 11))

          //              session.resumetoSuspension()
          //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 13)

          project.rpcDebugContinue("1")
          asyncHelper.expectAsync(30.seconds, DebugBreakEvent("1", "main", breakpointsFile, 13))

          //              bp11.setEnabled(false)
          project.rpcDebugClearBreakpoint(breakpointsPath, 11)
          //              session.waitForBreakpointsToBeDisabled(bp11)
          //
          //              session.resumetoSuspension()
          project.rpcDebugContinue("1")
          //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 13)
          asyncHelper.expectAsync(30.seconds, DebugBreakEvent("1", "main", breakpointsFile, 13))
          //
          //              bp11.setEnabled(true); bp13.setEnabled(false)
          project.rpcDebugSetBreakpoint(breakpointsPath, 11)
          project.rpcDebugClearBreakpoint(breakpointsPath, 13)
          //
          //              session.waitForBreakpointsToBeEnabled(bp11)
          //              session.waitForBreakpointsToBeDisabled(bp13)
          //
          //              session.resumetoSuspension()
          //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)
          project.rpcDebugContinue("1")
          asyncHelper.expectAsync(30.seconds, DebugBreakEvent("1", "main", breakpointsFile, 11))
          //
          //              session.resumetoSuspension()
          //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)
          project.rpcDebugContinue("1")
          asyncHelper.expectAsync(30.seconds, DebugBreakEvent("1", "main", breakpointsFile, 11))
          //
          project.rpcDebugContinue("1")
          //asyncHelper.expectAsync(30.seconds, DebugVMDisconnectEvent)
          //              session.resumeToCompletion()
        } finally {
          //              bp11.delete()
          //              bp13.delete()
        }
      }

    "list/clear" in withDebugSession(
      "breakpoints.Breakpoints",
      "breakpoints/Breakpoints.scala",
      32
    ) { (server, asyncHelper, breakpointsFile) =>
        implicit val s = server
        val project = server.project
        val breakpointsPath = breakpointsFile.getAbsolutePath

        // TODO: test listing/clearing pending breakpoints (i.e. before we connect)

        project.rpcDebugListBreakpoints() should matchPattern {
          case BreakpointList(Nil, Nil) =>
        }

        // break in main
        project.rpcDebugSetBreakpoint(breakpointsPath, 11)
        project.rpcDebugSetBreakpoint(breakpointsPath, 13)

        // breakpoints should now be active
        inside(project.rpcDebugListBreakpoints()) {
          case BreakpointList(activeBreakpoints, pendingBreakpoints) =>
            activeBreakpoints should contain theSameElementsAs Set(
              Breakpoint(breakpointsFile, 11), Breakpoint(breakpointsFile, 13)
            )
            pendingBreakpoints shouldBe empty
        }

        // check clear works again
        project.rpcDebugClearAllBreakpoints()
        project.rpcDebugListBreakpoints() should matchPattern {
          case BreakpointList(Nil, Nil) =>
        }
      }
  }

  "Debug Inspect variables" should {

    // starting up a debug session for each variable is unneeded and wasteful of test time.
    // this approach means that there is one test method, but it still explores all of the paths.

    "inspect variables" in withDebugSession(
      "debug.Variables",
      "debug/Variables.scala",
      21
    ) {
        (server, asyncHelper, variablesFile) =>
          implicit val s = server
          // boolean local
          getVariableValue("1", "a") should matchPattern {
            case DebugPrimitiveValue("true", "boolean") =>
          }

          // char local
          getVariableValue("1", "b") should matchPattern {
            case DebugPrimitiveValue("'c'", "char") =>
          }

          // short local
          getVariableValue("1", "c") should matchPattern {
            case DebugPrimitiveValue("3", "short") =>
          }

          // int local
          getVariableValue("1", "d") should matchPattern {
            case DebugPrimitiveValue("4", "int") =>
          }

          // long local
          getVariableValue("1", "e") should matchPattern {
            case DebugPrimitiveValue("5", "long") =>
          }

          // float local
          getVariableValue("1", "f") should matchPattern {
            case DebugPrimitiveValue("1.0", "float") =>
          }

          // double local
          getVariableValue("1", "g") should matchPattern {
            case DebugPrimitiveValue("2.0", "double") =>
          }

          // String local
          inside(getVariableValue("1", "h")) {
            case DebugStringInstance("\"test\"", debugFields, "java.lang.String", _) =>
              exactly(1, debugFields) should matchPattern {
                case DebugClassField(_, "value", "char[]", "Array['t', 'e', 's',...]") =>
              }
          }

          // primitive array local
          getVariableValue("1", "i") should matchPattern {
            case DebugArrayInstance(3, "int[]", "int", _) =>
          }

          // type local
          inside(getVariableValue("1", "j")) {
            case DebugObjectInstance("Instance of $colon$colon", debugFields, "scala.collection.immutable.$colon$colon", _) =>
              exactly(1, debugFields) should matchPattern {
                case DebugClassField(_, "head", "java.lang.Object", "Instance of Integer") =>
              }
          }

          // object array local
          getVariableValue("1", "k") should matchPattern {
            case DebugArrayInstance(3, "java.lang.Object[]", "java.lang.Object", _) =>
          }
      }
  }
}

trait DebugTestUtils {
  this: ServerFixture with Matchers =>

  /**
   * @param fileName to place the breakpoint
   * @param className containing the main method
   * @param breakLine where to start the session in the fileName
   */
  def withDebugSession(
    className: String,
    fileName: String,
    breakLine: Int)(
      f: (Server, AsyncMsgHelper, File) => Any): Any = withServer { (server, asyncHelper) =>
    val project = server.project
    val config = project.config
    val resolvedFile = (config.subprojects.head.sourceRoots.head / fileName)

    project.rpcDebugSetBreakpoint(fileName, breakLine)

    val startStatus = project.rpcDebugStartVM(className)
    assert(startStatus == DebugVmSuccess())

    asyncHelper.expectAsync(30.seconds, DebugBreakEvent("1", "main", resolvedFile, breakLine))
    project.rpcDebugClearBreakpoint(fileName, breakLine)

    try {
      f(server, asyncHelper, resolvedFile)
    } finally {
      project.rpcDebugClearAllBreakpoints() // otherwise we can have pending

      // no way to await the stopped condition so we let the app run
      // its course on the main thread
      project.rpcDebugContinue("1")
      project.rpcDebugStopVM()

      //asyncHelper.expectAsync(30 seconds, DebugVMDisconnectEvent)
    }
  }

  def getVariableValue(threadId: String, variableName: String)(implicit server: Server): DebugValue = {
    val project = server.project
    val vLocOpt = project.rpcDebugLocateName(threadId, variableName)
    val vLoc = vLocOpt.getOrThrow(
      s"unable to locate variable $variableName on thread $threadId")

    val vValueOpt = project.rpcDebugValue(vLoc)
    val vValue = vValueOpt.getOrThrow(
      s"Unable to get value of variable $variableName")
    vValue
  }

  def checkTopStackFrame(className: String, method: String, line: Int)(implicit server: Server): Unit = {
    server.project.rpcDebugBacktrace("1", 0, 1) should matchPattern {
      case DebugBacktrace(List(DebugStackFrame(0, _, 1, `className`, `method`,
        LineSourcePosition(_, `line`), _)),
        "1", "main") =>
    }
  }
}
