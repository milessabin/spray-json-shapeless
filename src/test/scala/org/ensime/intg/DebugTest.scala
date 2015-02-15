package org.ensime.intg

import java.io.File

import org.ensime.core.{ DebugVMDisconnectEvent, DebugBreakEvent, DebugVmSuccess }
import org.ensime.model._
import org.scalatest.FunSpec
import pimpathon.file._

import scala.concurrent.duration._

class DebugTest extends FunSpec with AbstractWorkflowTest {

  val path = "src/testprojects/debug/src"

  def withSrcFile[T](path: File)(f: (String, File) => T): T = {
    val filePath = path.getCanonicalPath
    val file = path.getCanonicalFile
    f(filePath, file)
  }

  def withDebugSession[T](mainPath: String, mainFile: File, className: String, breakLine: Int)(f: => T): T = {
    project.rpcDebugSetBreakpoint(mainPath, breakLine)

    val startStatus = project.rpcDebugStartVM(className)
    assert(startStatus == DebugVmSuccess)

    asyncHelper.expectAsync(30.seconds, DebugBreakEvent(1, "main", LineSourcePosition(mainFile, breakLine)))
    project.rpcDebugClearBreakpoint(mainPath, breakLine)
    try {
      f
    } finally {
      project.rpcDebugStopVM()
    }
  }

  def getVariableValue(threadId: Int, variableName: String): DebugValue = {
    val vLocOpt = project.rpcDebugLocateName(threadId, variableName)
    val vLoc = vLocOpt.getOrElse(throw new IllegalStateException("unable to locate variable " + variableName +
      " on thread " + threadId))

    val vValueOpt = project.rpcDebugValue(vLoc)
    val vValue = vValueOpt.getOrElse(throw new IllegalStateException("Unable to get value of variable " + variableName))
    vValue
  }

  def inspectVar(threadId: Int, variableName: String)(pf: PartialFunction[DebugValue, Unit]) {
    //    def inspectVar[T](threadId: Int, variableName: String)(pf: PartialFunction[DebugValue, T]) = {
    val value = getVariableValue(threadId, variableName)
    if (pf.isDefinedAt(value)) {
      pf.apply(value)
    } else
      throw new IllegalStateException("Unexpected value for " + variableName + ": " + value)
  }

  def checkTopStackFrame(className: String, method: String, line: Int): Unit = {
    val trace = project.rpcDebugBacktrace(1, 0, 1)
    trace match {
      case DebugBacktrace(List(DebugStackFrame(0, _, 1, `className`, `method`, LineSourcePosition(_, `line`), _)), 1, "main") =>
      case _ =>
        fail("Unexpected backtrace, expected " + className + "," + method + "," + line + " got: " + trace)
    }
  }

  describe("Debug - stepping") {
    // TODO This is broken because step in our case is stepping into the classloader for inner class rather than doing a user visible step.
    ignore("Should handle basic stepping") {
      val sourceRoot = config.modules.values.head.sourceRoots.head
      withSrcFile(sourceRoot / "stepping/ForComprehensionListString.scala") {
        case (steppingPath, steppingFile) =>
          // break in main
          withDebugSession(steppingPath, steppingFile, "stepping.ForComprehensionListString", 9) {
            checkTopStackFrame("stepping.ForComprehensionListString$", "main", 9)
            project.rpcDebugNext(1)
            checkTopStackFrame("stepping.ForComprehensionListString$$anonfun$main$1", "apply", 10)

          }
      }
    }
  }
  describe("Debug - breakpoints") {

    it("should handle simple breakpoints") {

      val sourceRoot = config.modules.values.head.sourceRoots.head
      withSrcFile(sourceRoot / "breakpoints/Breakpoints.scala") {
        case (breakpointsPath, breakpointsFile) =>
          // break in main
          withDebugSession(breakpointsPath, breakpointsFile, "breakpoints.Breakpoints", 32) {

            val backtraceRes = project.rpcDebugBacktrace(1, 0, 3)
            backtraceRes match {
              case DebugBacktrace(List(
                DebugStackFrame(0, List(), 0, "breakpoints.Breakpoints", "mainTest", LineSourcePosition(`breakpointsFile`, 32), _),
                DebugStackFrame(1, List(DebugStackLocal(0, "args", "java.lang.String[]", "Array[]")), 1, "breakpoints.Breakpoints$", "main", LineSourcePosition(`breakpointsFile`, 41), _),
                DebugStackFrame(2, List(), 1, "breakpoints.Breakpoints", "main", LineSourcePosition(`breakpointsFile`, _), _)), 1, "main") =>
              case _ => fail("Unexpected result for backtrace: " + backtraceRes)
            }

            //            val bp11 = session.addLineBreakpoint(BP_TYPENAME, 11)
            project.rpcDebugSetBreakpoint(breakpointsPath, 11)
            //            val bp13 = session.addLineBreakpoint(BP_TYPENAME, 13)
            project.rpcDebugSetBreakpoint(breakpointsPath, 13)
            try {
              //              session.waitForBreakpointsToBeEnabled(bp11, bp13)

              //              session.resumetoSuspension()
              //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)

              project.rpcDebugContinue(1)
              asyncHelper.expectAsync(30.seconds, DebugBreakEvent(1, "main", LineSourcePosition(breakpointsFile, 11)))

              //              session.resumetoSuspension()
              //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 13)

              project.rpcDebugContinue(1)
              asyncHelper.expectAsync(30.seconds, DebugBreakEvent(1, "main", LineSourcePosition(breakpointsFile, 13)))

              //              bp11.setEnabled(false)
              project.rpcDebugClearBreakpoint(breakpointsPath, 11)
              //              session.waitForBreakpointsToBeDisabled(bp11)
              //
              //              session.resumetoSuspension()
              project.rpcDebugContinue(1)
              //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 13)
              asyncHelper.expectAsync(30.seconds, DebugBreakEvent(1, "main", LineSourcePosition(breakpointsFile, 13)))
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
              project.rpcDebugContinue(1)
              asyncHelper.expectAsync(30.seconds, DebugBreakEvent(1, "main", LineSourcePosition(breakpointsFile, 11)))
              //
              //              session.resumetoSuspension()
              //              session.checkStackFrame(BP_TYPENAME, "simple1()V", 11)
              project.rpcDebugContinue(1)
              asyncHelper.expectAsync(30.seconds, DebugBreakEvent(1, "main", LineSourcePosition(breakpointsFile, 11)))
              //
              project.rpcDebugContinue(1)
              asyncHelper.expectAsync(30.seconds, DebugVMDisconnectEvent())
              //              session.resumeToCompletion()
            } finally {
              //              bp11.delete()
              //              bp13.delete()
            }
            //
          }
      }

    }

    it("should handle list and clear breakpoints") {

      val sourceRoot = config.modules.values.head.sourceRoots.head
      withSrcFile(sourceRoot / "breakpoints/Breakpoints.scala") {
        case (breakpointsPath, breakpointsFile) =>

          // breakpoint will be pending as server has not started yet
          project.rpcDebugSetBreakpoint(breakpointsPath, 11)
          project.rpcDebugSetBreakpoint(breakpointsPath, 13)

          val breakpointsSet = Set(Breakpoint(LineSourcePosition(breakpointsFile, 11)), Breakpoint(LineSourcePosition(breakpointsFile, 13)))

          project.rpcDebugListBreakpoints() match {
            case BreakpointList(Nil, pending) =>
              require(pending.toSet === breakpointsSet)
            case bpl => fail("Breakpoint list not as expected: " + bpl)
          }

          // break in main
          withDebugSession(breakpointsPath, breakpointsFile, "breakpoints.Breakpoints", 32) {
            //            project.rpcDebugSetBreakpoint(breakpointsPath, 11)
            //            project.rpcDebugSetBreakpoint(breakpointsPath, 13)

            // breakpoints should now be active
            project.rpcDebugListBreakpoints() match {

              case BreakpointList(activeBreakpoints, pendingBreakpoints) =>

                require(activeBreakpoints.toSet === breakpointsSet)
                require(pendingBreakpoints.toSet === breakpointsSet)
              case bpl => fail("Breakpoint list not as expected: " + bpl)
            }

            // check clear works
            project.rpcDebugClearAllBreakpoints()
            project.rpcDebugListBreakpoints() match {
              case BreakpointList(Nil, Nil) =>
              case bpl => fail("Breakpoint list not as expected: " + bpl)
            }

          }
      }
    }
  }

  describe("Debug Inspect variables") {

    // starting up a debug session for each variable is unneeded and wasteful of test time.
    // this approach means that there is one test method, but it still explores all of the paths.

    it("should inspect variables") {
      val sourceRoot = config.modules.values.head.sourceRoots.head
      withSrcFile(sourceRoot / "debug/Variables.scala") {
        case (variablesPath, variablesFile) =>
          withDebugSession(variablesPath, variablesFile, "debug.Variables", 30) {

            // boolean local
            inspectVar(1, "a") {
              case DebugPrimitiveValue("true", "boolean") =>
            }

            // char local
            inspectVar(1, "b") {
              case DebugPrimitiveValue("'c'", "char") =>
            }

            // short local
            inspectVar(1, "c") {
              case DebugPrimitiveValue("3", "short") =>
            }

            // int local
            inspectVar(1, "d") {
              case DebugPrimitiveValue("4", "int") =>
            }

            // long local
            inspectVar(1, "e") {
              case DebugPrimitiveValue("5", "long") =>
            }

            // float local
            inspectVar(1, "f") {
              case DebugPrimitiveValue("1.0", "float") =>
            }

            // double local
            inspectVar(1, "g") {
              case DebugPrimitiveValue("2.0", "double") =>
            }

            // String local
            inspectVar(1, "h") {
              case DebugStringInstance("\"test\"", debugFields, "java.lang.String", _) =>
                require(debugFields.exists {
                  case DebugClassField(_, "value", "char[]", "Array['t', 'e', 's',...]") => true
                  case _ => false
                })
            }

            // primitive array local
            inspectVar(1, "i") {
              case DebugArrayInstance(3, "int[]", "int", _) =>
            }

            // type local
            inspectVar(1, "j") {
              case DebugObjectInstance("Instance of $colon$colon", debugFields,
                "scala.collection.immutable.$colon$colon", _) =>
                require(debugFields.exists {
                  case DebugClassField(_, "head", "java.lang.Object", "Instance of Integer") => true
                  case _ => false
                })
            }

            // object array local
            inspectVar(1, "k") {
              case DebugArrayInstance(3, "java.lang.Object[]", "java.lang.Object", _) =>
            }

          }
      }
    }
  }
}
