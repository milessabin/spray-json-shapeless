package org.ensime.test.intg

import org.ensime.model._
import org.ensime.protocol.DebugBreakEvent
import org.ensime.server.DebugVmSuccess
import org.scalatest.{ FunSpec, Matchers }
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import pimpathon.file._

class DebugWorkflow extends FunSpec with Matchers {

  val log = LoggerFactory.getLogger(this.getClass)

  describe("Server (debug workflow - direct)") {
    it("should allow debugging of a project") {
      IntgUtil.withTestProject("src/example-simple") { (config, project, asycHelper) =>
        val sourceRoot = config.modules.values.head.sourceRoots.head
        val fooFilePath = (sourceRoot / "org/example/Foo.scala").getCanonicalPath
        val fooFile = (sourceRoot / "org/example/Foo.scala").getCanonicalFile

        project.rpcDebugSetBreakpoint(fooFilePath, 14)

        val startStatus = project.rpcDebugStartVM("org.example.Foo")
        assert(startStatus == DebugVmSuccess)

        asycHelper.expectAsync(30.seconds, DebugBreakEvent(1, "main", LineSourcePosition(fooFile, 14)))

        // get the 5 top stack frames from the main thread
        val backtraceRes = project.rpcDebugBacktrace(1, 0, 3)
        backtraceRes match {
          case DebugBacktrace(List(
            DebugStackFrame(0, List(), 0, "org.example.Foo$", "delayedEndpoint$org$example$Foo$1", LineSourcePosition(`fooFile`, 14), _),
            DebugStackFrame(1, List(), 0, "org.example.Foo$delayedInit$body", "apply", LineSourcePosition(`fooFile`, 3), _),
            DebugStackFrame(2, List(DebugStackLocal(0, "$this", "scala.Function0", "Instance of Foo$delayedInit$body")),
              1, "scala.Function0$class", "apply$mcV$sp", LineSourcePosition(function0SourceFile, 40), _)), 1, "main") =>
            // TODO function0SourceFile  looks broken
            log.warn("FunctionSourceFile looks borked: " + function0SourceFile)
          case _ => fail("Unexpected result for backtrace: " + backtraceRes)
        }

        val listBreakpointsRes = project.rpcDebugListBreakpoints()
        listBreakpointsRes match {
          case BreakpointList(List(Breakpoint(LineSourcePosition(`fooFile`, 14))), List(Breakpoint(LineSourcePosition(`fooFile`, 14)))) =>
          case _ => fail("Unexpected result for list breakpoints: " + listBreakpointsRes)
        }

        project.rpcDebugClearBreakpoint(fooFilePath, 14)

        val listBreakpointsRes2 = project.rpcDebugListBreakpoints()
        listBreakpointsRes2 match {
          case BreakpointList(Nil, Nil) =>
          case _ => fail("Unexpected result for list breakpoints after remove: " + listBreakpointsRes2)
        }

        project.rpcDebugStopVM()
      }
    }
  }
}
