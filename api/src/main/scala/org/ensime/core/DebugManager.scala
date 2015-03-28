package org.ensime.core

import java.io.{ File, InputStream, InputStreamReader }

import org.ensime.config._
import org.ensime.model._
import org.ensime.util._

import scala.collection.mutable.ListBuffer
import scala.collection.{ Iterable, mutable }

case object DebuggerShutdownEvent

case class DebugStartVMReq(commandLine: String) extends RPCRequest
case class DebugAttachVMReq(hostname: String, port: String) extends RPCRequest
case object DebugStopVMReq extends RPCRequest
case object DebugRunReq extends RPCRequest
case class DebugContinueReq(threadId: DebugThreadId) extends RPCRequest
case class DebugNextReq(threadId: DebugThreadId) extends RPCRequest
case class DebugStepReq(threadId: DebugThreadId) extends RPCRequest
case class DebugStepOutReq(threadId: DebugThreadId) extends RPCRequest
case class DebugLocateNameReq(threadId: DebugThreadId, name: String) extends RPCRequest
case class DebugValueReq(loc: DebugLocation) extends RPCRequest
case class DebugToStringReq(threadId: DebugThreadId, loc: DebugLocation) extends RPCRequest
case class DebugSetValueReq(loc: DebugLocation, newValue: String) extends RPCRequest
case class DebugBacktraceReq(threadId: DebugThreadId, index: Int, count: Int) extends RPCRequest
case object DebugActiveVMReq extends RPCRequest
case class DebugSetBreakpointReq(file: String, line: Int) extends RPCRequest
case class DebugClearBreakpointReq(file: String, line: Int) extends RPCRequest
case object DebugClearAllBreakpointsReq extends RPCRequest
case object DebugListBreakpointsReq extends RPCRequest

abstract class DebugVmStatus

// must have redundant status: String to match legacy API
case class DebugVmSuccess(
  status: String = "success") extends DebugVmStatus
case class DebugVmError(
  errorCode: Int,
  details: String,
  status: String = "error") extends DebugVmStatus
