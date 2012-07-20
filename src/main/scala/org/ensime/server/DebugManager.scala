/**
 *  Copyright (c) 2010, Aemon Cannon
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *      * Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      * Neither the name of ENSIME nor the
 *        names of its contributors may be used to endorse or promote products
 *        derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
 *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.ensime.server
import com.sun.jdi.request.BreakpointRequest
import com.sun.jdi.request.EventRequest
import com.sun.jdi.request.StepRequest
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import org.ensime.config.ProjectConfig
import org.ensime.model._
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.{ Iterable, Map }
import com.sun.jdi._
import com.sun.jdi.event._

case class DebuggerShutdownEvent()

case class DebugStartVMReq(commandLine: String)
case class DebugAttachVMReq(hostname: String, port: String)
case class DebugStopVMReq()
case class DebugRunReq()
case class DebugContinueReq(threadId: Long)
case class DebugNextReq(threadId: Long)
case class DebugStepReq(threadId: Long)
case class DebugStepOutReq(threadId: Long)
case class DebugValueForNameReq(threadId: Long, name: String)
case class DebugValueForFieldReq(objectId: Long, name: String)
case class DebugValueForStackVarReq(threadId: Long, frame: Int, index: Int)
case class DebugValueForIdReq(objectId: Long)
case class DebugValueForIndexReq(objectId: Long, index: Int)
case class DebugBacktraceReq(threadId: Long, index: Int, count: Int)
case class DebugActiveVMReq()
case class DebugBreakReq(file: String, line: Int)
case class DebugClearBreakReq(file: String, line: Int)
case class DebugClearAllBreaksReq()
case class DebugListBreaksReq()

abstract class DebugVmStatus

case class DebugVmSuccess() extends DebugVmStatus
case class DebugVmError(code: Int, details: String) extends DebugVmStatus

abstract class DebugEvent
case class DebugStepEvent(threadId: Long,
  threadName: String, pos: SourcePosition) extends DebugEvent
case class DebugBreakEvent(threadId: Long,
  threadName: String, pos: SourcePosition) extends DebugEvent
case class DebugVMDeathEvent() extends DebugEvent
case class DebugVMStartEvent() extends DebugEvent
case class DebugVMDisconnectEvent() extends DebugEvent
case class DebugExceptionEvent(excId: Long,
  threadId: Long, threadName: String) extends DebugEvent
case class DebugThreadStartEvent(threadId: Long) extends DebugEvent
case class DebugThreadDeathEvent(threadId: Long) extends DebugEvent
case class DebugOutputEvent(out: String) extends DebugEvent

class DebugManager(project: Project, protocol: ProtocolConversions,
  config: ProjectConfig) extends Actor {

  import protocol._

  def ignoreErr[E <: Exception, T](action: => T, orElse: => T): T = {
    try { action } catch { case e: E => orElse }
  }

  def locToPos(loc: Location): Option[SourcePosition] = {
    try {
      (for (set <- sourceMap.get(loc.sourceName())) yield {
        if (set.size > 1) {
          System.err.println("Warning, ambiguous source name: " +
            loc.sourceName())
        }
        set.headOption.map(f => SourcePosition(f, loc.lineNumber))
      }).getOrElse(None)
    } catch {
      case e: AbsentInformationException => None
    }
  }

  private val sourceMap = HashMap[String, HashSet[CanonFile]]()
  def rebuildSourceMap() {
    sourceMap.clear()
    for (f <- config.sources) {
      val set = sourceMap.getOrElse(f.getName, HashSet())
      set.add(f)
      sourceMap(f.getName) = set
    }
  }
  rebuildSourceMap()

  def tryPendingBreaksForSourcename(sourcename: String) {
    for (breaks <- pendingBreaksBySourceName.get(sourcename)) {
      val toTry = HashSet() ++ breaks
      for (bp <- toTry) {
        setBreakpoint(bp.pos.file, bp.pos.line)
      }
    }
  }

  def setBreakpoint(file: CanonFile, line: Int): Boolean = {
    if ((for (vm <- maybeVM) yield {
      vm.setBreakpoint(file, line)
    }).getOrElse { false }) {
      activeBreakpoints.add(Breakpoint(SourcePosition(file, line)))
      true
    } else {
      addPendingBreakpoint(Breakpoint(SourcePosition(file, line)))
      false
    }
  }

  def clearBreakpoint(file: CanonFile, line: Int) {
    val clearBp = Breakpoint(SourcePosition(file, line))
    for (bps <- pendingBreaksBySourceName.get(file.getName)) {
      bps.retain { _ != clearBp }
    }
    val toRemove = activeBreakpoints.filter { _ == clearBp }
    for (vm <- maybeVM) {
      vm.clearBreakpoints(toRemove)
    }
    activeBreakpoints --= toRemove
  }

  def clearAllBreakpoints() {
    pendingBreaksBySourceName.clear()
    activeBreakpoints.clear()
    for (vm <- maybeVM) {
      vm.clearAllBreakpoints()
    }
  }

  def moveActiveBreaksToPending() {
    for (bp <- activeBreakpoints) {
      addPendingBreakpoint(bp)
    }
    activeBreakpoints.clear()
  }

  def addPendingBreakpoint(bp: Breakpoint) {
    val file = bp.pos.file
    val breaks = pendingBreaksBySourceName.get(file.getName).getOrElse(HashSet())
    breaks.add(bp)
    pendingBreaksBySourceName(file.getName) = breaks
  }

  private val activeBreakpoints = HashSet[Breakpoint]()
  private val pendingBreaksBySourceName = new HashMap[String, HashSet[Breakpoint]] {
    override def default(key: String) = new HashSet()
  }

  def pendingBreakpoints: List[Breakpoint] = {
    pendingBreaksBySourceName.values.flatten.toList
  }

  def vmOptions(): List[String] = {
    List("-classpath", config.debugClasspath)
  }

  private var maybeVM: Option[VM] = None

  def withVM[T](action: (VM => T)): Option[T] = {
    maybeVM.synchronized {
      for (vm <- maybeVM) yield {
        action(vm)
      }
    }
  }

  private def handleRPCWithVM(callId: Int)(action: (VM => Unit)) = {
    try {
      withVM { vm =>
        action(vm)
      }.getOrElse {
        project ! RPCResultEvent(toWF(false), callId)
        System.err.println("No VM under debug!")
      }
    } catch {
      case e: AbsentInformationException => {
        e.printStackTrace()
        project ! RPCResultEvent(toWF(false), callId)
      }
    }
  }

  private def handleRPCWithVMAndThread(callId: Int,
    threadId: Long)(action: ((VM, ThreadReference) => Unit)) = {
    try {
      withVM { vm =>
        (for (thread <- vm.threadById(threadId)) yield {
          action(vm, thread)
        }).getOrElse {
          System.err.println("Couldn't find thread: " + threadId)
          project ! RPCResultEvent(toWF(false), callId)
        }
      }.getOrElse {
        System.err.println("No VM under debug!")
        project ! RPCResultEvent(toWF(false), callId)
      }
    } catch {
      case e: AbsentInformationException => {
        e.printStackTrace()
        project ! RPCResultEvent(toWF(false), callId)
      }
    }
  }

  def act() {
    loop {
      try {
        receive {
          case DebuggerShutdownEvent => {
            withVM { vm =>
              vm.dispose()
            }
            exit('stop)
          }
          case evt: com.sun.jdi.event.Event => {
            try {
              evt match {
                case e: VMStartEvent => {
                  withVM { vm =>
                    vm.initLocationMap()
                  }
                  project ! AsyncEvent(toWF(DebugVMStartEvent()))
                }
                case e: VMDeathEvent => {
                  maybeVM = None
                  moveActiveBreaksToPending()
                  project ! AsyncEvent(toWF(DebugVMDeathEvent()))
                }
                case e: VMDisconnectEvent => {
                  maybeVM = None
                  moveActiveBreaksToPending()
                  project ! AsyncEvent(toWF(DebugVMDisconnectEvent()))
                }
                case e: StepEvent => {
                  (for (pos <- locToPos(e.location())) yield {
                    project ! AsyncEvent(toWF(DebugStepEvent(
                      e.thread().uniqueID(), e.thread().name, pos)))
                  }) getOrElse {
                    System.err.println("Step position not found: " +
                      e.location().sourceName() + " : " + e.location().lineNumber())
                  }
                }
                case e: BreakpointEvent => {
                  (for (pos <- locToPos(e.location())) yield {
                    project ! AsyncEvent(toWF(DebugBreakEvent(
                      e.thread().uniqueID(), e.thread().name, pos)))
                  }) getOrElse {
                    System.err.println("Break position not found: " +
                      e.location().sourceName() + " : " + e.location().lineNumber())
                  }
                }
                case e: ExceptionEvent => {
                  withVM { vm => vm.remember(e.exception) }
                  project ! AsyncEvent(toWF(DebugExceptionEvent(
                    e.exception.uniqueID(),
                    e.thread().uniqueID(),
                    e.thread().name)))
                }
                case e: ThreadDeathEvent => {
                  project ! AsyncEvent(toWF(DebugThreadDeathEvent(
                    e.thread().uniqueID())))
                }
                case e: ThreadStartEvent => {
                  project ! AsyncEvent(toWF(DebugThreadStartEvent(
                    e.thread().uniqueID())))
                }
                case e: AccessWatchpointEvent => {}
                case e: ClassPrepareEvent => {
                  withVM { vm =>
                    println("ClassPrepareEvent: " + e.referenceType().name())
                  }
                }
                case e: ClassUnloadEvent => {}
                case e: MethodEntryEvent => {}
                case e: MethodExitEvent => {}
                case _ => {}
              }

            } catch {
              case e: VMDisconnectedException =>
                {
                  System.err.println("Error handling DebugEvent:")
                  e.printStackTrace()
                  moveActiveBreaksToPending()
                  project ! AsyncEvent(toWF(DebugVMDisconnectEvent()))
                  maybeVM = None
                }
            }
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              req match {
                case DebugStartVMReq(commandLine: String) ⇒ {
                  withVM { vm ⇒
                    vm.dispose()
                  }
                  try {
                    val vm = new VM(VmStart(commandLine))
                    maybeVM = Some(vm)
                    vm.start()
                    project ! RPCResultEvent(toWF(DebugVmSuccess()), callId)
                  } catch {
                    case e: Exception => {
                      maybeVM = None
                      println("Couldn't start VM")
                      project ! RPCResultEvent(toWF(DebugVmError(1, e.toString)), callId)
                    }
                  }
                }

                case DebugAttachVMReq(hostname, port) ⇒ {
                  withVM { vm ⇒
                    vm.dispose()
                  }
                  try {
                    val vm = new VM(VmAttach(hostname, port))
                    maybeVM = Some(vm)
                    vm.start()
                    project ! RPCResultEvent(toWF(DebugVmSuccess()), callId)
                  } catch {
                    case e: Exception => {
                      maybeVM = None
                      println("Couldn't attach to target VM.")
                      println("e: " + e)
                      project ! RPCResultEvent(toWF(DebugVmError(1, e.toString)), callId)
                    }
                  }
                }

                case DebugActiveVMReq() => {
                  handleRPCWithVM(callId) { vm =>
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugStopVMReq() => {
                  handleRPCWithVM(callId) { vm =>
                    vm.dispose()
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugRunReq() => {
                  handleRPCWithVM(callId) { vm =>
                    vm.resume()
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugContinueReq(threadId) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      vm.resume()
                      project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugBreakReq(filepath: String, line: Int) => {
                  val file = CanonFile(filepath)
                  if (!setBreakpoint(file, line)) {
                    project.bgMessage("Location not loaded. Set pending breakpoint.")
                  }
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugClearBreakReq(filepath: String, line: Int) => {
                  val file = CanonFile(filepath)
                  clearBreakpoint(file, line)
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugClearAllBreaksReq() => {
                  clearAllBreakpoints()
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugListBreaksReq() => {
                  import scala.collection.JavaConversions._
                  val breaks = BreakpointList(
                    activeBreakpoints.toList,
                    pendingBreakpoints)
                  project ! RPCResultEvent(toWF(breaks), callId)
                }

                case DebugNextReq(threadId: Long) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      vm.newStepRequest(thread,
                        StepRequest.STEP_LINE,
                        StepRequest.STEP_OVER)
                      project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugStepReq(threadId: Long) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      vm.newStepRequest(thread,
                        StepRequest.STEP_LINE,
                        StepRequest.STEP_INTO)
                      project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugStepOutReq(threadId: Long) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      vm.newStepRequest(thread,
                        StepRequest.STEP_LINE,
                        StepRequest.STEP_OUT)
                      project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugValueForNameReq(threadId: Long, name: String) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      vm.valueForName(thread, name) match {
                        case Some(value) =>
                          project ! RPCResultEvent(toWF(value), callId)
                        case None =>
                          project ! RPCResultEvent(toWF(false), callId)
                      }
                  }

                }
                case DebugBacktraceReq(threadId: Long, index: Int, count: Int) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      val bt = vm.backtrace(thread, index, count)
                      project ! RPCResultEvent(toWF(bt), callId)
                  }
                }
                case DebugValueForIdReq(objectId: Long) => {
                  handleRPCWithVM(callId) {
                    (vm) =>
                      vm.valueForId(objectId) match {
                        case Some(value) =>
                          project ! RPCResultEvent(toWF(value), callId)
                        case None =>
                          project ! RPCResultEvent(toWF(false), callId)
                      }
                  }

                }
                case DebugValueForFieldReq(objectId: Long, name: String) => {
                  handleRPCWithVM(callId) {
                    (vm) =>
                      vm.valueForField(objectId, name) match {
                        case Some(value) =>
                          project ! RPCResultEvent(toWF(value), callId)
                        case None =>
                          project ! RPCResultEvent(toWF(false), callId)
                      }
                  }
                }
                case DebugValueForStackVarReq(
                  threadId: Long, frame: Int, offset: Int) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      vm.valueForStackVar(thread, frame, offset) match {
                        case Some(value) =>
                          project ! RPCResultEvent(toWF(value), callId)
                        case None =>
                          project ! RPCResultEvent(toWF(false), callId)
                      }
                  }
                }
                case DebugValueForIndexReq(objectId: Long, index: Int) => {
                  handleRPCWithVM(callId) {
                    (vm) =>
                      vm.valueForIndex(objectId, index) match {
                        case Some(value) =>
                          project ! RPCResultEvent(toWF(value), callId)
                        case None =>
                          project ! RPCResultEvent(toWF(false), callId)
                      }
                  }
                }
              }
            } catch {
              case e: VMDisconnectedException =>
                {
                  System.err.println("Error handling RPC:")
                  e.printStackTrace()
                  moveActiveBreaksToPending()
                  project ! AsyncEvent(toWF(DebugVMDisconnectEvent()))
                  project.sendRPCError(ErrExceptionInDebugger,
                    Some("VM is disconnected!"), callId)
                  maybeVM = None
                }
              case e: Exception =>
                {
                  System.err.println("Error handling RPC:")
                  e.printStackTrace()
                  project.sendRPCError(ErrExceptionInDebugger,
                    Some("Error occurred in Debug Manager. Check the server log."),
                    callId)
                }
            }
          }
          case other =>
            {
              println("Debug Manager: WTF, what's " + other)
            }
        }

      } catch {
        case e: Exception =>
          {
            System.err.println("Error at Debug Manager message loop:")
            e.printStackTrace()
          }
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing debug manager actor.")
  }

  private sealed abstract class VmMode()
  private case class VmAttach(hostname: String, port: String) extends VmMode()
  private case class VmStart(commandLine: String) extends VmMode()

  private class VM(mode: VmMode) {
    import scala.collection.JavaConversions._

    private val vm: VirtualMachine = {
      mode match {
        case VmStart(commandLine) ⇒ {
          val connector = Bootstrap.virtualMachineManager().defaultConnector()
          val arguments = connector.defaultArguments()

          val opts = arguments.get("options").value
          val allVMOpts = (List(opts) ++ vmOptions).mkString(" ")
          arguments.get("options").setValue(allVMOpts)
          arguments.get("main").setValue(commandLine)
          arguments.get("suspend").setValue("false")

          println("Using Connector: " + connector.name +
            " : " + connector.description())
          println("Connector class: " + connector.getClass.getName())
          println("Debugger VM args: " + allVMOpts)
          println("Debugger program args: " + commandLine)
          connector.launch(arguments)
        }
        case VmAttach(hostname, port) ⇒ {
          println("Attach to running vm")

          val vmm = Bootstrap.virtualMachineManager()
          val connector = vmm.attachingConnectors().get(0)

          val env = connector.defaultArguments()
          env.get("port").setValue(port)
          env.get("hostname").setValue(hostname)

          println("Using Connector: " + connector.name +
            " : " + connector.description())
          println("Debugger arguments: " + env)
          println("Attach to VM")
          val vm = connector.attach(env)
          println("VM: " + vm.description + ", " + vm)
          vm
        }

      }

    }

    vm.setDebugTraceMode(VirtualMachine.TRACE_EVENTS)
    val evtQ = new VMEventManager(vm.eventQueue())
    val erm = vm.eventRequestManager();
    {
      val req = erm.createClassPrepareRequest()
      req.setSuspendPolicy(EventRequest.SUSPEND_ALL)
      req.enable()
    }
    {
      val req = erm.createThreadStartRequest()
      req.setSuspendPolicy(EventRequest.SUSPEND_NONE)
      req.enable()
    }
    {
      val req = erm.createThreadDeathRequest()
      req.setSuspendPolicy(EventRequest.SUSPEND_NONE)
      req.enable()
    }
    {
      val req = erm.createExceptionRequest(null, false, true)
      req.setSuspendPolicy(EventRequest.SUSPEND_ALL)
      req.enable()
    }

    private val fileToUnits = HashMap[String, HashSet[ReferenceType]]()
    private val process = vm.process();
    private val monitor = mode match {
      case VmAttach(_, _) => Nil
      case VmStart(_) => List(new MonitorOutput(process.getErrorStream()),
        new MonitorOutput(process.getInputStream()))
    }
    private val savedObjects = new HashMap[Long, ObjectReference]()

    def start() {
      evtQ.start()
      monitor.map{_.start()}
    }

    def remember(value: Value): Value = {
      value match {
        case v: ObjectReference => remember(v)
        case _ => value
      }
    }

    def remember(v: ObjectReference): ObjectReference = {
      savedObjects(v.uniqueID) = v
      v
    }

    def resume() {
      vm.resume()
    }

    def newStepRequest(thread: ThreadReference, stride: Int, depth: Int) {
      erm.deleteEventRequests(erm.stepRequests)
      val request = erm.createStepRequest(
        thread,
        stride,
        depth)
      request.addCountFilter(1)
      request.enable()
      vm.resume()
    }

    def setBreakpoint(file: CanonFile, line: Int): Boolean = {
      val locs = locations(file, line)
      if (!locs.isEmpty) {
        for (loc <- locs) {
          val request = erm.createBreakpointRequest(loc)
          request.setSuspendPolicy(EventRequest.SUSPEND_ALL)
          request.enable();
        }
        project.bgMessage("Resolved breakpoint at: " + file + " : " + line)
        true
      } else {
        false
      }
    }

    def clearAllBreakpoints() {
      erm.deleteAllBreakpoints()
    }

    def clearBreakpoints(bps: Iterable[Breakpoint]) {
      for (bp <- bps) {
        for (
          req <- erm.breakpointRequests();
          pos <- locToPos(req.location())
        ) {
          if (pos == bp.pos) {
            req.disable()
          }
        }
      }
    }

    def typeAdded(t: ReferenceType) {
      try {
        val key = t.sourceName
        val types = fileToUnits.get(key).getOrElse(HashSet[ReferenceType]())
        types += t
        fileToUnits(key) = types
        tryPendingBreaksForSourcename(key)
      } catch {
        case e: AbsentInformationException =>
          println("No location information available for: " + t.name())
      }
    }

    def initLocationMap() = {
      for (t <- vm.allClasses) {
        typeAdded(t)
      }
    }

    def locations(file: CanonFile, line: Int): List[Location] = {
      val buf = ListBuffer[Location]()
      val key = file.getName
      for (types <- fileToUnits.get(key)) {
        for (t <- types) {
          for (m <- t.methods()) {
            try { buf ++= m.locationsOfLine(line) } catch {
              case e: AbsentInformationException =>
            }
          }
          try { buf ++= t.locationsOfLine(line) } catch {
            case e: AbsentInformationException =>
          }
        }
      }
      buf.toList
    }

    def threadById(id: Long): Option[ThreadReference] = {
      vm.allThreads().find(t => t.uniqueID == id)
    }

    // Helper as Value.toString doesn't give
    // us what we want...
    def valueSummary(value: Value): String = {
      value match {
        case v: BooleanValue => v.value().toString()
        case v: ByteValue => v.value().toString()
        case v: CharValue => "'" + v.value().toString() + "'"
        case v: DoubleValue => v.value().toString()
        case v: FloatValue => v.value().toString()
        case v: IntegerValue => v.value().toString()
        case v: LongValue => v.value().toString()
        case v: ShortValue => v.value().toString()
        case v: VoidValue => "void"
        case v: StringReference => "\"" + v.value().toString() + "\""
        case v: ArrayReference => {
          "Array[" + v.getValues().take(3).map(valueSummary).mkString(", ") + "]"
        }
        case v: ObjectReference => {
          val tpe = v.referenceType()
          if (tpe.name().matches("^scala\\.runtime\\.[A-Z][a-z]+Ref$")) {
            val elemField = tpe.fieldByName("elem")
            valueSummary(v.getValue(elemField))
          } else "Instance of " + lastNameComponent(v.referenceType().name())
        }
        case _ => "NA"
      }
    }

    private def lastNameComponent(s: String): String = {
      "^.*?\\.([^\\.]+)$".r.findFirstMatchIn(s) match {
        case Some(m) => m.group(1)
        case None => s
      }
    }

    private def makeFields(
      tpeIn: ReferenceType,
      obj: ObjectReference): List[DebugObjectField] = {
      tpeIn match {
        case tpeIn: ClassType => {
          var fields = List[DebugObjectField]()
          var tpe = tpeIn
          while (tpe != null) {
            var i = -1
            fields = tpe.fields().map { f =>
              i += 1
              val value = obj.getValue(f)
              DebugObjectField(
                i, f.name(),
                f.typeName(),
                valueSummary(value))
            }.toList ++ fields
            tpe = tpe.superclass
          }
          fields
        }
        case _ => List()
      }
    }

    private def fieldByName(obj: ObjectReference, name: String): Option[Field] = {
      val tpeIn = obj.referenceType
      tpeIn match {
        case tpeIn: ClassType => {
          var result: Option[Field] = None
          var tpe = tpeIn
          while (tpe != null && result.isEmpty) {
            for (f <- tpe.fields()) {
              if (f.name() == name) result = Some(f)
            }
            tpe = tpe.superclass
          }
          result
        }
        case _ => None
      }
    }

    def makeDebugObj(value: ObjectReference): DebugObjectReference = {
      DebugObjectReference(
        valueSummary(value),
        makeFields(value.referenceType(), value),
        value.referenceType().name(),
        value.uniqueID())
    }

    def makeDebugStr(value: StringReference): DebugStringReference = {
      DebugStringReference(
        valueSummary(value),
        makeFields(value.referenceType(), value),
        value.referenceType().name(),
        value.uniqueID())
    }

    def makeDebugArr(value: ArrayReference): DebugArrayReference = {
      DebugArrayReference(
        value.length,
        value.referenceType().name,
        value.referenceType().asInstanceOf[ArrayType].componentTypeName(),
        value.uniqueID)
    }

    def makeDebugPrim(value: PrimitiveValue): DebugPrimitiveValue = DebugPrimitiveValue(
      valueSummary(value),
      value.`type`().name())

    def makeStackFrame(index: Int, frame: StackFrame): DebugStackFrame = {
      val locals = ignoreErr({
        frame.visibleVariables.zipWithIndex.map {
          case (v, i) =>
            DebugStackLocal(i, v.name, v.typeName(),
              valueSummary(frame.getValue(v)))
        }.toList
      }, List())

      val numArgs = ignoreErr(frame.getArgumentValues().length, 0)
      val methodName = ignoreErr(frame.location.method().name(), "Method")
      val className = ignoreErr(frame.location.declaringType().name(), "Class")
      val pcLocation = locToPos(frame.location).getOrElse(
        SourcePosition(
          CanonFile(
            frame.location.sourcePath()),
          frame.location.lineNumber))
      val thisObjId = ignoreErr(remember(frame.thisObject()).uniqueID, -1)
      DebugStackFrame(index, locals, numArgs, className,
        methodName, pcLocation, thisObjId)
    }

    def makeDebugNull(): DebugNullValue = DebugNullValue("Null")

    def makeDebugValue(value: Value): DebugValue = {
      if (value == null) makeDebugNull()
      else {
        value match {
          case v: ArrayReference => makeDebugArr(v)
          case v: StringReference => makeDebugStr(v)
          case v: ObjectReference => makeDebugObj(v)
          case v: PrimitiveValue => makeDebugPrim(v)
        }
      }
    }

    def valueForName(thread: ThreadReference, name: String): Option[DebugValue] = {
      val stackFrame = thread.frame(0)
      val objRef = stackFrame.thisObject();
      if (name == "this") {
        Some(makeDebugValue(remember(objRef)))
      } else {
        stackValueNamed(thread, name).orElse(
          fieldByName(objRef, name).flatMap { f =>
            Some(objRef.getValue(f))
          }).map { v =>
            makeDebugValue(remember(v))
          }
      }
    }

    def valueForId(objectId: Long): Option[DebugValue] = {
      savedObjects.get(objectId).map {
        obj => makeDebugValue(obj)
      }
    }

    def valueForField(objectId: Long, name: String): Option[DebugValue] = {
      (for (
        obj <- savedObjects.get(objectId);
        f <- fieldByName(obj, name)
      ) yield {
        remember(obj.getValue(f))
      }).map(makeDebugValue(_))
    }

    def valueForStackVar(
      thread: ThreadReference, frame: Int, offset: Int): Option[DebugValue] = {
      if (thread.frameCount > frame &&
        thread.frame(frame).visibleVariables.length > offset) {
        val stackFrame = thread.frame(frame)
        val value = stackFrame.getValue(stackFrame.visibleVariables.get(offset))
        Some(makeDebugValue(remember(value)))
      } else None
    }

    def valueForIndex(objectId: Long, index: Int): Option[DebugValue] = {
      (savedObjects.get(objectId) match {
        case Some(arr: ArrayReference) => Some(remember(arr.getValue(index)))
        case None => None
      }).map(makeDebugValue(_))
    }

    def backtrace(thread: ThreadReference, index: Int, count: Int): DebugBacktrace = {
      val frames = ListBuffer[DebugStackFrame]()
      var i = index
      while (i < thread.frameCount && (count == -1 || i < count)) {
        val stackFrame = thread.frame(i)
        frames += makeStackFrame(i, thread.frame(i))
        i += 1
      }
      DebugBacktrace(frames.toList, thread.uniqueID(), thread.name())
    }

    private def stackValueNamed(thread: ThreadReference, name: String): Option[Value] = {
      var result: Option[Value] = None
      var i = 0
      while (result.isEmpty && i < thread.frameCount) {
        val stackFrame = thread.frame(i)
        val visVars = stackFrame.visibleVariables()
        visVars.find(_.name() == name) match {
          case Some(visibleVar) => {
            result = Some(stackFrame.getValue(visibleVar))
          }
          case _ =>
        }
        i += 1
      }
      result
    }

    def dispose() = try {
      evtQ.finished = true
      vm.dispose()
    } catch {
      case e: VMDisconnectedException => {}
    }
  }

  private class VMEventManager(val eventQueue: EventQueue) extends Thread {
    @volatile var finished = false
    override def run() {
      try {
        do {
          val eventSet = eventQueue.remove();
          val it = eventSet.eventIterator()
          while (it.hasNext()) {
            val evt = it.nextEvent();
            evt match {
              case e: VMDisconnectEvent => {
                finished = true
              }
              case e: ClassPrepareEvent => {
                withVM { vm =>
                  vm.typeAdded(e.referenceType())
                }
                eventSet.resume()
              }
              case _ => {}
            }
            actor { DebugManager.this ! evt }
          }
        } while (!finished);
      } catch {
        case t: Throwable => t.printStackTrace();
      }
    }
  }

  private class MonitorOutput(val inStream: InputStream) extends Thread {
    val in = new InputStreamReader(inStream)
    override def run() {
      try {
        var i = 0;
        val buf = new Array[Char](512);
        i = in.read(buf, 0, buf.length)
        while (i >= 0) {
          project ! AsyncEvent(toWF(DebugOutputEvent(new String(buf, 0, i))))
          i = in.read(buf, 0, buf.length)
        }
      } catch {
        case t: Throwable => {
          t.printStackTrace();
        }
      }
    }
  }
}

