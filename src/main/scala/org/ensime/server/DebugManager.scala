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
case class DebugLocateNameReq(threadId: Long, name: String)
case class DebugValueReq(loc: DebugLocation)
case class DebugToStringReq(threadId: Long, loc: DebugLocation)
case class DebugSetValueReq(loc: DebugLocation, newValue: String)
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
  threadId: Long, threadName: String,
  pos: Option[SourcePosition]) extends DebugEvent
case class DebugThreadStartEvent(threadId: Long) extends DebugEvent
case class DebugThreadDeathEvent(threadId: Long) extends DebugEvent
case class DebugOutputEvent(out: String) extends DebugEvent

class DebugManager(project: Project, indexer: Actor,
  protocol: ProtocolConversions,
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

  // Map unqualified file names to sets of fully qualified paths.
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

  def disconnectDebugVM() {
    withVM { vm =>
      vm.dispose()
    }
    moveActiveBreaksToPending()
    maybeVM = None
    project ! AsyncEvent(toWF(DebugVMDisconnectEvent()))
  }

  def vmOptions(): List[String] = {
    List("-classpath", "\"" + config.debugClasspath + "\"")
  }

  private var maybeVM: Option[VM] = None

  def withVM[T](action: (VM => T)): Option[T] = {
    maybeVM.synchronized {
      try {
        for (vm <- maybeVM) yield {
          action(vm)
        }
      } catch {
        case e: AbsentInformationException => {
          e.printStackTrace()
          None
        }
        case e: VMDisconnectedException =>
          {
            System.err.println("Attempted interaction with disconnected VM:")
            e.printStackTrace()
            disconnectDebugVM()
            None
          }
        case e: Throwable =>
          {
            e.printStackTrace()
            None
          }
      }
    }
  }

  private def handleRPCWithVM(callId: Int)(action: (VM => Unit)) = {
    withVM { vm =>
      action(vm)
    }.getOrElse {
      project ! RPCResultEvent(toWF(false), callId)
      System.err.println("Could not access debug VM.")
    }
  }

  private def handleRPCWithVMAndThread(callId: Int,
    threadId: Long)(action: ((VM, ThreadReference) => Unit)) = {
    withVM { vm =>
      (for (thread <- vm.threadById(threadId)) yield {
        action(vm, thread)
      }).getOrElse {
        System.err.println("Couldn't find thread: " + threadId)
        project ! RPCResultEvent(toWF(false), callId)
      }
    }.getOrElse {
      System.err.println("Could not access debug VM")
      project ! RPCResultEvent(toWF(false), callId)
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
          case e: VMDisconnectedException => disconnectDebugVM()
          case evt: com.sun.jdi.event.Event => {
            evt match {
              case e: VMStartEvent => {
                withVM { vm =>
                  vm.initLocationMap()
                }
                project ! AsyncEvent(toWF(DebugVMStartEvent()))
              }
              case e: VMDeathEvent => disconnectDebugVM()
              case e: VMDisconnectEvent => disconnectDebugVM()
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
                  e.thread().name,
                  if (e.catchLocation() != null) locToPos(e.catchLocation()) else None)))
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

          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              def handleStartupFailure(e: Exception): Unit = {
                maybeVM = None
                e.printStackTrace()
                val message = e.toString
                project ! RPCResultEvent(toWF(DebugVmError(1, message.toString)), callId)
              }

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
                      println("Couldn't start VM")
                      handleStartupFailure(e)
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
                      println("Couldn't attach to target VM.")
                      handleStartupFailure(e)
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

                case DebugLocateNameReq(threadId: Long, name: String) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      vm.locationForName(thread, name) match {
                        case Some(loc) =>
                          project ! RPCResultEvent(toWF(loc), callId)
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
                case DebugValueReq(location) => {
                  handleRPCWithVM(callId) {
                    (vm) =>
                      vm.debugValueAtLocation(location).map(toWF) match {
                        case Some(payload) => project ! RPCResultEvent(payload, callId)
                        case None => project ! RPCResultEvent(toWF(false), callId)
                      }
                  }
                }
                case DebugToStringReq(threadId, location) => {
                  handleRPCWithVM(callId) {
                    (vm) =>
                      vm.debugValueAtLocationToString(threadId, location).map(toWF) match {
                        case Some(payload) => project ! RPCResultEvent(payload, callId)
                        case None => project ! RPCResultEvent(toWF(false), callId)
                      }
                  }
                }
                case DebugSetValueReq(location, newValue) => {
                  handleRPCWithVM(callId) { vm =>
                    location match {
                      case DebugStackSlot(threadId, frame, offset) => vm.threadById(threadId) match {
                        case Some(thread) => {
                          val status = vm.setStackVar(thread, frame, offset, newValue)
                          project ! RPCResultEvent(toWF(status), callId)
                        }
                        case _ =>
                      }
                      case _ => {
                        System.err.println("Unsupported location type for debug-set-value.")
                        project ! RPCResultEvent(toWF(false), callId)
                      }
                    }
                  }
                }
              }
            } catch {
              case e: Throwable =>
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
        case e: Throwable =>
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
          // if the remote VM has been started in suspended state, we need to nudge it
          // if the remote VM has been started in running state, this call seems to be a no-op
          vm.resume()
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

      // Re-index the classfiles on disk so our mappings are up
      // to date.
      indexer ! ReindexClassFilesReq(
        List(config.target, config.testTarget).flatten)

      evtQ.start()
      monitor.map { _.start() }
    }

    def dispose() = try {
      evtQ.finished = true
      vm.dispose()
      monitor.map { _.finished = true }
    } catch {
      case e: VMDisconnectedException => {}
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
        project.bgMessage("Resolved breakpoint at: " + file + " : " + line)
        project.bgMessage("Installing breakpoint at locations: " + locs)
        for (loc <- locs) {
          val request = erm.createBreakpointRequest(loc)
          request.setSuspendPolicy(EventRequest.SUSPEND_ALL)
          request.enable();
        }
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

    def locations(file: CanonFile, line: Int): Set[Location] = {

      // Group locations by file and line
      case class LocationClass(loc: Location) {
        override def equals(that: Any): Boolean = that match {
          case that: Location => {
            loc.sourcePath == that.sourcePath &&
              loc.sourceName == that.sourceName &&
              loc.lineNumber == that.lineNumber
          }
          case _ => false
        }
        override def hashCode: Int = loc.lineNumber.hashCode ^ loc.sourceName.hashCode
      }


      val buf = HashSet[LocationClass]()
      val key = file.getName
      for (types <- fileToUnits.get(key)) {
        for (t <- types) {
          for (m <- t.methods()) {
            try { buf ++= m.locationsOfLine(line).map(LocationClass.apply) } catch {
              case e: AbsentInformationException =>
            }
          }
          try { buf ++= t.locationsOfLine(line).map(LocationClass.apply) } catch {
            case e: AbsentInformationException =>
          }
        }
      }
      buf.map(_.loc).toSet
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
      obj: ObjectReference): List[DebugClassField] = {
      tpeIn match {
        case tpeIn: ClassType => {
          var fields = List[DebugClassField]()
          var tpe = tpeIn
          while (tpe != null) {
            var i = -1
            fields = tpe.fields().map { f =>
              i += 1
              val value = obj.getValue(f)
              DebugClassField(
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

    private def makeDebugObj(value: ObjectReference): DebugObjectInstance = {
      DebugObjectInstance(
        valueSummary(value),
        makeFields(value.referenceType(), value),
        value.referenceType().name(),
        value.uniqueID())
    }

    private def makeDebugStr(value: StringReference): DebugStringInstance = {
      DebugStringInstance(
        valueSummary(value),
        makeFields(value.referenceType(), value),
        value.referenceType().name(),
        value.uniqueID())
    }

    private def makeDebugArr(value: ArrayReference): DebugArrayInstance = {
      DebugArrayInstance(
        value.length,
        value.referenceType().name,
        value.referenceType().asInstanceOf[ArrayType].componentTypeName(),
        value.uniqueID)
    }

    private def makeDebugPrim(value: PrimitiveValue): DebugPrimitiveValue = DebugPrimitiveValue(
      valueSummary(value),
      value.`type`().name())

    private def makeDebugNull(): DebugNullValue = DebugNullValue("Null")

    private def makeDebugValue(value: Value): DebugValue = {
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

    def locationForName(thread: ThreadReference, name: String): Option[DebugLocation] = {
      val stackFrame = thread.frame(0)
      val objRef = stackFrame.thisObject();
      if (name == "this") {
        Some(DebugObjectReference(remember(objRef).uniqueID))
      } else {
        stackSlotForName(thread, name).map({ slot =>
          DebugStackSlot(thread.uniqueID, slot._1, slot._2)
        }).orElse(
          fieldByName(objRef, name).flatMap { f =>
            Some(DebugObjectField(objRef.uniqueID, f.name))
          })
      }
    }

    private def valueAtLocation(location: DebugLocation): Option[Value] = {
      location match {
        case DebugObjectReference(objectId) =>
          valueForId(objectId)
        case DebugObjectField(objectId, name) =>
          valueForField(objectId, name)
        case DebugArrayElement(objectId, index) =>
          valueForIndex(objectId, index)
        case DebugStackSlot(threadId, frame, offset) =>
          threadById(threadId) match {
            case Some(thread) =>
              valueForStackVar(thread, frame, offset)
            case None => None
          }
      }
    }

    def debugValueAtLocation(location: DebugLocation): Option[DebugValue] = {
      valueAtLocation(location).map(makeDebugValue)
    }

    private def callMethod(thread: ThreadReference, obj: ObjectReference, name: String, signature: String, args: java.util.List[Value]): Option[Value] = {
      if (!vm.canBeModified) {
        println("Sorry, this debug VM is read-only.")
        None
      } else {
        println("DebugManager.callMethod(obj = " + obj + " of type " + obj.referenceType + ", name = " + name + ", signature = " + signature + ", args = " + args)
        // println("obj.referenceType.allMethods = " + obj.referenceType.allMethods.toList.map(m => "name = " + m.name + ", signature = " + m.signature))
        obj.referenceType.methodsByName("toString", "()Ljava/lang/String;").headOption match {
          case Some(m) => {
            println("Invoking: " + m)
            Some(obj.invokeMethod(thread, m, args, ObjectReference.INVOKE_SINGLE_THREADED))
          }
          case other => {
            System.err.println("toString method not found: " + other)
            None
          }
        }
      }
    }

    def debugValueAtLocationToString(threadId: Long, location: DebugLocation): Option[String] = {
      valueAtLocation(location) match {
        case Some(arr: ArrayReference) =>
          val quantifier = if (arr.length == 1) "element" else "elements" // TODO: replace with something less naive
          Some("<array of " + arr.length + " " + quantifier + ">")
        case Some(str: StringReference) =>
          Some(str.value)
        case Some(obj: ObjectReference) => {
          threadById(threadId) flatMap { thread =>
            callMethod(thread, obj, "toString", "()Ljava/lang/String;", new java.util.Vector()) match {
              case Some(v: StringReference) => {
                Some(v.value.toString())
              }
              case Some(null) => Some("null")
              case _ => None
            }
          }
        }
        case Some(value) => Some(valueSummary(value))
        case _ => System.out.println("No value found at location."); None
      }
    }

    private def valueForId(objectId: Long): Option[ObjectReference] = {
      savedObjects.get(objectId)
    }

    private def valueForField(objectId: Long, name: String): Option[Value] = {
      for (
        obj <- savedObjects.get(objectId);
        f <- fieldByName(obj, name)
      ) yield {
        remember(obj.getValue(f))
      }
    }

    private def valueForIndex(objectId: Long, index: Int): Option[Value] = {
      savedObjects.get(objectId) match {
        case Some(arr: ArrayReference) => Some(remember(arr.getValue(index)))
        case _ => None
      }
    }

    private def valueForStackVar(
      thread: ThreadReference, frame: Int, offset: Int): Option[Value] = {
      if (thread.frameCount > frame &&
        thread.frame(frame).visibleVariables.length > offset) {
        val stackFrame = thread.frame(frame)
        val value = stackFrame.getValue(stackFrame.visibleVariables.get(offset))
        Some(remember(value))
      } else None
    }

    type StackSlot = (Int, Int) // frame, offset
    private def stackSlotForName(thread: ThreadReference, name: String): Option[StackSlot] = {
      var result: Option[(Int, Int)] = None
      var i = 0
      while (result.isEmpty && i < thread.frameCount) {
        val stackFrame = thread.frame(i)
        val visVars = stackFrame.visibleVariables()
        var j = 0
        while (j < visVars.length) {
          if (visVars(j).name == name) {
            result = Some((i, j))
          }
          j += 1
        }
        i += 1
      }
      result
    }

    private def makeStackFrame(index: Int, frame: StackFrame): DebugStackFrame = {
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

    private def mirrorFromString(tpe: Type, toMirror: String): Option[Value] = {
      val s = toMirror.trim
      if (s.length > 0) {
        tpe match {
          case tpe: BooleanType => Some(vm.mirrorOf(s.toBoolean))
          case tpe: ByteType => Some(vm.mirrorOf(s.toByte))
          case tpe: CharType => Some(vm.mirrorOf(s(0)))
          case tpe: DoubleType => Some(vm.mirrorOf(s.toDouble))
          case tpe: FloatType => Some(vm.mirrorOf(s.toFloat))
          case tpe: IntegerType => Some(vm.mirrorOf(s.toInt))
          case tpe: LongType => Some(vm.mirrorOf(s.toLong))
          case tpe: ShortType => Some(vm.mirrorOf(s.toShort))
          case tpe: ReferenceType if tpe.name == "java.lang.String" =>
            {
              if (s.startsWith("\"") && s.endsWith("\"")) {
                Some(vm.mirrorOf(s.substring(1, s.length - 1)))
              } else Some(vm.mirrorOf(s))
            }
          case _ => None
        }
      } else None
    }

    def setStackVar(thread: ThreadReference, frame: Int, offset: Int,
      newValue: String): Boolean = {
      if (thread.frameCount > frame &&
        thread.frame(frame).visibleVariables.length > offset) {
        val stackFrame = thread.frame(frame)
        val localVar = stackFrame.visibleVariables.get(offset)
        mirrorFromString(localVar.`type`(), newValue) match {
          case Some(v) => stackFrame.setValue(localVar, v); true
          case None => false
        }
      } else false
    }

  }

  private class VMEventManager(val eventQueue: EventQueue) extends Thread {
    @volatile var finished = false
    override def run() {
      do {
        try {
          val eventSet = eventQueue.remove()
          val it = eventSet.eventIterator()
          while (it.hasNext()) {
            val evt = it.nextEvent()
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
            DebugManager.this ! evt
          }
        } catch {
          case t: VMDisconnectedException =>
            {
              DebugManager.this ! t
              finished = true
            }
          case t: Throwable => {
            t.printStackTrace()
            finished = true
          }
        }
      } while (!finished);

    }
  }

  private class MonitorOutput(val inStream: InputStream) extends Thread {
    val in = new InputStreamReader(inStream)
    @volatile var finished = false
    override def run() {
      try {
        var i = 0;
        val buf = new Array[Char](512);
        i = in.read(buf, 0, buf.length)
        while (!finished && i >= 0) {
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

