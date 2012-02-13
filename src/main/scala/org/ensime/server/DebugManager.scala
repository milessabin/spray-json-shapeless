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
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import org.ensime.debug.ProjectDebugInfo
import org.ensime.config.ProjectConfig
import org.ensime.model.Breakpoint
import org.ensime.model.BreakpointList
import org.ensime.model.SourcePosition
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
case class DebugStopVMReq()
case class DebugNextReq(threadId: Long)
case class DebugStepReq(threadId: Long)
case class DebugStepOutReq(threadId: Long)
case class DebugBreakReq(file: String, line: Int)
case class DebugClearBreakReq(file: String, line: Int)
case class DebugClearAllBreaksReq()
case class DebugContinueReq()
case class DebugListBreaksReq()

abstract class DebugEvent
case class DebugStepEvent(threadId: Long, pos: SourcePosition) extends DebugEvent
case class DebugBreakEvent(threadId: Long, pos: SourcePosition) extends DebugEvent
case class DebugVMDeathEvent() extends DebugEvent
case class DebugVMStartEvent() extends DebugEvent
case class DebugVMDisconnectEvent() extends DebugEvent
case class DebugExceptionEvent(e: String) extends DebugEvent
case class DebugThreadStartEvent(threadId: Long) extends DebugEvent
case class DebugThreadDeathEvent(threadId: Long) extends DebugEvent

class DebugManager(project: Project, protocol: ProtocolConversions, config: ProjectConfig) extends Actor {

  import protocol._

  def locToPos(loc: Location): Option[SourcePosition] = {
    val set = sourceMap(loc.sourceName())
    if (set.size > 1) {
      System.err.println("Warning, ambiguous source name: " +
        loc.sourceName())
    }
    set.headOption.map(f => SourcePosition(f, loc.lineNumber))
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

  def vmOptions(): List[String] = {
    List("-classpath", config.debugClasspath)
  }

  private var maybeVM: Option[VM] = None

  private def handleWithVM(action: (VM => Unit)) = {
    (for (vm <- maybeVM) yield {
    }).getOrElse {
      System.err.println("No VM under debug!")
    }
  }
  private def handleRPCWithVM(callId: Int)(action: (VM => Unit)) = {
    (for (vm <- maybeVM) yield {
      action(vm)
    }).getOrElse {
      project ! RPCResultEvent(toWF(false), callId)
      System.err.println("No VM under debug!")
    }
  }
  private def handleRPCWithVMAndThread(callId: Int, threadId: Long)(action: ((VM, ThreadReference) => Unit)) = {
    (for (vm <- maybeVM) yield {
      (for (thread <- vm.threadById(threadId)) yield {
        action(vm, thread)
      }).getOrElse {
        System.err.println("Couldn't find thread: " + threadId)
        project ! RPCResultEvent(toWF(false), callId)
      }
    }).getOrElse {
      System.err.println("No VM under debug!")
      project ! RPCResultEvent(toWF(false), callId)
    }
  }

  def act() {
    loop {
      try {
        receive {
          case DebuggerShutdownEvent => {
            for (vm <- maybeVM) {
              vm.dispose()
            }
            exit('stop)
          }
          case evt: com.sun.jdi.event.Event => {
            evt match {
              case e: VMStartEvent => {
                for (vm <- maybeVM) {
                  vm.initLocationMap()
                }
                project ! AsyncEvent(toWF(DebugVMStartEvent()))
              }
              case e: VMDeathEvent => {
                maybeVM = None
                project ! AsyncEvent(toWF(DebugVMDeathEvent()))
              }
              case e: VMDisconnectEvent => {
                maybeVM = None
                project ! AsyncEvent(toWF(DebugVMDisconnectEvent()))
              }
              case e: StepEvent => {
                (for (pos <- locToPos(e.location())) yield {
                  project ! AsyncEvent(toWF(DebugStepEvent(
                    e.thread().uniqueID(), pos)))
                }) getOrElse {
                  System.err.println("Step position not found: " + 
		    e.location())
                }
              }
              case e: BreakpointEvent => {
                (for (pos <- locToPos(e.location())) yield {
                  project ! AsyncEvent(toWF(DebugBreakEvent(
                  e.thread().uniqueID(), pos)))
                }) getOrElse {
                  System.err.println("Break position not found: " + 
		    e.location())
                }
              }
              case e: ExceptionEvent => {
                project ! AsyncEvent(toWF(DebugExceptionEvent(e.toString)))
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
                for (vm <- maybeVM) {
                  vm.typeAdded(e.referenceType())
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
              req match {
                case DebugStartVMReq(commandLine: String) => {
                  for (vm <- maybeVM) {
                    vm.dispose()
                  }
                  maybeVM = None
                  val connector = Bootstrap.virtualMachineManager().defaultConnector
                  val arguments = connector.defaultArguments()
                  //arguments.get("home").setValue(jreHome);
                  val opts = arguments.get("options").value
                  val allVMOpts = (List(opts) ++ vmOptions).mkString(" ")
                  arguments.get("options").setValue(allVMOpts)
                  arguments.get("main").setValue(commandLine)
                  arguments.get("suspend").setValue("true")
                  //arguments.get("quote").setValue("\"");
                  //arguments.get("vmexec").setValue("java");
                  println("Using Connector: " + connector.name +
                    " : " + connector.description())
                  println("Connector class: " + connector.getClass.getName())
                  println("Debugger VM args: " + allVMOpts)
                  println("Debugger program args: " + commandLine)
                  val vm = connector.launch(arguments)

                  maybeVM = Some(new VM(vm))
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugStopVMReq() => {
                  handleRPCWithVM(callId) { vm =>
                    vm.dispose()
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugContinueReq() => {
                  handleRPCWithVM(callId) { vm =>
                    vm.resume()
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugBreakReq(filepath: String, line: Int) => {
                  val file = CanonFile(filepath)
                  handleRPCWithVM(callId) { vm =>
                    vm.setBreakpoint(file, line)
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugClearBreakReq(filepath: String, line: Int) => {
                  val file = CanonFile(filepath)
                  handleRPCWithVM(callId) { vm =>
                    vm.clearBreakpoint(file, line)
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugClearAllBreaksReq() => {
                  handleRPCWithVM(callId) { vm =>
                    vm.clearAllBreakpoints()
                    project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugListBreaksReq() => {
                  import scala.collection.JavaConversions._
                  handleRPCWithVM(callId) { vm =>
                    val breaks = BreakpointList(
                      vm.activeBreakpoints.toList ++
                        vm.pendingBreakpoints)
                    project ! RPCResultEvent(toWF(breaks), callId)
                  }
                }

                case DebugNextReq(threadId: Long) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      val request = vm.erm.createStepRequest(
                        thread,
                        StepRequest.STEP_LINE,
                        StepRequest.STEP_OVER);
                      request.addCountFilter(1);
                      request.enable();
                      vm.vm.resume();
                      project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugStepReq(threadId: Long) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      val request = vm.erm.createStepRequest(
                        thread,
                        StepRequest.STEP_LINE,
                        StepRequest.STEP_INTO);
                      request.addCountFilter(1);
                      request.enable();
                      vm.vm.resume();
                      project ! RPCResultEvent(toWF(true), callId)
                  }
                }

                case DebugStepOutReq(threadId: Long) => {
                  handleRPCWithVMAndThread(callId, threadId) {
                    (vm, thread) =>
                      val request = vm.erm.createStepRequest(
                        thread,
                        StepRequest.STEP_LINE,
                        StepRequest.STEP_OUT);
                      request.addCountFilter(1);
                      request.enable();
                      vm.vm.resume();
                  }
                }
              }
            } catch {
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

  private class VM(val vm: VirtualMachine) {
      import scala.collection.JavaConversions._

    //    vm.setDebugTraceMode(VirtualMachine.TRACE_ALL)
    val evtQ = new VMEventManager(vm.eventQueue())
    evtQ.start()
    val erm = vm.eventRequestManager();
    {
      val req = erm.createClassPrepareRequest()
      req.setSuspendPolicy(EventRequest.SUSPEND_NONE)
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
    private val pendingBreaksBySourceName = 
    HashMap[String, HashSet[Breakpoint]]()
    val activeBreakpoints = HashSet[Breakpoint]()
    private val process = vm.process();
    private val outputMon = new MonitorOutput(process.getErrorStream());
    outputMon.start
    private val inputMon = new MonitorOutput(process.getInputStream());
    inputMon.start

    def pendingBreakpoints: List[Breakpoint] = {
      pendingBreaksBySourceName.values.flatten.toList
    }

    def resume() {
      println("VM: resume")
      vm.resume()
    }

    def clearBreakpoint(file: CanonFile, line: Int) {
      for(bps <- pendingBreaksBySourceName.get(file.getName)){
	bps.retain{ bp => bp.pos.file != file || bp.pos.line != line }
      }
      val toRemove = activeBreakpoints.filter{ bp => 
	bp.pos.file == file && bp.pos.line == line}
      for(bp <- toRemove){
	for(req <- erm.breakpointRequests()){
	  val pos = locToPos(req.location())
	  if(pos == bp.pos) req.disable()
	}
      }
      activeBreakpoints --= toRemove
    }

    def clearAllBreakpoints() {
      pendingBreaksBySourceName.clear()
      activeBreakpoints.clear()
      erm.deleteAllBreakpoints()
    }

    def setBreakpoint(file: CanonFile, line: Int) {
      (for (loc <- location(file, line)) yield {
        println("Setting breakpoint at: " + loc)
        val request = erm.createBreakpointRequest(loc)
        request.setSuspendPolicy(EventRequest.SUSPEND_ALL)
        request.enable();
        removePendingBreakpoint(file, line)
        activeBreakpoints += Breakpoint(SourcePosition(file, line))
        project.bgMessage("Set breakpoint at: " + file + " : " + line)
        true
      }).getOrElse {
        project.bgMessage("Location not loaded. Set pending breakpoint.")
        addPendingBreakpoint(file, line)
        false
      }
    }

    def typeAdded(t: ReferenceType) {
      try {
        val key = t.sourceName
        val types = fileToUnits.get(key).getOrElse(HashSet[ReferenceType]())
        types += t
        fileToUnits(key) = types
        val pending = HashMap() ++ pendingBreaksBySourceName
        for (breaks <- pending.get(key)) {
          for (bp <- breaks) {
            setBreakpoint(bp.pos.file, bp.pos.line)
          }
        }
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

    def location(file: CanonFile, line: Int): Option[Location] = {
      val buf = ListBuffer[Location]()
      val key = file.getName
      for (types <- fileToUnits.get(key)) {
        for (t <- types) {
          for (m <- t.methods()) {
            try { buf ++= m.locationsOfLine(line) } catch {
              case _ =>
                print("no debug info for: " + m)
            }
          }
          try { buf ++= t.locationsOfLine(line) } catch {
            case _ =>
              print("no debug info for: " + t)
          }
        }
      }
      println("Found locations: " + buf)
      buf.headOption
    }

    def addPendingBreakpoint(file: CanonFile, line: Int) {
      val key = file.getName
      val breaks = pendingBreaksBySourceName.getOrElse(key, HashSet())
      breaks.add(Breakpoint(SourcePosition(file, line)))
      pendingBreaksBySourceName(key) = breaks
      println("Pending for key: " + key + " : " + breaks)
    }

    def removePendingBreakpoint(file: CanonFile, line: Int) {
      val key = file.getName
      if (pendingBreaksBySourceName.contains(key)) {
        val breaks = pendingBreaksBySourceName.getOrElse(key, HashSet())
        breaks.remove(Breakpoint(SourcePosition(file, line)))
        pendingBreaksBySourceName(key) = breaks
      }
    }

    def threadById(id: Long): Option[ThreadReference] = {
      vm.allThreads().find(t => t.uniqueID == id)
    }

    def dispose() {
      evtQ.finished = true
      vm.dispose()
    }

  }

  private class VMEventManager(val eventQueue: EventQueue) extends Thread {
    var finished = false
    override def run() {
      try {
        do {
          val eventSet = eventQueue.remove();
          val it = eventSet.eventIterator()
          while (it.hasNext()) {
            val evt = it.nextEvent();
            println("VM Event:" + evt.toString)
            evt match {
              case e: VMDisconnectEvent => {
                finished = true
              }
              case _ => {

              }
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
    val out = new OutputStreamWriter(System.out);
    override def run() {
      try {
        var i = 0;
        val buf = new Array[Char](256);
        i = in.read(buf, 0, buf.length)
        while (i >= 0) {
          out.write(buf, 0, i);
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

