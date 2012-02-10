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
import com.sun.jdi.request.StepRequest
import java.io.File
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import org.ensime.debug.ProjectDebugInfo
import org.ensime.config.ProjectConfig
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
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
case class DebugListBreaksReq()

abstract class DebugEvent
case class DebugStepEvent(threadId: Long, loc: (String, Int)) extends DebugEvent
case class DebugBreakEvent(threadId: Long, loc: (String, Int)) extends DebugEvent
case class DebugVMDeathEvent() extends DebugEvent
case class DebugVMStartEvent() extends DebugEvent
case class DebugVMDisconnectEvent() extends DebugEvent
case class DebugExceptionEvent(e: String) extends DebugEvent
case class DebugThreadStartEvent(threadId: Long) extends DebugEvent
case class DebugThreadDeathEvent(threadId: Long) extends DebugEvent

class DebugManager(project: Project, protocol: ProtocolConversions, config: ProjectConfig) extends Actor {

  import protocol._

  //  val debugInfo: ProjectDebugInfo = new ProjectDebugInfo(config)

  def toScalaLoc(loc: Location): (String, Int) = {
    (loc.sourcePath(), loc.lineNumber)
  }

  def vmOptions(): List[String] = {
    List("-cp", config.debugClasspath)
  }

  private var maybeVM: Option[VM] = None

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
                val loc = toScalaLoc(e.location())
                project ! AsyncEvent(toWF(DebugBreakEvent(
                      e.thread().uniqueID(), loc)))
              }
              case e: BreakpointEvent => {
                val loc = toScalaLoc(e.location())
                project ! AsyncEvent(toWF(DebugBreakEvent(
                      e.thread().uniqueID(), loc)))
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
              case e: ClassPrepareEvent => {}
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
                  arguments.get("options").setValue(vmOptions.mkString(" "))
                  arguments.get("main").setValue(commandLine)
                  //arguments.get("suspend").setValue("true");
                  //arguments.get("quote").setValue("\"");
                  //arguments.get("vmexec").setValue("java");
                  val vm = connector.launch(arguments)
                  maybeVM = Some(new VM(vm))
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugStopVMReq() => {
                  for (vm <- maybeVM) {
                    vm.dispose()
                  }
                  maybeVM = None
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugBreakReq(file: String, line: Int) => {
                  for (
                    vm <- maybeVM;
                    loc <- vm.location(file, line)
                  ) {
                    val request = vm.erm.createBreakpointRequest(loc)
                    request.enable();
                  }
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugListBreaksReq() => {
                  import scala.collection.JavaConversions._
                  for (vm <- maybeVM) {
                    val locs = vm.erm.breakpointRequests().map { b =>
		      toScalaLoc(b.location())
                    }
                    project ! RPCResultEvent(toWF(locs),callId)
                  }
                  project ! RPCResultEvent(toWF(false), callId)
                }

                case DebugNextReq(threadId: Long) => {
                  for (
                    vm <- maybeVM;
                    thread <- vm.threadById(threadId)) {
                    val request = vm.erm.createStepRequest(
                      thread,
                      StepRequest.STEP_LINE,
                      StepRequest.STEP_OVER);
                    request.addCountFilter(1);
                    request.enable();
                    vm.vm.resume();
                  }
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugStepReq(threadId: Long) => {
                  for (
                    vm <- maybeVM;
                    thread <- vm.threadById(threadId)
                  ) {
                    val request = vm.erm.createStepRequest(
                      thread,
                      StepRequest.STEP_LINE,
                      StepRequest.STEP_INTO);
                    request.addCountFilter(1);
                    request.enable();
                    vm.vm.resume();
                  }
                  project ! RPCResultEvent(toWF(true), callId)
                }

                case DebugStepOutReq(threadId: Long) => {
                  for (
                    vm <- maybeVM;
                    thread <- vm.threadById(threadId)
                  ) {
                    val request = vm.erm.createStepRequest(
                      thread,
                      StepRequest.STEP_LINE,
                      StepRequest.STEP_OUT);
                    request.addCountFilter(1);
                    request.enable();
                    vm.vm.resume();
                  }
                  project ! RPCResultEvent(toWF(true), callId)
                }

              }
            } catch {
              case e: Exception =>
              {
                System.err.println("Error handling RPC:")
                e.printStackTrace()
                project ! RPCErrorEvent(ErrExceptionInDebugger,
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

    val evtQ = new VMEventManager(vm.eventQueue())
    evtQ.start()

    val erm = vm.eventRequestManager();

    def location(file: String, line: Int): Option[Location] = {
    }

    def threadById(id: Long): Option[ThreadReference] = {
      import scala.collection.JavaConversions._
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
            actor {
              DebugManager.this ! evt
            }
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

