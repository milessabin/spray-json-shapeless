package org.ensime.core

import java.io.File
import org.ensime.model.LineSourcePosition
import org.ensime.util.Note

/** Asynchronous swank protocol event */
sealed trait EnsimeEvent
sealed trait GeneralSwankEvent extends EnsimeEvent
sealed trait DebugEvent extends EnsimeEvent

/** Generic background notification. */
case class SendBackgroundMessageEvent(code: Int, detail: Option[String]) extends GeneralSwankEvent

/** The presentation compiler is ready to accept requests. */
case object AnalyzerReadyEvent extends GeneralSwankEvent

/** The presentation compiler has finished analysing the entire project. */
case object FullTypeCheckCompleteEvent extends GeneralSwankEvent

/** The search engine has finished indexing the classpath. */
case object IndexerReadyEvent extends GeneralSwankEvent

/** The presentation compiler was restarted. Existing `:type-id`s are invalid. */
case object CompilerRestartedEvent extends GeneralSwankEvent

/** The presentation compiler has invalidated all existing notes.  */
case object ClearAllScalaNotesEvent extends GeneralSwankEvent

/** The presentation compiler is providing notes: e.g. errors, warnings. */
case class NewScalaNotesEvent(
  isFull: Boolean,
  notes: List[Note]) extends GeneralSwankEvent

/** The debugged VM has stepped to a new location and is now paused awaiting control. */
case class DebugStepEvent(
  threadId: String,
  threadName: String,
  file: File,
  line: Int) extends DebugEvent

/** The debugged VM has stopped at a breakpoint. */
case class DebugBreakEvent(
  threadId: String,
  threadName: String,
  file: File,
  line: Int) extends DebugEvent

/** The debugged VM has started. */
case object DebugVMStartEvent extends DebugEvent

/** The debugger has disconnected from the debugged VM. */
case object DebugVMDisconnectEvent extends DebugEvent

/** The debugged VM has thrown an exception and is now paused waiting for control. */
case class DebugExceptionEvent(
  exception: Long,
  threadId: String,
  threadName: String,
  file: Option[File],
  line: Option[Int]) extends DebugEvent

/** A new thread has started. */
case class DebugThreadStartEvent(threadId: String) extends DebugEvent

/** A thread has died. */
case class DebugThreadDeathEvent(threadId: String) extends DebugEvent

/** Communicates stdout/stderr of debugged VM to client. */
case class DebugOutputEvent(body: String) extends DebugEvent

