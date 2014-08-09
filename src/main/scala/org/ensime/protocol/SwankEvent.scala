package org.ensime.protocol

import org.ensime.util.NoteList

/**
 * A asynchronous swank protocol event
 */
sealed trait SwankEvent

case class SendBackgroundMessageEvent(code: Int, detail: Option[String]) extends SwankEvent
case object AnalyzerReadyEvent extends SwankEvent
case object FullTypeCheckCompleteEvent extends SwankEvent
case object IndexerReadyEvent extends SwankEvent

case object ClearAllJavaNotesEvent extends SwankEvent
case object ClearAllScalaNotesEvent extends SwankEvent
case class NewScalaNotesEvent(noteList: NoteList) extends SwankEvent
case class NewJavaNotesEvent(noteList: NoteList) extends SwankEvent

