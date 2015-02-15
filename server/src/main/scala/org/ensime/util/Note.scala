package org.ensime.util

sealed trait NoteSeverity
case object NoteError extends NoteSeverity
case object NoteWarn extends NoteSeverity
case object NoteInfo extends NoteSeverity
object NoteSeverity {
  def apply(severity: Int) = severity match {
    case 2 => NoteError
    case 1 => NoteWarn
    case 0 => NoteInfo
  }
}

case class Note(
  file: String,
  msg: String,
  severity: NoteSeverity,
  beg: Int,
  end: Int,
  line: Int,
  col: Int)
