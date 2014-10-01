package org.ensime.util

case class NoteList(full: Boolean, notes: Iterable[Note])

case class Note(file: String, msg: String, severity: Int, beg: Int, end: Int, line: Int, col: Int) {
  def friendlySeverity = severity match {
    case 2 => 'error
    case 1 => 'warn
    case 0 => 'info
  }
}
