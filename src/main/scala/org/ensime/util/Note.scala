package org.ensime.util

import org.eclipse.jdt.core.compiler.IProblem

case class NoteList(full: Boolean, notes: Iterable[Note])

object Note {

  def apply(prob: IProblem) = {
    new Note(
      prob.getOriginatingFileName.mkString,
      prob.getMessage, if (prob.isError) { 2 } else if (prob.isWarning) { 1 } else { 0 },
      prob.getSourceStart,
      prob.getSourceEnd + 1,
      prob.getSourceLineNumber,
      -1
    )
  }
}

case class Note(file: String, msg: String, severity: Int, beg: Int, end: Int, line: Int, col: Int) {

  def friendlySeverity = severity match {
    case 2 => 'error
    case 1 => 'warn
    case 0 => 'info
  }
}
