package org.ensime.util

import org.ensime.api._

object FileEditHelper {

  import scala.tools.refactoring.common.{ Change, NewFileChange, TextChange }

  def fromChange(ch: Change): FileEdit = {
    ch match {
      case ch: TextChange => TextEdit(ch.file.file, ch.from, ch.to, ch.text)
      case ch: NewFileChange => NewFile(ch.file, ch.text)
    }
  }

  def applyEdits(ch: List[TextEdit], source: String): String = {
    (source /: ch.sortBy(-_.to)) { (src, change) =>
      src.substring(0, change.from) + change.text + src.substring(change.to)
    }
  }

}
