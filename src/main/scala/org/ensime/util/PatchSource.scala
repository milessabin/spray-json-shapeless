package org.ensime.util

import scala.reflect.internal.util.{ SourceFile, BatchSourceFile }
import org.ensime.model.PatchOp
import org.ensime.model.PatchInsert
import org.ensime.model.PatchDelete
import org.ensime.model.PatchReplace

object PatchSource {

  def applyOperations(
    s: SourceFile, ops: List[PatchOp]): SourceFile = {
    val result = applyOperations(s.content, ops)
    new BatchSourceFile(s.file, result)
  }

  def applyOperations(
    input: String, ops: List[PatchOp]): String = {
    val chars = new Array[Char](input.length)
    input.getChars(0, input.length, chars, 0)
    val result = applyOperations(chars, ops)
    new String(result)
  }

  def applyOperations(
    input: Array[Char], ops: List[PatchOp]): Array[Char] = {
    val newLen = input.length + netLengthChange(ops)
    val result = new Array[Char](newLen)
    var offset = 0
    var srcCursor = 0
    for (op <- ops) {
      val i = op.start
      val copyLen = i - srcCursor
      System.arraycopy(input, srcCursor, result,
        srcCursor + offset, copyLen)
      srcCursor += copyLen
      op match {
        case PatchInsert(i, text) =>
          text.getChars(0, text.length, result, i + offset)
          offset += text.length
        case PatchReplace(i, j, text) =>
          text.getChars(0, text.length, result, i + offset)
          offset += text.length - (j - i)
          srcCursor += j - i
        case PatchDelete(i, j) =>
          offset -= j - i
          srcCursor += j - i
      }
    }
    val copyLen = input.length - srcCursor
    System.arraycopy(input, srcCursor, result,
      srcCursor + offset, copyLen)
    result
  }

  private def netLengthChange(ops: List[PatchOp]): Int = {
    var offset = 0
    for (op <- ops) {
      op match {
        case PatchInsert(i, text) =>
          offset += text.length
        case PatchReplace(i, j, text) =>
          offset += text.length - (j - i)
        case PatchDelete(i, j) =>
          offset -= j - i
      }
    }
    offset
  }
}
