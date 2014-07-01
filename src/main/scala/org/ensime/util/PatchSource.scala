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

  def main(args: Array[String]) {
    assert(
      applyOperations("abc", List(PatchReplace(0, 1, ""))) == "bc")
    assert(
      applyOperations("abc", List(PatchDelete(0, 1))) == "bc")
    assert(
      applyOperations("abc", List(
        PatchDelete(0, 1),
        PatchInsert(1, "z"),
        PatchDelete(2, 3))) == "zb")
    assert(
      applyOperations("hello there", List(PatchReplace(0, 6, ""))) == "there")
    assert(
      applyOperations("hello there", List(PatchInsert(0, "zz"))) == "zzhello there")
    assert(applyOperations("", List(PatchInsert(0, "moose"))) == "moose")
    assert(applyOperations("abcde", List(
      PatchReplace(0, 3, "z"),
      PatchReplace(3, 5, "q")
    )) == "zq")
    assert(
      applyOperations("", List(PatchInsert(0, "darling!\n"))) == "darling!\n")
    assert(
      applyOperations("\n", List(PatchInsert(1, "darling!\n"))) == "\ndarling!\n")
  }

}
