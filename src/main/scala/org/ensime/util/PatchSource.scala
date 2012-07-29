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

package org.ensime.util
import scala.tools.nsc.util.{ SourceFile, BatchSourceFile }
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
        case PatchInsert(i, text) => {
	  text.getChars(0, text.length, result, i + offset)
          offset += text.length
        }
        case PatchReplace(i, j, text) => {
	  text.getChars(0, text.length, result, i + offset)
          offset += text.length - (j - i)
	  srcCursor += j - i
        }
        case PatchDelete(i, j) => {
          offset -= j - i
	  srcCursor += j - i
        }
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
        case PatchInsert(i, text) => {
          offset += text.length
        }
        case PatchReplace(i, j, text) => {
          offset += text.length - (j - i)
        }
        case PatchDelete(i, j) => {
          offset -= j - i
        }
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
