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

package org.ensime.indexer
import java.io.File
import org.ensime.util.ClassIterator
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.commons.EmptyVisitor
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MultiMap

class ClassFileIndex {

  case class Op(
    op: String,
    description: String)
  case class MethodBytecode(
    className: String,
    methodSignature: String,
    byteCode: List[Op])

  private val NoFlags = 0

  private val classFilesForSourceName =
    new HashMap[String, HashSet[String]].withDefault(s => HashSet())

  def indexFiles(files: Iterable[File]) {
    ClassIterator.find(files,
      (location, classReader) =>
        classReader.accept(new EmptyVisitor() {
          override def visitSource(source: String, debug: String) {
            classFilesForSourceName(source) += location.getAbsolutePath()
          }
        }, NoFlags))
    println("Finished indexing " + files.size + " classpath files.")
  }

  trait RichClassVisitor extends ClassVisitor {
    type Result
    def result: Option[Result]
  }

  class MethodByteCodeFinder(targetSource: String, targetLine: Int)
    extends EmptyVisitor with RichClassVisitor {
    type Result = List[MethodBytecode]
    private var quit: Boolean = false
    private var className: String = ""
    private val methods: ListBuffer[MethodBytecode] = ListBuffer()
    def result: Option[List[MethodBytecode]] = Some(methods.toList)

    override def visit(version: Int, access: Int,
      name: String, sig: String, superName: String,
      interfaces: Array[String]) {
      className = name
    }

    override def visitSource(source: String, debug: String) {
      if (source != targetSource) {
        quit = true
      }
    }

    override def visitMethod(access: Int,
      name: String,
      description: String,
      signature: String,
      exceptions: Array[String]): MethodVisitor = {
      if (!quit) {
        return new EmptyVisitor() {
          var maxLine = Int.MinValue
          var minLine = Int.MaxValue
          var ops = ListBuffer[Op]()
          var includesLine = false

          override def visitFieldInsn(
            opcode: Int, owner: String, name: String, desc: String) {
          }
          override def visitIincInsn(variable: Int, increment: Int) {
          }
          override def visitInsn(opcode: Int) {
          }
          override def visitIntInsn(opcode: Int, operand: Int) {
          }
          override def visitJumpInsn(opcode: Int, label: Label) {
          }
          override def visitLdcInsn(cst: Object) {
          }
          override def visitLookupSwitchInsn(dflt: Label, keys: Array[Int],
            labels: Array[Label]) {}
          override def visitMethodInsn(
            opcode: Int, owner: String, name: String, desc: String) {
          }
          override def visitMultiANewArrayInsn(desc: String, dims: Int) {
          }
          override def visitTableSwitchInsn(
            min: Int, max: Int, dflt: Label, labels: Array[Label]) {
          }
          override def visitTypeInsn(opcode: Int, tpe: String) {}
          override def visitVarInsn(opcode: Int, variable: Int) {}

          override def visitLineNumber(line: Int, start: Label) {
            minLine = scala.math.min(minLine, line)
            maxLine = scala.math.max(maxLine, line)
            if (targetLine >= minLine && targetLine <= maxLine) {
              includesLine = true
            }
          }
          override def visitEnd() {
            if (includesLine) {
              methods += MethodBytecode(className, signature, ops.toList)
            }
          }
        }
      } else null
    }
  }

  def visitClass[T <: RichClassVisitor](
    classContainerFile: String, visitor: T): Option[T#Result] = {
    ClassIterator.find(
      List(new File(classContainerFile)),
      (location, classReader) => classReader.accept(visitor, NoFlags))
    visitor.result
  }

  def locateBytecode(
    sourceName: String,
    line: Int): List[MethodBytecode] = {
    classFilesForSourceName(sourceName).flatMap { f =>
      visitClass(
        f, new MethodByteCodeFinder(sourceName, line)).getOrElse(List())
    }.toList
  }
}

