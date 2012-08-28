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
import org.ensime.config.ProjectConfig
import org.ensime.util.ClassIterator
import org.ensime.util.RichClassVisitor
import org.ensime.util.CanonFile
import org.ensime.util.ClassLocation
import org.objectweb.asm.ClassReader
import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.commons.EmptyVisitor
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MultiMap

case class Op(
  op: String,
  description: String)
case class MethodBytecode(
  className: String,
  methodName: String,
  methodSignature: Option[String],
  byteCode: List[Op],
  startLine: Int,
  endLine: Int)

class ClassFileIndex(config: ProjectConfig) {

  // TODO(aemoncannon): This set can change over time if new
  // source files are created. Need to arrange for indexer to
  // receive a re-up message when new sources are created.
  private val allSources: Set[CanonFile] = config.sources ++ config.referenceSources

  private val classFilesForSourceName =
    new HashMap[String, HashSet[ClassLocation]].withDefault(s => HashSet())

  private val sourceNamesForClassFile =
    new HashMap[ClassLocation, HashSet[String]].withDefault(s => HashSet())

  val ASMAcceptAll = 0

  def indexFiles(files: Iterable[File]) {
    println("Indexing source names for files: " + files)
    val t = System.currentTimeMillis()
    ClassIterator.find(files,
      (location, classReader) =>
        classReader.accept(new EmptyVisitor() {
          override def visitSource(source: String, debug: String) {
            val set = classFilesForSourceName(source)
            set += location
            classFilesForSourceName(source) = set

            val sourceSet = sourceNamesForClassFile(location)
            sourceSet += source
            sourceNamesForClassFile(location) = sourceSet
          }
        }, ASMAcceptAll))
    val elapsed = System.currentTimeMillis() - t
    println("Finished indexing " +
      files.size + " classpath files in " +
      elapsed / 1000.0 + " seconds.")
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
        return new EmptyVisitor() with MethodDescriber {
          var maxLine = Int.MinValue
          var minLine = Int.MaxValue
          var ops = ListBuffer[Op]()
          var includesLine = false

          def appendOp(name: String, args: String): Unit = {
            ops += Op(name, args)
          }

          override def visitLineNumber(line: Int, start: Label) {
            minLine = scala.math.min(minLine, line)
            maxLine = scala.math.max(maxLine, line)
            if (targetLine >= minLine && targetLine <= maxLine) {
              includesLine = true
            }
          }
          override def visitEnd() {
            if (includesLine) {
              methods += MethodBytecode(className,
                name, Option(signature),
                ops.toList,
                minLine,
                maxLine)
            }
          }
        }
      } else null
    }
  }

  def locateBytecode(
    sourceName: String,
    line: Int): List[MethodBytecode] = {
    def forFileType(extension: String): List[MethodBytecode] = {
      classFilesForSourceName(sourceName).filter { loc => loc.file.endsWith(".class") }.flatMap { f =>
        ClassIterator.findInClasses(
          List(new File(f.file)),
          new MethodByteCodeFinder(sourceName, line)).getOrElse(List())
      }.toList.sortBy(_.startLine * -1)
    }
    val fromClasses = forFileType(".class")
    if (!fromClasses.isEmpty) fromClasses else forFileType(".jar")
  }

  def sourceFileCandidates(
    enclosingPackage: String,
    classNamePrefix: String): Set[File] = {
    println("Looking for " + (enclosingPackage, classNamePrefix))
    val subPath = enclosingPackage.replace(".", "/") + "/" + classNamePrefix
    // TODO(aemoncannon): Build lookup structure to make this more efficient.
    if (System.getProperty("ensime.log.symbol.lookup") != null) {
      println("subPath is " + subPath)
      println("sourceNamesForClassFile is " + sourceNamesForClassFile)
    }
    val sourceNames: Set[String] = sourceNamesForClassFile.collect {
      case (loc, sourceNames) if (
        loc.file.contains(subPath) ||
        loc.entry.contains(subPath)) => sourceNames
    }.flatten.toSet
    sourceNames.flatMap { sourceName =>
      allSources.filter(_.getName() == sourceName)
    }
  }

}

