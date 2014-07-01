package org.ensime.indexer

import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.util.ClassIterator
import org.ensime.util.FileUtils
import org.ensime.util.RichClassVisitor
import org.ensime.util.ClassLocation
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.commons.EmptyVisitor
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.io.AbstractFile

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
  private val allSources: Set[AbstractFile] = FileUtils.expandSourceJars(
    config.sources ++ config.referenceSources
  ).toSet

  private val classFilesForSourceName =
    new mutable.HashMap[String, mutable.HashSet[ClassLocation]].withDefault(s => mutable.HashSet())

  private val sourceNamesForClassFile =
    new mutable.HashMap[ClassLocation, mutable.HashSet[String]].withDefault(s => mutable.HashSet())

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
        new EmptyVisitor() with MethodDescriber {
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
    if (fromClasses.nonEmpty) fromClasses else forFileType(".jar")
  }

  def sourceFileCandidates(
    enclosingPackage: String,
    classNamePrefix: String): Set[AbstractFile] = {
    println("Looking for " + (enclosingPackage, classNamePrefix))
    val subPath = enclosingPackage.replace(".", "/") + "/" + classNamePrefix.replaceAll("[$]$", "")
    val sourceNames: Set[String] = sourceNamesForClassFile.collect {
      case (loc, sourceNames) if loc.file.contains(subPath) ||
        loc.entry.contains(subPath) => sourceNames
    }.flatten.toSet
    sourceNames.flatMap { sourceName =>
      allSources.filter(_.name == sourceName)
    }
  }

}
