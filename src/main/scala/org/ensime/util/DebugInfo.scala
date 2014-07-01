package org.ensime.debug
import java.io._
import org.objectweb.asm._
import org.objectweb.asm.commons.EmptyVisitor
import scala.math._

/*
* Helper for printing debug information for a class.
*/
object DebugInfo {

  def main(args: Array[String]) {
    val f = new File(args(0))
    val fs = new FileInputStream(f)
    try {
      val reader = new ClassReader(fs)
      reader.accept(new EmptyVisitor() {
        var qualName: String = null
        var packageName: String = null
        var sourceName: String = null
        var startLine = Int.MaxValue
        var endLine = Int.MinValue

        override def visit(version: Int, access: Int,
          name: String, signature: String, superName: String,
          interfaces: Array[String]) {
          if (name != null) {
            qualName = name.replace("/", ".")
            packageName = qualName.split(".").lastOption.getOrElse("")
            println("")
            println("--------")
            println(qualName)
          }
        }

        override def visitAttribute(att: Attribute) {
          println("Attribute:" + att.`type`)
        }

        override def visitAnnotation(desc: String,
          visibleAtRuntime: Boolean): AnnotationVisitor = {
          println("Annotation: " + desc)
          new EmptyVisitor() {
            override def visit(name: String, value: Object) {
              println(name)
            }
            override def visitArray(name: String): AnnotationVisitor = {
              println("Array:" + name)
              null
            }
            override def visitEnum(name: String, desc: String, value: String) {
              println("Enum:" + name)
            }
          }
        }

        override def visitField(i: Int, s1: String,
          s2: String, s3: String, a: Any): FieldVisitor = null

        override def visitMethod(access: Int, name: String,
          desc: String, signature: String,
          exceptions: Array[String]): MethodVisitor = {
          println("Method: " + signature + " " + name)
          new EmptyVisitor() {
            override def visitLineNumber(line: Int, start: Label) {
              println("  line: " + line + ", label=" + start)
              startLine = min(startLine, line)
              endLine = max(endLine, line)
            }
            override def visitAttribute(att: Attribute) {}
            override def visitAnnotation(desc: String,
              visibleAtRuntime: Boolean): AnnotationVisitor = null
            override def visitAnnotationDefault(): AnnotationVisitor = null
            override def visitParameterAnnotation(parameter: Int,
              desc: String, visible: Boolean): AnnotationVisitor = null
          }
        }

        override def visitSource(source: String, debug: String) {
          sourceName = source
        }

        override def visitEnd() {
        }

      }, 0)
    } catch {
      case e: Exception =>
        System.err.println("Error reading classfile.")
        e.printStackTrace(System.err)
    } finally {
      fs.close()
    }
  }

}

