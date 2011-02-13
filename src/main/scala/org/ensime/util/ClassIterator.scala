/**
* Much of this code is derived from the excellent tool,
* ClassUtil, by Brian M. Clapper
* 
* Copyright 2010, Brian M. Clapp
* All Rights Reserved
*/

package org.ensime.util

import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.mutable.{ HashMap, HashSet }
import org.objectweb.asm.FieldVisitor
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.commons.EmptyVisitor
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes
import java.io._
import java.util.jar.{ JarFile, Manifest => JarManifest }
import java.util.zip._
import java.io.{ File, InputStream, IOException }

trait ClassHandler {
  def onClass(name: String, location: String, flags: Int) {}
  def onMethod(className: String, name: String, location: String, flags: Int) {}
  def onField(className: String, name: String, location: String, flags: Int) {}
}

private class ClassVisitor(location: File, handler: ClassHandler) extends EmptyVisitor {
  var currentClassName: Option[String] = None
  val path: String = location.getPath()

  override def visit(version: Int,
    access: Int,
    name: String,
    signature: String,
    superName: String,
    interfaces: Array[String]) {
    val nm = mapClassName(name)
    currentClassName = Some(nm)
    handler.onClass(nm, path, access)
  }

  override def visitMethod(access: Int,
    name: String,
    description: String,
    signature: String,
    exceptions: Array[String]): MethodVisitor =
    {
      handler.onMethod(currentClassName.getOrElse("."), name, path, access)
      null
    }

  override def visitField(access: Int,
    name: String,
    description: String,
    signature: String,
    value: java.lang.Object): FieldVisitor =
    {
      handler.onField(currentClassName.getOrElse("."), name, path, access)
      null
    }

  private def mapClassName(name: String): String = {
    if (name == null) ""
    else name.replaceAll("/", ".")
  }
}

object ClassIterator {

  val ASMAcceptCriteria = 0

  def find(path: Iterable[File], handler: ClassHandler) {
    for (f <- path) {
      try {
        findClassesIn(f, handler)
      } catch {
        case e: IOException => {
          System.err.println("Failed to open: " + f)
          e.printStackTrace(System.err)
        }
      }
    }
  }

  private def findClassesIn(f: File, handler: ClassHandler) {
    val name = f.getPath.toLowerCase
    if (name.endsWith(".jar"))
      processJar(f, handler)
    else if (name.endsWith(".zip"))
      processZip(f, handler)
    else if (f.isDirectory)
      processDirectory(f, handler)
  }

  private def processJar(file: File, handler: ClassHandler) {
    val jar = new JarFile(file)
    processOpenZip(file, jar, handler)
    var manifest = jar.getManifest
    if (manifest != null) {
      val path = loadManifestPath(jar, file, manifest)
      find(path, handler)
    }
  }

  private def loadManifestPath(jar: JarFile,
    jarFile: File,
    manifest: JarManifest): List[File] =
    {
      import scala.collection.JavaConversions._
      val attrs = manifest.getMainAttributes
      val value = attrs.get("Class-Path").asInstanceOf[String]

      if (value == null)
        Nil

      else {
        val parent = jarFile.getParent
        val tokens = value.split("""\s+""").toList
        if (parent == null)
          tokens.map(new File(_))
        else
          tokens.map(s => new File(parent + File.separator + s))
      }
    }

  private def processZip(file: File, handler: ClassHandler) {
    processOpenZip(file, new ZipFile(file), handler)
  }

  private def processOpenZip(file: File, zipFile: ZipFile, handler: ClassHandler) {
    import scala.collection.JavaConversions._
    val zipFileName = file.getPath
    for (e <- zipFile.entries) {
      if (isClass(e)) {
        processClassData(new BufferedInputStream(
          zipFile.getInputStream(e)), file, handler)
      }
    }
  }

  // Matches both ZipEntry and File
  type FileEntry = {
    def isDirectory(): Boolean
    def getName(): String
  }

  private def isClass(e: FileEntry): Boolean =
    (!e.isDirectory) && (e.getName.toLowerCase.endsWith(".class"))

  private def processDirectory(dir: File, handler: ClassHandler) {
    import FileUtils._
    for (f <- dir.andTree) {
      if (isClass(f)) {
        processClassData(new BufferedInputStream(
          new FileInputStream(f)), dir, handler)
      }
    }
  }

  private def processClassData(is: InputStream, location: File, handler: ClassHandler) {
    val cr = new ClassReader(is)
    val visitor = new ClassVisitor(location, handler)
    cr.accept(visitor, ClassReader.SKIP_CODE)
  }

}
