package org.ensime.indexer

import com.google.common.io.ByteStreams
import org.apache.commons.vfs2.FileObject

import scala.tools.scalap.scalax.rules.scalasig._

class ClassfileDepickler(file: FileObject) {

  val scalasig = depickle(file)

  /** Uses scalap to produce a scala reflective view of the classfile */
  private def depickle(file: FileObject): Option[ScalaSig] = {
    val in = file.getContent.getInputStream
    try {
      val bytes = ByteStreams.toByteArray(in)
      val byteCode = ByteCode(bytes)
      val classFile = ClassFileParser.parse(byteCode)
      ScalaSigParser.parse(classFile)
    } catch {
      // ClassFileParser fails to parse some JDK class files
      case e: Exception => None
    } finally in.close()
  }

  def getTypeAliases: Seq[RawType] = {
    scalasig match {
      case Some(sig: ScalaSig) =>
        sig.symbols.map {
          case s: AliasSymbol => Some(RawType(symbolName(s), access(s)))
          case _ => None
        }.flatten
      case None => Nil
    }
  }

  private def access(sym: Symbol): Access = {
    if (sym.isPrivate) Private
    else if (sym.isProtected) Protected
    else Public
  }

  private def symbolName(a: Symbol): String = {
    a.parent match {
      case Some(s: SymbolInfoSymbol) => symbolName(s) + "$" + a.name
      case Some(s: Symbol) => s.toString + "." + a.name
      case None => a.name
    }
  }
}
