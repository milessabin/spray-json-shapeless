package org.ensime.indexer

import akka.actor.Actor
import org.apache.lucene.index.IndexWriter
import org.ensime.model.{ MethodSearchResult, TypeSearchResult }
import org.objectweb.asm.Opcodes

class IndexWorkQueue(writer: IndexWriter) extends Actor {
  import LuceneIndex._
  override def receive = {
    case ClassEvent(name: String, location: String, flags: Int) =>
      val i = name.lastIndexOf(".")
      val localName = if (i > -1) name.substring(i + 1) else name
      val value = TypeSearchResult(name,
        localName,
        declaredAs(name, flags),
        Some((location, -1)))
      val doc = buildDoc(value)
      writer.addDocument(doc)
    case MethodEvent(className: String, name: String,
      location: String, flags: Int) =>
      val isStatic = (flags & Opcodes.ACC_STATIC) != 0
      val revisedClassName = if (isStatic) className + "$"
      else className
      val lookupKey = revisedClassName + "." + name
      val value = MethodSearchResult(lookupKey,
        name,
        'method,
        Some((location, -1)),
        revisedClassName)
      val doc = buildDoc(value)
      writer.addDocument(doc)
    case StopEvent =>
      sender ! StopEvent
      context.stop(self)
  }
}
