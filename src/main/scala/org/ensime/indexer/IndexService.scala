package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.apache.commons.vfs2.FileObject
import org.apache.lucene.document.Document
import org.apache.lucene.document.TextField
import org.apache.lucene.document.Field.Store
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.PrefixQuery
import org.apache.lucene.search.TermQuery
import DatabaseService._
import org.apache.lucene.search.BooleanClause.Occur
import org.ensime.indexer.lucene._
import pimpathon.list._

object IndexService extends SLF4JLogging {

  class FqnAnalyzer extends DynamicSynonymAnalyzer {
    private def cases(s: String) = List(s, s.toLowerCase)
    def synonyms(term: String): Set[String] = {
      val (path, name) = term.split('.').toList.initLast
      def camel(s: String) = s.filter(_.isUpper)
      def spacey(s: String) = List(s, s.replace('.', ' '))

      val shortPkg = path.map(_.charAt(0)).mkString("", ".", "." + name)
      val innerNames = name.split('$').filter(_.size > 3).flatMap(cases)

      Set(term, name, camel(name)) ++ innerNames ++ spacey(shortPkg)
    }.filter(_.size > 1).flatMap(cases) - term
  }

  sealed trait FqnIndex extends Entity {
    def file: Option[FileCheck] // not retrieved
    def fqn: String
    def id = fqn
  }
  case class ClassIndex(fqn: String, file: Option[FileCheck]) extends FqnIndex
  case class MethodIndex(fqn: String, file: Option[FileCheck]) extends FqnIndex
  case class FieldIndex(fqn: String, file: Option[FileCheck]) extends FqnIndex
  abstract class AFqnIndexS[T <: FqnIndex](
      clazz: Class[T],
      cons: (String, Option[FileCheck]) => T) extends EntityS(clazz) {
    def addFields(doc: Document, i: T): Unit = {
      doc.add(new TextField("file", i.file.get.filename, Store.NO))
      doc.add(new TextField("fqn", i.fqn, Store.YES))
    }
    def toEntity(d: Document): T = cons(d.get("fqn"), None)
  }
  implicit object ClassIndexS extends AFqnIndexS(classOf[ClassIndex], ClassIndex)
  implicit object MethodIndexS extends AFqnIndexS(classOf[MethodIndex], MethodIndex)
  implicit object FieldIndexS extends AFqnIndexS(classOf[FieldIndex], FieldIndex)
  implicit object FqnIndexS extends DocumentRecovery[FqnIndex] {
    def toEntity(d: Document) = d.get("TYPE") match {
      case "ClassIndex" => ClassIndexS.toEntity(d)
      case "MethodIndex" => MethodIndexS.toEntity(d)
      case "FieldIndex" => FieldIndexS.toEntity(d)
      case o => throw new IllegalStateException(o)
    }
  }
  private val ClassIndexT = new TermQuery(new Term("TYPE", classOf[ClassIndex].getSimpleName))
  private val MethodIndexT = new TermQuery(new Term("TYPE", classOf[MethodIndex].getSimpleName))
  private val FieldIndexT = new TermQuery(new Term("TYPE", classOf[FieldIndex].getSimpleName))

  /**
   * Like `PrefixQuery` but gives a higher value to exact matches.
   * [Stack Overflow](http://stackoverflow.com/questions/17723025)
   */
  class BoostedPrefixQuery(t: Term) extends BooleanQuery {
    add(new PrefixQuery(t), Occur.SHOULD)
    add(new TermQuery(t), Occur.SHOULD)
  }
}

class IndexService(path: File) {
  import IndexService._

  private val analyzers = Map("fqn" -> new FqnAnalyzer)

  private val lucene = new SimpleLucene(path, analyzers)

  def persist(check: FileCheck, symbols: List[FqnSymbol]): Unit = {
    val f = Some(check)
    val fqns: List[Document] = symbols.map {
      case FqnSymbol(_, _, _, fqn, Some(_), _, _, _, _) => MethodIndex(fqn, f).toDocument
      case FqnSymbol(_, _, _, fqn, _, Some(_), _, _, _) => FieldIndex(fqn, f).toDocument
      case FqnSymbol(_, _, _, fqn, _, _, _, _, _) => ClassIndex(fqn, f).toDocument
    }
    lucene.create(fqns, commit = false)
  }

  def commit(): Unit = lucene.commit()

  def remove(fs: List[FileObject]): Unit = {
    val terms = fs.map { f => new TermQuery(new Term("file", f.getName.getURI)) }
    lucene.delete(terms, commit = false) // don't commit yet
  }

  def searchClasses(query: String, max: Int): List[ClassIndex] = {
    val q = new BooleanQuery {
      add(new BoostedPrefixQuery(new Term("fqn", query)), Occur.MUST)
      add(ClassIndexT, Occur.MUST)
    }
    lucene.search(q, max).map(_.toEntity[ClassIndex])
  }

  def searchClassesFieldsMethods(query: String, max: Int): List[FqnIndex] = {
    val q = new BooleanQuery {
      add(new BoostedPrefixQuery(new Term("fqn", query)), Occur.MUST)
      add(new BooleanQuery {
        add(ClassIndexT, Occur.SHOULD)
        add(FieldIndexT, Occur.SHOULD)
        add(MethodIndexT, Occur.SHOULD)
      }, Occur.MUST)
    }
    lucene.search(q, max).map(_.toEntity[FqnIndex])
  }

}
