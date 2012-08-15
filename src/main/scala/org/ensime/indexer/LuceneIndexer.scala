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
import java.io.IOException
import java.io.Reader
import org.apache.lucene.analysis.SimpleAnalyzer
import org.apache.lucene.analysis.ReusableAnalyzerBase
import org.apache.lucene.analysis.LetterTokenizer
import org.apache.lucene.analysis.TokenFilter
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.CharTokenizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.index.CorruptIndexException
import org.apache.lucene.index.FieldInvertState
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.Term
import org.apache.lucene.search.BooleanClause
import org.apache.lucene.search.BooleanQuery
import org.apache.lucene.search.DefaultSimilarity
import org.apache.lucene.search.FuzzyQuery
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.MultiTermQuery
import org.apache.lucene.search.PrefixQuery
import org.apache.lucene.search.ScoringRewrite
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.Query
import org.apache.lucene.search.TopScoreDocCollector
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.store.RAMDirectory
import org.apache.lucene.queryParser.QueryParser
import org.apache.lucene.store.NIOFSDirectory
import org.apache.lucene.util.Version
import org.eclipse.jdt.core.compiler.CharOperation
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileReader
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import scala.Char
import scala.actors._
import scala.actors.Actor._
import org.ensime.model.{
  TypeInfo,
  SymbolSearchResult,
  TypeSearchResult,
  MethodSearchResult,
  SymbolSearchResults,
  ImportSuggestions
}
import scala.collection.JavaConversions
import org.ardverk.collection._
import io.prelink.critbit.MCritBitTree
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.{ NoPosition }
import scala.util.matching.Regex
import scala.collection.mutable.{ HashMap, HashSet, ArrayBuffer, ListBuffer }
import org.objectweb.asm.Opcodes;
import org.json.simple._

object LuceneIndex extends StringSimilarity {
  val KeyIndexVersion = "indexVersion"
  val KeyFileHashes = "fileHashes"
  val IndexVersion: Int = 5
  val DirName = ".ensime_lucene"

  val Similarity = new DefaultSimilarity {
    override def computeNorm(field: String, state: FieldInvertState): Float = {
      state.getBoost() * (1F / (state.getLength()))
    }
  }

  def splitTypeName(nm: String): List[String] = {
    val keywords = new ListBuffer[String]()
    var i = 0
    var k = 0
    while (i < nm.length) {
      val c: Char = nm.charAt(i)
      if (Character.isUpperCase(c) && i != k) {
        keywords += nm.substring(k, i)
        k = i
      }
      i += 1
    }
    if (i != k) {
      keywords += nm.substring(k)
    }
    keywords.toList
  }

  private val cache = new HashMap[(String, String), Int]
  def editDist(a: String, b: String): Int = {
    cache.getOrElseUpdate((a, b), getLevenshteinDistance(a, b))
  }

  def isValidType(s: String): Boolean = {
    val i = s.indexOf("$")
    i == -1 || (i == (s.length - 1))
  }
  def isValidMethod(s: String): Boolean = {
    s.indexOf("$") == -1 && !s.equals("<init>") && !s.equals("this")
  }

  def loadIndexUserData(dir: File): (Int, Map[String, String]) = {
    try {
      if (dir.exists) {
        println(dir.toString + " exists, loading user data...")
        var index = FSDirectory.open(dir)
        val reader = IndexReader.open(index)
        println("Num docs: " + reader.numDocs())
        val userData = reader.getCommitUserData()
        val onDiskIndexVersion = Option(
          userData.get(KeyIndexVersion)).getOrElse("0").toInt
        val src = Option(userData.get(KeyFileHashes)).getOrElse("{}")
        val indexedFiles: Map[String, String] = try {
          JSONValue.parse(src) match {
            case obj: java.util.Map[String, Object] =>
              {
                val m = JavaConversions.mapAsScalaMap[String, Object](obj)
                m.map { ea => (ea._1, ea._2.toString) }.toMap
              }
            case _ => Map()
          }
        } catch {
          case e: Throwable => Map()
        }

        reader.close()
        (onDiskIndexVersion, indexedFiles)
      } else (0, Map())
    } catch {
      case e: IOException => e.printStackTrace(); (0, Map())
      case e: CorruptIndexException => e.printStackTrace(); (0, Map())
      case e: Exception => e.printStackTrace(); (0, Map())
    }
  }

  def shouldReindex(
    version: Int,
    onDisk: Set[(String, String)],
    proposed: Set[(String, String)]): Boolean = {

    // Very conservative.
    // Re-index whenver the proposed set
    // contains unindexed files.
    (version < IndexVersion ||
      !(proposed -- onDisk.toList).isEmpty)
  }

  def tokenize(nm: String): String = {
    val tokens = new StringBuilder
    tokens.append(nm.toLowerCase)
    tokens.append(" ")
    var i = 0
    var k = 0
    while (i < nm.length) {
      val c: Char = nm.charAt(i)
      if ((c == ' ' || c == '.') && i != k) {
        tokens.append(nm.substring(k, i))
        tokens.append(" ")
        k = i + 1
      } else if (Character.isUpperCase(c) && i != k) {
        tokens.append(nm.substring(k, i))
        tokens.append(" ")
        k = i
      }
      i += 1
    }
    if (i != k) {
      tokens.append(nm.substring(k))
    }
    tokens.toString
  }

  private def buildDoc(value: SymbolSearchResult): Document = {
    val doc = new Document()

    doc.add(new Field("tags",
      tokenize(value.name), Field.Store.NO, Field.Index.ANALYZED))
    doc.add(new Field("localNameTags",
      tokenize(value.localName), Field.Store.NO, Field.Index.ANALYZED))

    doc.add(new Field("name",
      value.name, Field.Store.YES, Field.Index.NOT_ANALYZED))
    doc.add(new Field("localName",
      value.localName, Field.Store.YES, Field.Index.NOT_ANALYZED))
    doc.add(new Field("type",
      value.declaredAs.toString, Field.Store.YES,
      Field.Index.NOT_ANALYZED))
    value.pos match {
      case Some((file, offset)) => {
        doc.add(new Field("file", file, Field.Store.YES,
          Field.Index.NOT_ANALYZED))
        doc.add(new Field("offset", offset.toString, Field.Store.YES,
          Field.Index.NOT_ANALYZED))
      }
      case None => {
        doc.add(new Field("file", "", Field.Store.YES,
          Field.Index.NOT_ANALYZED))
        doc.add(new Field("offset", "", Field.Store.YES,
          Field.Index.NOT_ANALYZED))
      }
    }
    value match {
      case value: TypeSearchResult => {
        doc.add(new Field("docType",
          "type", Field.Store.NO,
          Field.Index.ANALYZED))
      }
      case value: MethodSearchResult => {
        doc.add(new Field("owner",
          value.owner, Field.Store.YES,
          Field.Index.NOT_ANALYZED))
        doc.add(new Field("docType",
          "method", Field.Store.NO,
          Field.Index.ANALYZED))
      }
    }
    doc
  }

  private def buildSym(d: Document): SymbolSearchResult = {
    val name = d.get("name")
    val localName = d.get("localName")
    val tpe = scala.Symbol(d.get("type"))
    val file = d.get("file")
    val offset = d.get("offset")
    val pos = (file, offset) match {
      case (file:String, "") => Some((file, 0))
      case (file:String, offset:String) => Some((file, offset.toInt))
      case _ => None
    }
    val owner = Option(d.get("owner"))
    owner match {
      case Some(owner) => {
        MethodSearchResult(name, localName, tpe, pos, owner)
      }
      case None => {
        TypeSearchResult(name, localName, tpe, pos)
      }
    }
  }

  private def declaredAs(name: String, flags: Int) = {
    if (name.endsWith("$")) 'object
    else if ((flags & Opcodes.ACC_INTERFACE) != 0) 'trait
    else 'class
  }

  private def include(name: String,
    includes: Iterable[Regex],
    excludes: Iterable[Regex]): Boolean = {
    if (includes.isEmpty || includes.exists(_.findFirstIn(name) != None)) {
      excludes.forall(_.findFirstIn(name) == None)
    } else {
      false
    }
  }

  def buildStaticIndex(
    writer: IndexWriter,
    files: Iterable[File],
    includes: Iterable[Regex],
    excludes: Iterable[Regex]) {
    val t = System.currentTimeMillis()

    sealed abstract trait IndexEvent
    case class ClassEvent(name: String,
      location: String, flags: Int) extends IndexEvent
    case class MethodEvent(className: String, name: String,
      location: String, flags: Int) extends IndexEvent
    case object StopEvent extends IndexEvent

    /**
     * Implement a producer, consumer model for creation of the static index.
     * We fill the IndexWorkQueue's mailbox as quickly as possibly, reading
     * classes off the disk (see use of ClassIterator below). IndexWorkQueue
     * drains the mailbox as fast as it can write to the search index.
     *
     * The idea is to allow the reading of class information to proceed at
     * full speed, spooling work to the mailbox, while the index writing
     * slowly catches up.
     */
    class IndexWorkQueue extends Actor {
      def act() {
        loop {
          receive {
            case ClassEvent(name: String, location: String, flags: Int) => {
              val i = name.lastIndexOf(".")
              val localName = if (i > -1) name.substring(i + 1) else name
              val value = TypeSearchResult(name,
                localName,
                declaredAs(name, flags),
                Some((location, -1)))
              val doc = buildDoc(value)
              writer.addDocument(doc)
            }
            case MethodEvent(className: String, name: String,
              location: String, flags: Int) => {
              val isStatic = ((flags & Opcodes.ACC_STATIC) != 0)
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
            }
            case StopEvent => {
              reply(StopEvent)
              exit()
            }
          }
        }
      }
    }
    val indexWorkQ = new IndexWorkQueue
    indexWorkQ.start

    val handler = new ClassHandler {
      var classCount = 0
      var methodCount = 0
      var validClass = false
      override def onClass(name: String, location: String, flags: Int) {
        val isPublic = ((flags & Opcodes.ACC_PUBLIC) != 0)
        validClass = (isPublic && isValidType(name) && include(name,
          includes, excludes))
        if (validClass) {
          indexWorkQ ! ClassEvent(name, location, flags)
          classCount += 1
        }
      }
      override def onMethod(className: String, name: String,
        location: String, flags: Int) {
        val isPublic = ((flags & Opcodes.ACC_PUBLIC) != 0)
        if (validClass && isPublic && isValidMethod(name)) {
          indexWorkQ ! MethodEvent(className, name, location, flags)
          methodCount += 1
        }
      }
    }

    println("Updated: Indexing classpath...")
    ClassIterator.findPublicSymbols(files, handler)
    indexWorkQ !? StopEvent
    val elapsed = System.currentTimeMillis() - t
    println("Indexing completed in " + elapsed / 1000.0 + " seconds.")
    println("Indexed " + handler.classCount + " classes with " +
      handler.methodCount + " methods.")
  }

}

trait LuceneIndex {

  import LuceneIndex._

  private val analyzer = new SimpleAnalyzer(Version.LUCENE_35)
  private val config: IndexWriterConfig = new IndexWriterConfig(
    Version.LUCENE_35, analyzer)
  config.setSimilarity(Similarity)
  private var index: FSDirectory = null
  private var indexWriter: Option[IndexWriter] = None
  private var indexReader: Option[IndexReader] = None

  def initialize(
    root: File,
    files: Set[File],
    includes: Iterable[Regex],
    excludes: Iterable[Regex]): Unit = {

    val dir: File = new File(root, DirName)

    val hashed = files.map { f =>
      if (f.exists) {
        (f.getAbsolutePath(), FileUtils.md5(f))
      } else {
        (f.getAbsolutePath(), "")
      }
    }
    val (version, indexedFiles) = loadIndexUserData(dir)
    println("ENSIME indexer version: " + IndexVersion)
    println("On disk version: " + version)
    println("On disk indexed files: " + indexedFiles.toString)

    if (shouldReindex(version, indexedFiles.toSet, hashed)) {
      println("Requires re-index.")
      println("Deleting on-disk index.")
      FileUtils.delete(dir)
    } else {
      println("No need to re-index.")
    }

    index = FSDirectory.open(dir)

    indexWriter = Some(new IndexWriter(index, config))
    for (writer <- indexWriter) {
      if (shouldReindex(version, indexedFiles.toSet, hashed)) {
        println("Re-indexing...")
        buildStaticIndex(writer, files, includes, excludes)
        val json = JSONValue.toJSONString(
          JavaConversions.mapAsJavaMap(hashed.toMap))
        val userData = Map[String, String](
          (KeyIndexVersion, IndexVersion.toString),
          (KeyFileHashes, json))
        writer.commit(JavaConversions.mapAsJavaMap(userData))
      }
    }

  }

  def keywordSearch(
    keywords: Iterable[String],
    maxResults: Int = 0,
    restrictToTypes: Boolean = false): List[SymbolSearchResult] = {
    var results = new ListBuffer[SymbolSearchResult]
    search(keywords, maxResults, restrictToTypes, false,
      {(r: SymbolSearchResult) =>
        results += r
      })
    results.toList
  }

  def getImportSuggestions(typeNames: Iterable[String],
    maxResults: Int = 0): List[List[SymbolSearchResult]] = {
    def suggestions(typeName: String): List[SymbolSearchResult] = {
      val keywords = typeName::splitTypeName(typeName)
      val candidates = new HashSet[SymbolSearchResult]

      search(keywords,
	maxResults, true, true,
        (r: SymbolSearchResult) =>
        r match {
          case r: TypeSearchResult => candidates += r
          case _ => // nothing
        })

      // Sort by edit distance of type name primarily, and
      // length of full name secondarily.
      candidates.toList.sortWith { (a, b) =>
        val d1 = editDist(a.localName, typeName)
        val d2 = editDist(b.localName, typeName)
        if (d1 == d2) a.name.length < b.name.length
        else d1 < d2
      }

    }
    typeNames.map(suggestions).toList
  }


  def insert(value: SymbolSearchResult): Unit = {
    for (w <- indexWriter) {
      val doc = buildDoc(value)
      w.updateDocument(new Term("name", value.name), doc)
    }
  }

  def remove(key: String): Unit = {
    for (w <- indexWriter) {
      val w = new IndexWriter(index, config)
      w.deleteDocuments(new Term("name", key))
    }
  }

  def commit(): Unit = {
    for (w <- indexWriter) {
      w.commit()
      for (r <- indexReader) {
        IndexReader.openIfChanged(r)
      }
    }
  }

  private def splitTypeName(nm: String): List[String] = {
    val keywords = new ListBuffer[String]()
    var i = 0
    var k = 0
    while (i < nm.length) {
      val c: Char = nm.charAt(i)
      if (Character.isUpperCase(c) && i != k) {
        keywords += nm.substring(k, i)
        k = i
      }
      i += 1
    }
    if (i != k) {
      keywords += nm.substring(k)
    }
    keywords.toList
  }

  def search(
    keys: Iterable[String],
    maxResults: Int,
    restrictToTypes: Boolean,
    fuzzy: Boolean,
    receiver: (SymbolSearchResult => Unit)): Unit = {

    val preparedKeys = keys.filter(!_.isEmpty).map(_.toLowerCase)
    val field = if (restrictToTypes) "localNameTags" else "tags"

    val bq = new BooleanQuery()
    if (restrictToTypes) {
      bq.add(new TermQuery(new Term("docType", "type")),
        BooleanClause.Occur.MUST)
    }
    for (key <- preparedKeys) {
      val term = new Term(field, key)
      val pq = if (fuzzy) new FuzzyQuery(term, 0.6F) else new PrefixQuery(term)
      /* Must force scoring, otherwise prefix queries use a constant
      score which ignores field length normalization, etc.
      http://stackoverflow.com/questions/3060636/lucene-question-of-score-caculation-with-prefixquery */
      pq.setRewriteMethod(ScoringRewrite.SCORING_BOOLEAN_QUERY_REWRITE)
      import BooleanClause._
      val operator = if (fuzzy) Occur.SHOULD else Occur.MUST
      bq.add(pq, operator)

      // val boostExacts = new TermQuery(term)
      // boostExacts.setBoost(2)
      // bq.add(boostExacts, Occur.SHOULD)
    }
    searchByQuery(bq, maxResults, receiver)
  }

  private def searchByQuery(
    q: Query,
    maxResults: Int,
    receiver: (SymbolSearchResult => Unit)): Unit = {
    if (indexReader.isEmpty) {
      indexReader = Some(IndexReader.open(index))
    }
    println("Handling query: " + q)
    for (reader <- indexReader) {
      val hitsPerPage = if (maxResults == 0) 100 else maxResults
      val searcher = new IndexSearcher(reader)
      searcher.setSimilarity(LuceneIndex.Similarity)
      val hits = searcher.search(q, maxResults)
      for (hit <- hits.scoreDocs) {
        val docId = hit.doc
        val doc = searcher.doc(docId)
        receiver(buildSym(doc))
        //println(hit.score + "   " + doc.get("name"))
      }
    }
  }

  def close() {
    for (w <- indexWriter) {
      w.close()
    }
  }
}


object IndexTest extends LuceneIndex {

  def projectConfig() {
    println("done")
  }

  def main(args: Array[String]) {
    val classpath = "/Users/aemon/projects/ensime/target/scala-2.9.2/classes:/Users/aemon/projects/ensime/lib/org.scala-refactoring_2.9.2-SNAPSHOT-0.5.0-SNAPSHOT.jar"
    val files = classpath.split(":").map { new File(_) }.toSet
    initialize(new File("."), files, List(), List())

    import java.util.Scanner
    val in = new Scanner(System.in)
    var line = in.nextLine()
    while (!line.isEmpty) {
      val keys = line.split(" ")
      for (l <- getImportSuggestions(keys, 20)) {
        for (s <- l) {
          println(s.name)
        }
      }
      line = in.nextLine()
    }
  }
}
