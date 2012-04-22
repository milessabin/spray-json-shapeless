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

package org.ensime.server
import org.apache.lucene.analysis.SimpleAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.Term
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.TermQuery
import org.apache.lucene.search.TopScoreDocCollector
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
import scala.collection.JavaConversions._
import org.ardverk.collection._
import io.prelink.critbit.MCritBitTree
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.{ NoPosition }
import scala.util.matching.Regex
import scala.collection.mutable.{ HashMap, HashSet, ArrayBuffer, ListBuffer }
import org.objectweb.asm.Opcodes;

trait AbstractIndex {
  def initialize(): Unit = {}
  def commitChanges(): Unit = {}
  def insert(key: String, value: SymbolSearchResult): Unit
  def remove(key: String): Unit
  def search(keys: Iterable[String], receiver: (SymbolSearchResult => Unit)): Unit
}

trait LuceneIndex {

  val dir: File = new File(FileUtils.temporaryDirectory + "/.ensime_lucene")
  val index: NIOFSDirectory = new NIOFSDirectory(dir)
  val analyzer = new SimpleAnalyzer(Version.LUCENE_35)
  val config: IndexWriterConfig = new IndexWriterConfig(
    Version.LUCENE_35, analyzer);
  var indexWriter: Option[IndexWriter] = None
  var indexReader: Option[IndexReader] = None
  var batchMode = false

  def initialize(): Unit = {
    indexWriter = Some(new IndexWriter(index, config))
  }

  def setBatchMode(value: Boolean): Unit = {
    batchMode = value
  }

  private def buildDoc(value: SymbolSearchResult): Document = {
    val doc = new Document()
    doc.add(new Field("name",
      value.name, Field.Store.YES, Field.Index.ANALYZED))
    doc.add(new Field("localName",
      value.localName, Field.Store.YES, Field.Index.NOT_ANALYZED))
    doc.add(new Field("type",
      value.declaredAs.toString, Field.Store.YES,
      Field.Index.ANALYZED))
    value.pos match {
      case Some((file, offset)) => {
        doc.add(new Field("file", file, Field.Store.YES,
          Field.Index.ANALYZED))
        doc.add(new Field("offset", offset.toString, Field.Store.YES,
          Field.Index.ANALYZED))
      }
      case None => {
        doc.add(new Field("file", "", Field.Store.YES,
          Field.Index.NOT_ANALYZED))
        doc.add(new Field("offset", "", Field.Store.YES,
          Field.Index.NOT_ANALYZED))
      }
    }
    value match {
      case value: TypeSearchResult => {}
      case value: MethodSearchResult => {
        doc.add(new Field("owner",
          value.owner, Field.Store.YES,
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
    val pos = (file, offset) match{
      case ("","") => None
      case (file,"") => Some((file, 0))
      case (file,offset) => Some((file, offset.toInt))
    }
    val owner = Option(d.get("owner"))
    owner match {
      case Some(owner) => {
        new MethodSearchResult(name, localName, tpe, pos, owner)
      }
      case None => {
        new TypeSearchResult(name, localName, tpe, pos)
      }
    }

  }

  def insert(key: String, value: SymbolSearchResult): Unit = {
    for (w <- indexWriter) {
      val doc = buildDoc(value)
      w.addDocument(doc)
    }
  }

  def commitChanges(): Unit = {
    for (w <- indexWriter) {
      w.commit()
    }
    for (r <- indexReader) {
      IndexReader.openIfChanged(r)
    }
  }

  def remove(key: String): Unit = {
    for (w <- indexWriter) {
      val w = new IndexWriter(index, config)
      w.deleteDocuments(new Term("name", key))
    }
  }

  def search(keys: Iterable[String], receiver: (SymbolSearchResult => Unit)): Unit = {
    if (indexReader.isEmpty) {
      indexReader = Some(IndexReader.open(index))
    }
    val q = new QueryParser(Version.LUCENE_35, "name", analyzer).parse(
      keys.mkString(" "))
    for (reader <- indexReader) {
      val hitsPerPage = 50
      val searcher = new IndexSearcher(reader)
      val collector = TopScoreDocCollector.create(hitsPerPage, true)
      searcher.search(q, collector)
      val hits = collector.topDocs().scoreDocs
      for (hit <- hits) {
        val docId = hit.doc
        val doc = searcher.doc(docId)
        receiver(buildSym(doc))
      }
    }
  }
}

trait TrieIndex extends AbstractIndex {

  implicit def fnToForEachValCursor[V](fn: V => Any): ForEachValCursor[V] =
    new ForEachValCursor[V](fn)

  protected val trie = new MCritBitTree[String, SymbolSearchResult](
    StringKeyAnalyzer.INSTANCE)

  def insert(key: String, value: SymbolSearchResult): Unit = {
    val tmp = key.toLowerCase()
    val k = tmp + tmp.hashCode()
    trie.put(k, value)
    var i: Int = 1
    while (i < key.length) {
      val c: Char = key.charAt(i)
      if (c == '.' || c == '_') {
        trie.put(k.substring(i), value)
        trie.put(k.substring(i + 1), value)
        i += 1
      } else if (Character.isUpperCase(c)) {
        trie.put(k.substring(i), value)
      }
      i += 1
    }
  }

  def remove(key: String): Unit = {
    val tmp = key.toLowerCase()
    val k = tmp + tmp.hashCode()
    trie.remove(k)
    var i: Int = 1
    while (i < key.length) {
      val c: Char = key.charAt(i)
      if (c == '.' || c == '_') {
        trie.remove(k.substring(i))
        trie.remove(k.substring(i + 1))
        i += 1
      } else if (Character.isUpperCase(c)) {
        trie.remove(k.substring(i))
      }
      i += 1
    }
  }

  def search(keys: Iterable[String], receiver: (SymbolSearchResult => Unit)): Unit = {
    for (key <- keys) {
      trie.traverseWithPrefix(key.toLowerCase(), (r: SymbolSearchResult) =>
        r match {
          case r: TypeSearchResult => receiver(r)
          case _ => // nothing
        })
    }
  }
}

case class IndexerShutdownReq()
case class RebuildStaticIndexReq()
case class TypeCompletionsReq(prefix: String, maxResults: Int)
case class AddSymbolsReq(syms: Iterable[SymbolSearchResult])
case class RemoveSymbolsReq(syms: Iterable[String])

trait IndexerInterface { self: RichPresentationCompiler =>

  private def isType(sym: Symbol): Boolean = {
    sym.isClass || sym.isModule || sym.isInterface
  }

  private def typeSymName(sym: Symbol): String = {
    try {
      typeFullName(sym.tpe)
    } catch { case e => sym.nameString }
  }

  private def lookupKey(sym: Symbol): String = {
    if (isType(sym)) typeSymName(sym)
    else typeSymName(sym.owner) + "." + sym.nameString
  }

  def unindexTopLevelSyms(syms: Iterable[Symbol]) {
    val keys = new ArrayBuffer[String]
    for (sym <- syms) {
      keys += lookupKey(sym)
      for (mem <- try { sym.tpe.members } catch { case e => List() }) {
        keys += lookupKey(mem)
      }
    }
    indexer ! RemoveSymbolsReq(keys)
  }

  private implicit def symToSearchResult(sym: Symbol): SymbolSearchResult = {

    val pos = if (sym.pos.isDefined) { Some((sym.pos.source.path, sym.pos.point)) }
    else None

    if (isType(sym)) {
      new TypeSearchResult(
        lookupKey(sym),
        sym.nameString,
        declaredAs(sym),
        pos)
    } else {
      new MethodSearchResult(
        lookupKey(sym),
        sym.nameString,
        declaredAs(sym),
        pos,
        typeSymName(sym.owner))
    }
  }

  def indexTopLevelSyms(syms: Iterable[Symbol]) {
    val infos = new ArrayBuffer[SymbolSearchResult]
    for (sym <- syms) {
      if (Indexer.isValidType(typeSymName(sym))) {
        val key = lookupKey(sym)
        infos += sym
        for (mem <- try { sym.tpe.members } catch { case e => { List() } }) {
          if (Indexer.isValidMethod(mem.nameString)) {
            val key = lookupKey(mem)
            infos += mem
          }
        }
      }
    }
    indexer ! AddSymbolsReq(infos)
  }
}

class ForEachValCursor[V](fn: V => Any) extends Cursor[Any, V] {
  override def select(entry: java.util.Map.Entry[_, _ <: V]): Cursor.Decision = {
    fn(entry.getValue())
    Cursor.Decision.CONTINUE
  }
}

trait Indexing extends LuceneIndex with StringSimilarity {

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

  private val cache = new HashMap[(String, String), Int]
  private def editDist(a: String, b: String): Int = {
    cache.getOrElseUpdate((a, b), getLevenshteinDistance(a, b))
  }

  protected def getImportSuggestions(typeNames: Iterable[String],
    maxResults: Int = 0): List[List[SymbolSearchResult]] = {
    def suggestions(typeName: String): List[SymbolSearchResult] = {
      val keywords = splitTypeName(typeName)
      val candidates = new HashSet[SymbolSearchResult]

      for (key <- keywords) {
        search(List(key.toLowerCase),
          (r: SymbolSearchResult) =>
            r match {
              case r: TypeSearchResult => candidates += r
              case _ => // nothing
            })
      }

      // Sort by edit distance of type name primarily, and
      // length of full name secondarily.
      val candidates2 = candidates.toList.sortWith { (a, b) =>
        val d1 = editDist(a.localName, typeName)
        val d2 = editDist(b.localName, typeName)
        if (d1 == d2) a.name.length < b.name.length
        else d1 < d2
      }

      if (maxResults == 0) {
        candidates2
      } else {
        candidates2.take(maxResults)
      }
    }
    typeNames.map(suggestions).toList
  }

  private val BruteForceThresh = 1000
  protected def findTopLevelSyms(keywords: Iterable[String],
    maxResults: Int = 0): List[SymbolSearchResult] = {

    var resultSet = new HashSet[SymbolSearchResult]
    val keyword = keywords.head
    val key = keyword.toLowerCase()
    val caseSens = !keyword.equals(key)
    search(keywords, { (r: SymbolSearchResult) =>
      if (!caseSens || r.name.contains(keyword)) {
        resultSet += r
      }
    })

    val sorted = resultSet.toList.sortWith { (a, b) => a.name.length < b.name.length }
    if (maxResults == 0) {
      sorted
    } else {
      sorted.take(maxResults)
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

  def buildStaticIndex(files: Iterable[File],
    includes: Iterable[Regex],
    excludes: Iterable[Regex]) {
    val t = System.currentTimeMillis()

    sealed abstract trait IndexEvent
    case class ClassEvent(name: String,
      location: String, flags: Int) extends IndexEvent
    case class MethodEvent(className: String, name: String,
      location: String, flags: Int) extends IndexEvent
    case object StopEvent extends IndexEvent

    class IndexWorkQueue extends Actor {
      def act() {
        loop {
          receive {
            case ClassEvent(name: String, location: String, flags: Int) => {
              val i = name.lastIndexOf(".")
              val localName = if (i > -1) name.substring(i + 1) else name
              val value = new TypeSearchResult(name,
                localName,
                declaredAs(name, flags),
                Some((location, -1)))
              insert(name, value)
            }
            case MethodEvent(className: String, name: String,
              location: String, flags: Int) => {
              val isStatic = ((flags & Opcodes.ACC_STATIC) != 0)
              val revisedClassName = if (isStatic) className + "$"
              else className
              val lookupKey = revisedClassName + "." + name
              val value = new MethodSearchResult(lookupKey,
                name,
                'method,
                Some((location, -1)),
                revisedClassName)
              insert(lookupKey, value)
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
        validClass = (isPublic && Indexer.isValidType(name) && include(name,
          includes, excludes))
        if (validClass) {
          indexWorkQ ! ClassEvent(name, location, flags)
          classCount += 1
        }
      }
      override def onMethod(className: String, name: String,
        location: String, flags: Int) {
        val isPublic = ((flags & Opcodes.ACC_PUBLIC) != 0)
        if (validClass && isPublic && Indexer.isValidMethod(name)) {
          indexWorkQ ! MethodEvent(className, name, location, flags)
          methodCount += 1
        }
      }
    }

    println("Updated: Indexing classpath...")
    ClassIterator.find(files, handler)
    indexWorkQ !? StopEvent
    val elapsed = System.currentTimeMillis() - t
    println("Indexing completed in " + elapsed / 1000.0 + " seconds.")
    println("Indexed " + handler.classCount + " classes with " +
      handler.methodCount + " methods.")
    onIndexingComplete()
  }

  def onIndexingComplete()
}

object Indexer {
  def isValidType(s: String): Boolean = {
    val i = s.indexOf("$")
    i == -1 || (i == (s.length - 1))
  }
  def isValidMethod(s: String): Boolean = {
    s.indexOf("$") == -1 && !s.equals("<init>") && !s.equals("this")
  }
}

class Indexer(project: Project, protocol: ProtocolConversions,
  config: ProjectConfig) extends Actor with Indexing {

  import protocol._

  override def onIndexingComplete() {
    project ! AsyncEvent(toWF(IndexerReadyEvent()))
  }

  def act() {

    println("Initializing Indexer...")
    initialize()

    loop {
      try {
        receive {
          case IndexerShutdownReq() => {
            exit('stop)
          }
          case RebuildStaticIndexReq() => {
            buildStaticIndex(config.allFilesOnClasspath,
              config.onlyIncludeInIndex,
              config.excludeFromIndex)

            val t = System.currentTimeMillis()
            commitChanges()
            val elapsed = System.currentTimeMillis() - t
            println("Index changes committed in " + elapsed / 1000.0 +
              " seconds.")
          }
          case AddSymbolsReq(syms: Iterable[SymbolSearchResult]) => {
            syms.foreach { info =>
              insert(info.name, info)
            }
            commitChanges()
          }
          case RemoveSymbolsReq(syms: Iterable[String]) => {
            syms.foreach { s => remove(s) }
            commitChanges()
          }
          case TypeCompletionsReq(prefix: String, maxResults: Int) => {
            val suggestions = getImportSuggestions(List(prefix), maxResults).flatten
            sender ! suggestions
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              req match {
                case ImportSuggestionsReq(file: File, point: Int,
                  names: List[String], maxResults: Int) => {
                  val suggestions = ImportSuggestions(getImportSuggestions(names, maxResults))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
                case PublicSymbolSearchReq(keywords: List[String],
                  maxResults: Int) => {
                  val nonEmptyKeywords = keywords.filter { _.length > 0 }
                  val suggestions = SymbolSearchResults(
                    findTopLevelSyms(nonEmptyKeywords, maxResults))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
              }
            } catch {
              case e: Exception =>
                {
                  System.err.println("Error handling RPC: " +
                    e + " :\n" +
                    e.getStackTraceString)
                  project.sendRPCError(ErrExceptionInIndexer,
                    Some("Error occurred in indexer. Check the server log."),
                    callId)
                }
            }
          }
          case other =>
            {
              println("Indexer: WTF, what's " + other)
            }
        }

      } catch {
        case e: Exception => {
          System.err.println("Error at Indexer message loop: " +
            e + " :\n" + e.getStackTraceString)
        }
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing Indexer actor.")
  }
}

object IndexTest extends Indexing {
  def onIndexingComplete() {
    println("done")
  }
  def main(args: Array[String]) {
    val classpath = "/Users/aemon/.ivy2/cache/nekohtml/xercesMinimal/jars/xercesMinimal-1.9.6.2.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-repository-metadata/jars/maven-repository-metadata-2.2.1.jar:/Users/aemon/projects/ensime/lib/org.scala-refactoring_2.9.2-SNAPSHOT-0.5.0-SNAPSHOT.jar:/Users/aemon/projects/ensime/lib/critbit-0.0.4.jar:/Users/aemon/.ivy2/cache/org.apache.ant/ant/jars/ant-1.8.1.jar:/Users/aemon/.ivy2/cache/org.codehaus.plexus/plexus-container-default/jars/plexus-container-default-1.0-alpha-9-stable-1.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-error-diagnostics/jars/maven-error-diagnostics-2.2.1.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-artifact/jars/maven-artifact-2.2.1.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-settings/jars/maven-settings-2.2.1.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-profile/jars/maven-profile-2.2.1.jar:/Users/aemon/.ivy2/cache/org.codehaus.plexus/plexus-utils/jars/plexus-utils-1.5.15.jar:/Users/aemon/.ivy2/cache/classworlds/classworlds/jars/classworlds-1.1-alpha-2.jar:/Users/aemon/.ivy2/cache/asm/asm/jars/asm-3.2.jar:/Users/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-provider-api/jars/wagon-provider-api-1.0-beta-6.jar:/Users/aemon/.ivy2/cache/org.apache.ivy/ivy/jars/ivy-2.1.0.jar:/Users/aemon/.ivy2/cache/org.apache.ant/ant-launcher/jars/ant-launcher-1.8.1.jar:/Users/aemon/.ivy2/cache/ant/ant/jars/ant-1.6.5.jar:/Users/aemon/.ivy2/cache/backport-util-concurrent/backport-util-concurrent/jars/backport-util-concurrent-3.1.jar:/Users/aemon/.ivy2/cache/nekohtml/nekohtml/jars/nekohtml-1.9.6.2.jar:/Users/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-http-lightweight/jars/wagon-http-lightweight-1.0-beta-6.jar:/Users/aemon/.ivy2/cache/org.scalariform/scalariform_2.9.1/jars/scalariform_2.9.1-0.1.1.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-project/jars/maven-project-2.2.1.jar:/Users/aemon/.ivy2/cache/asm/asm-commons/jars/asm-commons-3.2.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-ant-tasks/jars/maven-ant-tasks-2.1.0.jar:/Users/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-file/jars/wagon-file-1.0-beta-6.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-plugin-registry/jars/maven-plugin-registry-2.2.1.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-model/jars/maven-model-2.2.1.jar:/Users/aemon/.ivy2/cache/org.scalatest/scalatest_2.9.1/jars/scalatest_2.9.1-1.6.1.jar:/Users/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-http-shared/jars/wagon-http-shared-1.0-beta-6.jar:/Users/aemon/.sbt/boot/scala-2.9.2-SNAPSHOT/lib/scala-compiler.jar:/Users/aemon/projects/ensime/target/scala-2.9.2-SNAPSHOT/ensime_2.9.2-SNAPSHOT-0.9.3.RC3-test.jar:/Users/aemon/projects/ensime/dist_2.9.2-RC1/lib/scala-library.jar:/Users/aemon/projects/ensime/dist_2.9.2-RC1/lib/scala-compiler.jar:/Users/aemon/.ivy2/cache/org.sonatype.tycho/org.eclipse.jdt.core/jars/org.eclipse.jdt.core-3.6.0.v_A58.jar:/Users/aemon/projects/ensime/target/scala-2.9.2-SNAPSHOT/ensime_2.9.2-SNAPSHOT-0.9.3.RC3.jar:/Users/aemon/.ivy2/cache/org.codehaus.plexus/plexus-interpolation/jars/plexus-interpolation-1.11.jar:/Users/aemon/.ivy2/cache/org.apache.maven/maven-artifact-manager/jars/maven-artifact-manager-2.2.1.jar:/Users/aemon/.sbt/boot/scala-2.9.2-SNAPSHOT/lib/scala-library.jar:/Users/aemon/.ivy2/cache/asm/asm-tree/jars/asm-tree-3.2.jar"
    val files = classpath.split(":").map { new File(_) }
    import java.util.Scanner
    val in = new Scanner(System.in)
    val name = in.nextLine()
    buildStaticIndex(files, List(), List())
    for (l <- getImportSuggestions(args, 5)) {
      for (s <- l) {
        println(s.name)
      }
    }
  }
}
