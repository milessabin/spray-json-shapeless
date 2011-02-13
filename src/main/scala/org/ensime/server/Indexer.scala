package org.ensime.server
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
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.{ NoPosition }
import scala.collection.mutable.{ HashMap, HashSet, ArrayBuffer, ListBuffer }
import org.objectweb.asm.Opcodes;

case class IndexerShutdownReq()
case class RebuildStaticIndexReq()
case class AddSymbolsReq(syms: Iterable[(String, SymbolSearchResult)])
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
        typeSymName(sym),
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
    val infos = new ArrayBuffer[(String, SymbolSearchResult)]
    for (sym <- syms) {
      if (Indexer.isValidType(typeSymName(sym))) {
        val key = lookupKey(sym)
        infos += ((key, sym))
        for (mem <- try { sym.tpe.members } catch { case e => { List() } }) {
          if (Indexer.isValidMethod(mem.nameString)) {
            val key = lookupKey(mem)
            infos += ((key, mem))
          }
        }
      }
    }
    indexer ! AddSymbolsReq(infos)
  }
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

trait Indexing extends StringSimilarity {

  protected val trie = new PatriciaTrie[String, SymbolSearchResult](
    StringKeyAnalyzer.INSTANCE)

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
        for (v <- trie.prefixMap(key.toLowerCase()).values) {
          v match {
            case r: TypeSearchResult => candidates += v
            case _ =>
          }
        }
      }

      // Sort by edit distance of type name primarily, and 
      // length of full name secondarily.
      val candidates2 = candidates.toList.sortWith { (a, b) =>
	val d1 = editDist(a.localName, typeName)
	val d2 = editDist(b.localName, typeName)
	if(d1 == d2) a.name.length < b.name.length
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

  protected def findTopLevelSyms(keywords: Iterable[String],
    maxResults: Int = 0, caseSens: Boolean = false): List[SymbolSearchResult] = {

    var resultSet = new HashSet[SymbolSearchResult]
    if (keywords.size() > 0) {
      val key = keywords.head.toLowerCase()
      resultSet ++= trie.prefixMap(key).values
    }

    if (keywords.size() > 1) {
      for (keyword <- keywords.tail) {
        val key = keyword.toLowerCase()
        val results = trie.prefixMap(key).values.toSet
        resultSet = resultSet.intersect(results)
      }
    }

    resultSet = if (caseSens) {
      resultSet.filter { s => keywords.forall { k => s.name.contains(k) } }
    } else {
      resultSet
    }

    val sorted = resultSet.toList.sortWith { (a, b) => a.name.length < b.name.length }
    if (maxResults == 0) {
      sorted
    } else {
      sorted.take(maxResults)
    }

  }

  protected def insertSuffixes(key: String, value: SymbolSearchResult) {
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

  protected def removeSuffixes(key: String) {
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

  private def declaredAs(name: String, flags: Int) = {
    if (name.endsWith("$")) 'object
    else if ((flags & Opcodes.ACC_INTERFACE) != 0) 'trait
    else 'class
  }

  def buildStaticIndex(files: Iterable[File]) {
    val t = System.currentTimeMillis()

    val handler = new ClassHandler {
      var classCount = 0
      var methodCount = 0
      var validClass = false
      override def onClass(name: String, location: String, flags: Int) {
        val isPublic = ((flags & Opcodes.ACC_PUBLIC) != 0)
        if (isPublic && Indexer.isValidType(name)) {
          validClass = true
          val i = name.lastIndexOf(".")
          val localName = if (i > -1) name.substring(i + 1) else name
          val value = new TypeSearchResult(
            name,
            localName,
            declaredAs(name, flags),
            Some((location, -1)))
          insertSuffixes(name, value)
          classCount += 1
        } else validClass = false
      }
      override def onMethod(className: String, name: String,
        location: String, flags: Int) {
        val isPublic = ((flags & Opcodes.ACC_PUBLIC) != 0)
        if (validClass && isPublic && Indexer.isValidMethod(name)) {
          val isStatic = ((flags & Opcodes.ACC_STATIC) != 0)
          val revisedClassName = if (isStatic) className + "$"
          else className
          val lookupKey = revisedClassName + "." + name
          val value = new MethodSearchResult(
            lookupKey,
            name,
            'method,
            Some((location, -1)),
            revisedClassName)
          insertSuffixes(lookupKey, value)
          methodCount += 1
        }
      }
    }

    println("Indexing classpath...")
    ClassIterator.find(files.toList, handler)
    val elapsed = System.currentTimeMillis() - t
    println("Indexing completed in " + elapsed / 1000.0 + " seconds.")
    println("Indexed " + handler.classCount + " classes with " + handler.methodCount + " methods.")
  }

}

class Indexer(project: Project, protocol: ProtocolConversions, config: ProjectConfig) extends Actor with Indexing {

  import protocol._

  def act() {

    println("Initializing Indexer...")

    loop {
      try {
        receive {
          case IndexerShutdownReq() => {
            exit('stop)
          }
          case RebuildStaticIndexReq() => {
            buildStaticIndex(config.allFilesOnClasspath)
          }
          case AddSymbolsReq(syms: Iterable[(String, SymbolSearchResult)]) => {
            syms.foreach {
              case (key, info) => {
                insertSuffixes(key, info)
              }
            }
          }
          case RemoveSymbolsReq(syms: Iterable[String]) => {
            syms.foreach { s => removeSuffixes(s) }
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              req match {
                case ImportSuggestionsReq(file: File, point: Int, names: List[String]) => {
                  val suggestions = ImportSuggestions(getImportSuggestions(names, 5))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
                case PublicSymbolSearchReq(keywords: List[String],
                  maxResults: Int, caseSens: Boolean) => {
                  val nonEmptyKeywords = keywords.filter { _.length > 0 }
                  val suggestions = SymbolSearchResults(
                    findTopLevelSyms(nonEmptyKeywords, maxResults, caseSens))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
              }
            } catch {
              case e: Exception =>
                {
                System.err.println("Error handling RPC: " +
                  e + " :\n" +
                  e.getStackTraceString)
                project ! RPCErrorEvent(ErrExceptionInIndexer,
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
  def main(args: Array[String]) {
    val classpath = "/usr/lib/jvm/java-6-openjdk/jre/lib/resources.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/rt.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/jsse.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/jce.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/charsets.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/rhino.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/ext/dnsns.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/ext/pulse-java.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/ext/sunjce_provider.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/ext/sunpkcs11.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/ext/localedata.jar:/usr/lib/jvm/java-6-openjdk/jre/lib/ext/gnome-java-bridge.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-project-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/scalatest-1.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/ivy-2.1.0.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/backport-util-concurrent-3.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/asm-tree-3.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/classworlds-1.1-alpha-2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/plexus-container-default-1.0-alpha-9-stable-1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/plexus-interpolation-1.11.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-plugin-registry-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/ant-1.6.5.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-repository-metadata-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/nekohtml-1.9.6.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/nekohtml-1.9.6.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/wagon-http-shared-1.0-beta-6.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-ant-tasks-2.1.0.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-project-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/core-3.4.2.v_883_R34x.jar:/home/aemon/src/misc/ensime/dist/lib/implicitNotFound.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/backport-util-concurrent-3.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-plugin-registry-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/scalariform_2.8.0-0.0.7.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-artifact-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/ant-1.6.5.jar:/home/aemon/src/misc/ensime/lib/org.scala-refactoring.library_0.3.0.201101021636.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/asm-commons-3.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/xercesMinimal-1.9.6.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/ant-1.8.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/wagon-http-shared-1.0-beta-6.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/wagon-http-lightweight-1.0-beta-6.jar:/home/aemon/src/misc/ensime/project/boot/scala-2.8.1/lib/scala-compiler.jar:/home/aemon/src/misc/ensime/lib/patricia-trie-0.3.jar:/home/aemon/src/misc/ensime/project/boot/scala-2.8.1/lib/scala-library.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/wagon-file-1.0-beta-6.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-settings-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/wagon-provider-api-1.0-beta-6.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/plexus-interpolation-1.11.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-profile-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-model-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-repository-metadata-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-error-diagnostics-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-artifact-manager-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/ant-1.8.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/wagon-file-1.0-beta-6.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/ivy-2.1.0.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/core-3.4.2.v_883_R34x.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/scalariform_2.8.0-0.0.7.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/plexus-utils-1.5.15.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-settings-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/ant-launcher-1.8.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-profile-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/plexus-utils-1.5.15.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/asm-3.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/wagon-http-lightweight-1.0-beta-6.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-artifact-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/maven-model-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/plexus-container-default-1.0-alpha-9-stable-1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-ant-tasks-2.1.0.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/ant-launcher-1.8.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/wagon-provider-api-1.0-beta-6.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/xercesMinimal-1.9.6.2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/compile/classworlds-1.1-alpha-2.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-error-diagnostics-2.2.1.jar:/home/aemon/src/misc/ensime/lib_managed/scala_2.8.1/test/maven-artifact-manager-2.2.1.jar"
    val files = classpath.split(":").map { new File(_) }
    import java.util.Scanner
    val in = new Scanner(System.in)
    val name = in.nextLine()
    buildStaticIndex(files)
    for (l <- getImportSuggestions(args, 5)) {
      for (s <- l) {
        println(s.name)
      }
    }
  }
}
