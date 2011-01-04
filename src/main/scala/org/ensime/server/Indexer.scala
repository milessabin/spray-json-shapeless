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
  SymbolSearchResults
}
import org.clapper.classutil._
import scala.collection.JavaConversions._
import org.ardverk.collection._
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.{ NoPosition }
import scala.collection.mutable.{ HashMap, ArrayBuffer }

case class IndexerShutdownReq()
case class RebuildStaticIndexReq()
case class AddSymbolsReq(syms: Iterable[(String, SymbolSearchResult)])
case class RemoveSymbolsReq(syms: Iterable[String])

trait IndexerInterface { self: RichPresentationCompiler =>

  private def isType(sym: Symbol): Boolean = {
    sym.isClass || sym.isModule || sym.isInterface
  }

  private def typeSymName(sym:Symbol):String = {
    try{
      typeFullName(sym.tpe)
    } catch{ case e => sym.nameString }
  }

  private def lookupKey(sym: Symbol):String = {
    if(isType(sym)) typeSymName(sym)
    else sym.nameString + typeSymName(sym.owner).hashCode()
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

  private implicit def symToSearchResult(sym:Symbol):SymbolSearchResult = {

    val pos = if(sym.pos.isDefined)
    { Some((sym.pos.source.path, sym.pos.point)) }
    else None

    if(isType(sym)) {
      new TypeSearchResult(
        typeSymName(sym),
	declaredAs(sym),
        pos)
    }
    else{
      new MethodSearchResult(
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
    s.indexOf("$") == -1
  }
}

class Indexer(project: Project, protocol: ProtocolConversions, config: ProjectConfig) extends Actor {

  import protocol._

  private var trie = new PatriciaTrie[String, SymbolSearchResult](
    StringKeyAnalyzer.INSTANCE)

  private var invalidated = true

  private def findTopLevelSyms(str: String, maxResults: Int = 0, caseSens: Boolean = false): List[SymbolSearchResult] = {
    if (invalidated) rebuildIndex()
    val key = str.toLowerCase()
    val results = trie.prefixMap(key).values
    val refined: Iterable[SymbolSearchResult] = if (caseSens) {
      results.filter { s => s.name.contains(str) }
    } else {
      results
    }
    if (maxResults == 0) {
      refined.toList
    } else {
      refined.toList.take(maxResults)
    }
  }

  private def insertSuffixes(key: String, value: SymbolSearchResult) {
    val tmp = key.toLowerCase()
    val k = tmp + tmp.hashCode()
    trie.put(k, value)
    for (i <- (1 to key.length - 1)) {
      val c:Char = key.charAt(i)
      if(c == '.' || c == '_'){
	trie.put(k.substring(i), value)	
	trie.put(k.substring(i + 1), value)
      }
      else if(Character.isUpperCase(c)){
	trie.put(k.substring(i), value)	
      }
    }
  }

  private def removeSuffixes(key: String) {
    val tmp = key.toLowerCase()
    val k = tmp + tmp.hashCode()
    trie.remove(k)
    for (i <- (1 to key.length - 1)) {
      val c:Char = key.charAt(i)
      if(c == '.' || c == '_'){
	trie.remove(k.substring(i))
	trie.remove(k.substring(i + 1))
      }
      else if(Character.isUpperCase(c)){
	trie.remove(k.substring(i))	
      }
    }
  }

  private def declaredAs(ci: ClassInfo):scala.Symbol = {
    if(ci.name.endsWith("$")) 'object
    else if(ci.isInterface) 'trait
    else 'class
  }

  private def declaredAs(mi: MethodInfo):scala.Symbol = 'method

  private def lookupKey(ci: ClassInfo):String = ci.name
  private def lookupKey(owner: ClassInfo, mi: MethodInfo):String = {
    mi.name + owner.name.hashCode()
  }

  private def rebuildIndex() {
    println("Rebuilding index...")
    val t = System.currentTimeMillis()
    val finder = ClassFinder(config.allFilesOnClasspath.toList)
    val classes: Iterator[ClassInfo] = finder.getClasses

    for(ci <- classes){
      if (Indexer.isValidType(ci.name)) {
        for (mi <- ci.methods) {
	  if (Indexer.isValidMethod(mi.name)) {
	    val value = new MethodSearchResult(
              mi.name,
	      declaredAs(mi),
              Some((ci.location.getPath(),-1)),
              ci.name)
            insertSuffixes(lookupKey(ci, mi), value)
          }
	}
	val value = new TypeSearchResult(
          ci.name,
	  declaredAs(ci),
          Some((ci.location.getPath(), -1)))
        insertSuffixes(lookupKey(ci), value)
      }
    }
    invalidated = false

    val elapsed = System.currentTimeMillis() - t
    println("Indexing completed in " + elapsed / 1000.0 + " seconds.")
  }

  def act() {

    println("Initializing Indexer...")

    loop {
      try {
        receive {
          case IndexerShutdownReq() => {
            exit('stop)
          }
          case RebuildStaticIndexReq() => {
            rebuildIndex()
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
                  val suggestions = SymbolSearchResults(
                    names.map { nm => findTopLevelSyms(nm) })
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
                case PublicSymbolSearchReq(names: List[String], 
		  maxResults: Int, caseSens: Boolean) => {
                  val suggestions = SymbolSearchResults(
                    names.map { nm =>
		      findTopLevelSyms(nm, maxResults, caseSens).sortWith { (a, b) =>
                        a.name.length < b.name.length
		      }
                    })
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
        case e: Exception =>
        {
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

