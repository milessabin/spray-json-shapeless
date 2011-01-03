package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.protocol.ProtocolConversions
import org.ensime.protocol.ProtocolConst._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import org.ensime.model.{SymbolInfo, TypeInfo, SymbolSearchResult}
import org.clapper.classutil._
import scala.collection.JavaConversions._
import org.ardverk.collection._
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.{ NoPosition }
import scala.collection.mutable.{ HashMap, ArrayBuffer }

case class IndexerShutdownReq()
case class RebuildStaticIndexReq()
case class AddSymbolsReq(syms: Iterable[(String, SymbolInfo)])
case class RemoveSymbolsReq(syms: Iterable[String])


trait IndexerInterface{ self : RichPresentationCompiler =>

  def unindexTopLevelSyms(syms: Iterable[Symbol]) {
    val keys = new ArrayBuffer[String]
    for(sym <- syms){
      for(name <- nameForIndex(sym)) keys += name
      for(mem <- try{sym.tpe.members}catch{case e => List()}){
	for(name <- nameForIndex(mem)) keys += name
      }
    }
    indexer ! RemoveSymbolsReq(keys)
  }

  def indexTopLevelSyms(syms: Iterable[Symbol]) {
    val infos = new ArrayBuffer[(String, SymbolInfo)]
    for(sym <- syms){
      for(name <- nameForIndex(sym)) {
	if(Indexer.isValidTypeName(name)){
	  infos += ((name, SymbolInfo(sym)))
	  for(mem <- try{sym.tpe.members}catch{case e => { List()}}){
	    for(mname <- nameForIndex(mem)) {
	      if(Indexer.isValidTypeName(mname)){
		infos += ((mname, SymbolInfo(mem)))
	      }
	    }
	  }
	}
      }
    }
    indexer ! AddSymbolsReq(infos)
  }

  private def nameForIndex(sym:Symbol):Option[String] = {
    try{
      if(sym.isMethod) Some(sym.nameString)
      else{
	val tpe = sym.tpe
	Some(typeFullName(tpe))
      }
    }catch{
      case e:AssertionError => None
    }
  }

}

object Indexer{
  def isValidTypeName(s:String):Boolean = {
    val i = s.indexOf("$")
    i == -1 || (i == (s.length - 1))
  }
  def isValidMethodName(s:String):Boolean = {
    s.indexOf("$") == 0
  }
}

class Indexer(project: Project, protocol: ProtocolConversions, config: ProjectConfig) extends Actor {

  import protocol._

  private var trie = new PatriciaTrie[String, SymbolInfo](
    StringKeyAnalyzer.INSTANCE)

  private var invalidated = true

  private def findTopLevelSyms(str: String, maxResults: Int = 0, caseSens: Boolean = false): List[SymbolInfo] = {
    if(invalidated) rebuildIndex()
    val results = trie.prefixMap(str.toLowerCase()).values
    val refined: Iterable[SymbolInfo] = if(caseSens) {
      results.filter{ s => s.name.contains(str) }
    }
    else{
      results
    }
    if(maxResults == 0){
      refined.toList
    }
    else{
      refined.toList.take(maxResults)
    }
  }

  private def insertSuffixes(key: String, value: SymbolInfo) {
    val tmp = key.toLowerCase()
    val k = tmp + tmp.hashCode()
    for(i <- (0 to key.length - 1)){
      trie.put(k.substring(i), value)
    }
  }

  private def removeSuffixes(key: String) {
    val tmp = key.toLowerCase()
    val k = tmp + tmp.hashCode()
    for(i <- (0 to key.length - 1)){
      trie.remove(k.substring(i))
    }
  }

  private def rebuildIndex() {
    println("Rebuilding index...")
    val t = System.currentTimeMillis()
    val finder = ClassFinder(config.allFilesOnClasspath.toList)
    val classes: Iterator[ClassInfo] = finder.getClasses

    // TODO: lets not use this
    def nullInfo = new TypeInfo("NA", -1, 'nil, "NA", List(), List(), NoPosition, None)

    def indexType(ci:ClassInfo){
      if(Indexer.isValidTypeName(ci.name)){
	ci.methods.foreach(indexMethod)
	insertSuffixes(ci.name,
	  new SymbolInfo(
	    ci.name,
	    NoPosition,
	    nullInfo,
	    false))
      }
    }

    def indexMethod(mi:MethodInfo){
      if(Indexer.isValidMethodName(mi.name)){
	insertSuffixes(mi.name,
	  new SymbolInfo(
	    mi.name,
	    NoPosition,
	    nullInfo,
	    true))
      }
    }

    classes.foreach{ ci =>
      indexType(ci)
    }
    invalidated = false

    val elapsed = System.currentTimeMillis() - t
    println("Indexing completed in " + elapsed/1000.0 + " seconds.")
  }


  def act() {

    println("Initializing Indexer...")

    loop {
      try {
	receive {
          case IndexerShutdownReq => {
            exit('stop)
          }
          case RebuildStaticIndexReq() => {
	    rebuildIndex()
          }
          case AddSymbolsReq(syms: Iterable[(String, SymbolInfo)]) => {
	    syms.foreach{
	      case (key, info) => {
		insertSuffixes(key, info)
	      }
	    }
          }
          case RemoveSymbolsReq(syms: Iterable[String]) => {
	    syms.foreach{ s => removeSuffixes(s) }
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
	      req match {
		case ImportSuggestionsReq(file: File, point: Int, names: List[String]) => {
                  val suggestions = SymbolSearchResult(
		    names.map{ nm => findTopLevelSyms(nm) })
                  project ! RPCResultEvent(toWF(suggestions), callId)
		}
		case PublicSymbolSearchReq(names: List[String], maxResults: Int, caseSens: Boolean) => {
                  val suggestions = SymbolSearchResult(
		    names.map{ nm => 
		      findTopLevelSyms(nm, maxResults, caseSens).sortWith{
			(a,b) => a.name.length < b.name.length
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
          System.err.println("Error at Indexer message loop: " + e + " :\n" + e.getStackTraceString)
	}
      }
    }
  }

  override def finalize() {
    System.out.println("Finalizing indexer actor.")
  }

}

