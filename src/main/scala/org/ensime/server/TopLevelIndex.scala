package org.ensime.server
import org.ensime.util._
import org.ensime.model.{SymbolInfo, TypeInfo}
import scala.collection.{ immutable, mutable }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.{ Global }
import org.ardverk.collection._
import scala.collection.JavaConversions._
import org.clapper.classutil._
import scala.tools.nsc.util.{ OffsetPosition }

trait TopLevelIndex { self: RichPresentationCompiler =>

  private var trie = new PatriciaTrie[String, SymbolInfo](
    StringKeyAnalyzer.INSTANCE)

  private var invalidated = true

  def invalidateTopLevelIndex(){ invalidated = true }

  def findTopLevelSyms(str: String): List[SymbolInfo] = {
    if(invalidated) rebuildIndex()
    val results: Iterable[SymbolInfo] = trie.prefixMap(str).values
    results.toList
  }

  def unindexTopLevelSyms(syms: Iterable[Symbol]) {
    for(sym <- syms){
      unindexSym(sym)
      for(mem <- try{sym.tpe.members}catch{case e => List()}){
	unindexSym(mem)
      }
    }
  }

  private def unindexSym(sym:Symbol){
    println("Un-indexing: " + sym)
    for(name <- nameForIndex(sym)){
      removeSuffixes(name)
    }
  }

  def indexTopLevelSyms(syms: Iterable[Symbol]) {
    for(sym <- syms){
      indexSym(sym)
      for(mem <- try{sym.tpe.members}catch{case e => { List()}}){
	indexSym(mem)
      }
    }
  }

  private def indexSym(sym:Symbol){
    println("Indexing: " + sym)
    for(name <- nameForIndex(sym)){
      insertSuffixes(name, SymbolInfo(sym))
    }
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

  private def insertSuffixes(key: String, value: SymbolInfo) {
    val k = key + key.hashCode()
    for(i <- (0 to key.length - 1)){
      trie.put(k.substring(i), value)
    }
  }

  private def removeSuffixes(key: String) {
    val k = key + key.hashCode()
    for(i <- (0 to key.length - 1)){
      trie.remove(k.substring(i))
    }
  }

  import definitions.RootPackage

  def rebuildIndex() {
    println("Rebuilding index...")
    val t = System.currentTimeMillis()
    val finder = ClassFinder(config.allFilesOnClasspath.toList)
    val classes: Iterator[ClassInfo] = finder.getClasses

    def indexType(ci:ClassInfo){
      ci.methods.foreach(indexMethod)
      insertSuffixes(ci.name,
	new SymbolInfo(
	  ci.name,
	  NoPosition,
	  TypeInfo.nullInfo,
	  false))
    }

    def indexMethod(mi:MethodInfo){
      insertSuffixes(mi.name,
	new SymbolInfo(
	  mi.name,
	  NoPosition,
	  TypeInfo.nullInfo,
	  true))
    }

    classes.foreach{ ci =>
      val i = ci.name.indexOf("$")
      if(i > -1){
	if(i == ci.name.length - 1){
	  indexType(ci)
	}
      }
      else{
	indexType(ci)
      }
    }
    invalidated = false

    val elapsed = System.currentTimeMillis() - t
    println("Indexing completed in " + elapsed/1000.0 + " seconds.")
  }

  def add(units: Iterable[CompilationUnit]) {}
  def remove(symbols: Iterable[Symbol]) {}

}
