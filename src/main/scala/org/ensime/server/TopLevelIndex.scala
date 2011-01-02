package org.ensime.server
import org.ensime.util._
import scala.collection.{ immutable, mutable }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.{ Global }
import org.ardverk.collection._
import scala.collection.JavaConversions._

trait TopLevelIndex { self: RichPresentationCompiler =>

  private var trie = new PatriciaTrie[String, Symbol](
    StringKeyAnalyzer.INSTANCE)

  private var invalidated = true

  def invalidateTopLevelIndex(){ invalidated = true }

  def findTopLevelSyms(str: String): List[Symbol] = {
    if(invalidated) rebuildIndex()
    val results: Iterable[Symbol] = trie.prefixMap(str).values
    results.toList
  }

  private def insertSuffixes(key: String, value: Symbol) {
    for(i <- (0 to key.length - 1)){
      trie.put(key.substring(i), value)
    }
  }

  import definitions.RootPackage

  def rebuildIndex() {
    trie = new PatriciaTrie[String, Symbol](StringKeyAnalyzer.INSTANCE)
    traverse(new NamespaceVisitor{
	def visitPackage(s:Symbol) {}
	def visitType(s:Symbol) {
	  val name = typeFullName(s.tpe)
	  insertSuffixes(name, s)
	}
      }, 
      RootPackage)
    invalidated = false
  }

  def add(units: Iterable[CompilationUnit]) {}
  def remove(symbols: Iterable[Symbol]) {}

}
