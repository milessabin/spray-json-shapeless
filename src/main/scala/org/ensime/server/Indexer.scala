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

import scala.Char
import scala.actors.Actor._
import scala.actors._
import scala.collection.JavaConversions
import scala.collection.mutable.{ HashMap, HashSet, ArrayBuffer, ListBuffer }
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.util.{ NoPosition }
import scala.util.matching.Regex

import com.codahale.jerkson.Json
import io.prelink.critbit.MCritBitTree
import java.io.File
import java.io.IOException
import java.io.Reader
import org.ardverk.collection._
import org.eclipse.jdt.core.compiler.CharOperation
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileReader
import org.objectweb.asm.Opcodes;

import org.ensime.config.ProjectConfig
import org.ensime.model.{
  ImportSuggestions,
  MethodSearchResult,
  SymbolSearchResult,
  SymbolSearchResults,
  TypeInfo,
  TypeSearchResult
}
import org.ensime.protocol.ProtocolConst._
import org.ensime.protocol.ProtocolConversions
import org.ensime.util._
import org.ensime.indexer.LuceneIndex

/**
 * Main IDE interface to the Indexer.
 */
trait Indexing extends LuceneIndex {

  import LuceneIndex._

  protected def getImportSuggestions(typeNames: Iterable[String],
    maxResults: Int = 0): List[List[SymbolSearchResult]] = {
    def suggestions(typeName: String): List[SymbolSearchResult] = {
      val keywords = splitTypeName(typeName)
      val candidates = new HashSet[SymbolSearchResult]

      for (key <- keywords) {
        search(List(key.toLowerCase), true,
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
    search(keywords, false,
      { (r: SymbolSearchResult) =>
        resultSet += r
        println("Result: " + r.name)
      })
    val sorted = resultSet.toList.sortWith {
      (a, b) => a.name.length < b.name.length
    }
    if (maxResults == 0) {
      sorted
    } else {
      sorted.take(maxResults)
    }
  }

  def onIndexingComplete()

}


object Indexer {
  def isValidType(s: String): Boolean = {
    LuceneIndex.isValidType(s)
  }
  def isValidMethod(s: String): Boolean = {
    LuceneIndex.isValidMethod(s)
  }
}


case class IndexerShutdownReq()
case class RebuildStaticIndexReq()
case class TypeCompletionsReq(prefix: String, maxResults: Int)
case class AddSymbolsReq(syms: Iterable[SymbolSearchResult])
case class RemoveSymbolsReq(syms: Iterable[String])
case class CommitReq()

/**
 * The main indexer actor.
 */
class Indexer(
  project: Project,
  protocol: ProtocolConversions,
  config: ProjectConfig) extends Actor with Indexing {

  import protocol._

  override def onIndexingComplete() {
    project ! AsyncEvent(toWF(IndexerReadyEvent()))
  }

  def act() {
    loop {
      try {
        receive {
          case IndexerShutdownReq() => {
            close()
            exit('stop)
          }
          case RebuildStaticIndexReq() => {
            initialize(
              config.root,
              config.allFilesOnClasspath,
              config.onlyIncludeInIndex,
              config.excludeFromIndex)
          }
          case CommitReq() => {
	    commit()
          }
          case AddSymbolsReq(syms: Iterable[SymbolSearchResult]) => {
            syms.foreach { info =>
              insert(info)
            }
          }
          case RemoveSymbolsReq(syms: Iterable[String]) => {
            syms.foreach { s => remove(s) }
          }
          case TypeCompletionsReq(prefix: String, maxResults: Int) => {
            val suggestions = getImportSuggestions(
              List(prefix), maxResults).flatten
            sender ! suggestions
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              req match {
                case ImportSuggestionsReq(file: File, point: Int,
                  names: List[String], maxResults: Int) => {
                  val suggestions = ImportSuggestions(getImportSuggestions(
                    names, maxResults))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
                case PublicSymbolSearchReq(keywords: List[String],
                  maxResults: Int) => {
                  println("Received keywords: " + keywords)
                  val suggestions = SymbolSearchResults(
                    findTopLevelSyms(keywords, maxResults))
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


/**
 * Helper mixin for interfacing compiler (Symbols)
 * with the indexer.
 */
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
    val pos = if (sym.pos.isDefined) {
      Some((sym.pos.source.path, sym.pos.point))
    } else None

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

