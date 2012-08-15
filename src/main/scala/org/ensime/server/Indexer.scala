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

import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.indexer.ClassFileIndex
import org.ensime.indexer.LuceneIndex
import org.objectweb.asm.ClassReader
import org.ensime.model.{
  ImportSuggestions, MethodSearchResult, SymbolSearchResult,
  SymbolSearchResults, TypeInfo, TypeSearchResult}
import org.ensime.protocol.ProtocolConst._
import org.ensime.protocol.ProtocolConversions
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable.{ArrayBuffer, HashSet, ListBuffer}

case class IndexerShutdownReq()
case class RebuildStaticIndexReq()
case class TypeCompletionsReq(prefix: String, maxResults: Int)
case class SourceFileCandidatesReq(enclosingPackage: String,
  classNamePrefix: String)
case class AddSymbolsReq(syms: Iterable[SymbolSearchResult])
case class RemoveSymbolsReq(syms: Iterable[String])
case class ReindexClassFilesReq(files: Iterable[File])
case class CommitReq()

/**
 * The main index actor.
 */
class Indexer(
  project: Project,
  protocol: ProtocolConversions,
  config: ProjectConfig) extends Actor {

  import protocol._

  val index = new LuceneIndex{}
  val classFileIndex = new ClassFileIndex(config)

  def act() {
    loop {
      try {
        receive {
          case IndexerShutdownReq() => {
            index.close()
            exit('stop)
          }
          case RebuildStaticIndexReq() => {
            index.initialize(
              config.root,
              config.allFilesOnClasspath,
              config.onlyIncludeInIndex,
              config.excludeFromIndex)
	    classFileIndex.indexFiles(
	      config.allFilesOnClasspath
	      ++ List(config.target, config.testTarget).flatten
	    )
	    project ! AsyncEvent(toWF(IndexerReadyEvent()))
          }
          case ReindexClassFilesReq(files: Iterable[File]) => {
	    classFileIndex.indexFiles(files)
	  }
          case CommitReq() => {
            index.commit()
          }
          case AddSymbolsReq(syms: Iterable[SymbolSearchResult]) => {
            syms.foreach { info =>
              index.insert(info)
            }
          }
          case RemoveSymbolsReq(syms: Iterable[String]) => {
            syms.foreach { s => index.remove(s) }
          }
          case TypeCompletionsReq(prefix: String, maxResults: Int) => {
            val suggestions = index.keywordSearch(List(prefix), maxResults, true)
	    sender ! suggestions
          }
          case SourceFileCandidatesReq(enclosingPackage, classNamePrefix) => {
	    sender ! classFileIndex.sourceFileCandidates(
	      enclosingPackage,
	      classNamePrefix)
          }
          case RPCRequestEvent(req: Any, callId: Int) => {
            try {
              req match {
                case ImportSuggestionsReq(file: File, point: Int,
                  names: List[String], maxResults: Int) => {
                  val suggestions = ImportSuggestions(index.getImportSuggestions(
                    names, maxResults))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
                case PublicSymbolSearchReq(keywords: List[String],
                  maxResults: Int) => {
                  println("Received keywords: " + keywords)
                  val suggestions = SymbolSearchResults(
                    index.keywordSearch(keywords, maxResults))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                }
                case MethodBytecodeReq(sourceName: String, line: Int) => {
		  classFileIndex.locateBytecode(sourceName, line) match{
		    case method :: rest =>
		      project ! RPCResultEvent(
		      toWF(method), callId)
		    case _ => project.sendRPCError(ErrExceptionInIndexer,
                      Some("Failed to find method bytecode"), callId)
		  }
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

/**
 * Main IDE interface to the Indexer.
 */

object Indexer {
  def isValidType(s: String): Boolean = {
    LuceneIndex.isValidType(s)
  }
  def isValidMethod(s: String): Boolean = {
    LuceneIndex.isValidMethod(s)
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
    } catch { case e : Throwable => sym.nameString }
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
      TypeSearchResult(
        lookupKey(sym),
        sym.nameString,
        declaredAs(sym),
        pos)
    } else {
      MethodSearchResult(
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




