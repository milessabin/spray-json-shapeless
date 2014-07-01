package org.ensime.server

import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.indexer.ClassFileIndex
import org.ensime.indexer.LuceneIndex
import org.ensime.model.{
  ImportSuggestions,
  MethodSearchResult,
  SymbolSearchResult,
  SymbolSearchResults,
  TypeSearchResult
}
import org.ensime.protocol.ProtocolConst._
import org.ensime.protocol.ProtocolConversions
import scala.actors._
import scala.collection.mutable.ArrayBuffer

case class IndexerShutdownReq()
case class RebuildStaticIndexReq()
case class TypeCompletionsReq(prefix: String, maxResults: Int)
case class SourceFileCandidatesReq(enclosingPackage: String, classNamePrefix: String)
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

  val index = new LuceneIndex {}
  val classFileIndex = new ClassFileIndex(config)

  def act() {
    loop {
      try {
        receive {
          case IndexerShutdownReq() =>
            index.close()
            exit('stop)
          case RebuildStaticIndexReq() =>
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
          case ReindexClassFilesReq(files) =>
            classFileIndex.indexFiles(files)
          case CommitReq() =>
            index.commit()
          case AddSymbolsReq(syms) =>
            syms.foreach { info =>
              index.insert(info)
            }
          case RemoveSymbolsReq(syms) =>
            syms.foreach { s => index.remove(s) }
          case TypeCompletionsReq(prefix: String, maxResults: Int) =>
            val suggestions = index.keywordSearch(List(prefix), maxResults, restrictToTypes = true)
            sender ! suggestions
          case SourceFileCandidatesReq(enclosingPackage, classNamePrefix) =>
            sender ! classFileIndex.sourceFileCandidates(
              enclosingPackage,
              classNamePrefix)
          case RPCRequestEvent(req: Any, callId: Int) =>
            try {
              req match {
                case ImportSuggestionsReq(file, point, names, maxResults) =>
                  val suggestions = ImportSuggestions(index.getImportSuggestions(
                    names, maxResults))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                case PublicSymbolSearchReq(keywords, maxResults) =>
                  println("Received keywords: " + keywords)
                  val suggestions = SymbolSearchResults(
                    index.keywordSearch(keywords, maxResults))
                  project ! RPCResultEvent(toWF(suggestions), callId)
                case MethodBytecodeReq(sourceName: String, line: Int) =>
                  classFileIndex.locateBytecode(sourceName, line) match {
                    case method :: rest =>
                      project ! RPCResultEvent(
                        toWF(method), callId)
                    case _ => project.sendRPCError(ErrExceptionInIndexer,
                      Some("Failed to find method bytecode"), callId)
                  }
              }
            } catch {
              case e: Exception =>
                System.err.println("Error handling RPC: " +
                  e + " :\n" +
                  e.getStackTraceString)
                project.sendRPCError(ErrExceptionInIndexer,
                  Some("Error occurred in indexer. Check the server log."),
                  callId)
            }
          case other =>
            println("Indexer: WTF, what's " + other)
        }

      } catch {
        case e: Exception =>
          System.err.println("Error at Indexer message loop: " +
            e + " :\n" + e.getStackTraceString)
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
    } catch { case e: Throwable => sym.nameString }
  }

  private def lookupKey(sym: Symbol): String = {
    if (isType(sym)) typeSymName(sym)
    else typeSymName(sym.owner) + "." + sym.nameString
  }

  def unindexTopLevelSyms(syms: Iterable[Symbol]) {
    val keys = new ArrayBuffer[String]
    for (sym <- syms) {
      keys += lookupKey(sym)
      for (mem <- try { sym.tpe.members } catch { case e: Throwable => List() }) {
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
        for (mem <- try { sym.tpe.members } catch { case e: Throwable => List() }) {
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

