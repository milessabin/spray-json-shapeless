package org.ensime.server

import java.io.File
import akka.actor.{ ActorLogging, Actor }
import org.ensime.config.ProjectConfig
import org.ensime.indexer.ClassFileIndex
import org.ensime.indexer.LuceneIndex
import scala.reflect.io.AbstractFile
import org.ensime.model.{
  ImportSuggestions,
  MethodSearchResult,
  SymbolSearchResult,
  SymbolSearchResults,
  TypeSearchResult
}
import org.ensime.protocol.ProtocolConst._
import org.ensime.protocol.{ IndexerReadyEvent, ProtocolConversions }
import scala.collection.mutable.ArrayBuffer

case object IndexerShutdownReq
case object RebuildStaticIndexReq
case class TypeCompletionsReq(prefix: String, maxResults: Int)
case class SourceFileCandidatesReq(enclosingPackage: String, classNamePrefix: String)
case class AddSymbolsReq(syms: Iterable[SymbolSearchResult])
case class RemoveSymbolsReq(syms: Iterable[String])
case class ReindexClassFilesReq(files: Iterable[File])
case object CommitReq

case class AbstractFiles(files: Set[AbstractFile])

/**
 * The main index actor.
 */
class Indexer(project: Project,
    cacheDir: File,
    protocol: ProtocolConversions,
    config: ProjectConfig) extends Actor with ActorLogging {

  import protocol._

  val index = new LuceneIndex(cacheDir)
  val classFileIndex = new ClassFileIndex(config)

  override def receive = {
    case msg: Any => try {
      process(msg)
    } catch {
      case e: Exception =>
        log.error(e, "Error at Indexer message loop: " + e)
    }
  }

  def process(msg: Any) {
    msg match {
      case IndexerShutdownReq =>
        index.close()
        context.stop(self)
      case RebuildStaticIndexReq =>
        index.initialize(
          context.system,
          config.root,
          config.allFilesOnClasspath,
          config.onlyIncludeInIndex,
          config.excludeFromIndex)
        classFileIndex.indexFiles(
          config.allFilesOnClasspath
            ++ List(config.target, config.testTarget).flatten
        )
        project ! IndexerReadyEvent
      case ReindexClassFilesReq(files) =>
        classFileIndex.indexFiles(files)
      case CommitReq =>
        index.commit()
      case AddSymbolsReq(syms) =>
        syms.foreach { info =>
          index.insert(info)
        }
      case RemoveSymbolsReq(syms) =>
        syms.foreach { s => index.remove(s) }
      case TypeCompletionsReq(prefix: String, maxResults: Int) =>
        val suggestions = index.keywordSearch(List(prefix), maxResults, restrictToTypes = true)
        sender ! SymbolSearchResults(suggestions)
      case SourceFileCandidatesReq(enclosingPackage, classNamePrefix) =>
        sender ! AbstractFiles(classFileIndex.sourceFileCandidates(enclosingPackage, classNamePrefix))
      case RPCRequestEvent(req: Any, callId: Int) =>
        try {
          req match {
            case ImportSuggestionsReq(file, point, names, maxResults) =>
              val suggestions = ImportSuggestions(index.getImportSuggestions(
                names, maxResults))
              project ! RPCResultEvent(toWF(suggestions), callId)
            case PublicSymbolSearchReq(keywords, maxResults) =>
              log.info("Received keywords: " + keywords)
              val suggestions = SymbolSearchResults(
                index.keywordSearch(keywords, maxResults))
              project ! RPCResultEvent(toWF(suggestions), callId)
            case MethodBytecodeReq(sourceName: String, line: Int) =>
              classFileIndex.locateBytecode(sourceName, line) match {
                case method :: rest =>
                  project ! RPCResultEvent(
                    toWF(method), callId)
                case _ => project.sendRPCError(ErrExceptionInIndexer, "Failed to find method bytecode", callId)
              }
          }
        } catch {
          case e: Exception =>
            log.error(e, "Error handling RPC: " + req)
            project.sendRPCError(ErrExceptionInIndexer, "Error occurred in indexer. Check the server log.", callId)
        }
      case other =>
        log.warning("Indexer: WTF, what's " + other)
    }
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
      for (mem <- try { sym.tpe.members } catch { case e: Throwable => List.empty }) {
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
        for (mem <- try { sym.tpe.members } catch { case e: Throwable => List.empty }) {
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

