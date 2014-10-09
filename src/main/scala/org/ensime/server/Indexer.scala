package org.ensime.server

import akka.actor.{ Actor, ActorLogging }
import java.io.File
import java.net.URI
import org.apache.commons.vfs2.FileObject
import org.ensime.config.EnsimeConfig
import org.ensime.indexer.DatabaseService.FqnSymbol
import org.ensime.indexer.MemberName
import org.ensime.indexer.SearchService
import org.ensime.model._
import org.ensime.protocol.ProtocolConst._
import org.ensime.protocol.ProtocolConversions
import scala.reflect.io.{ AbstractFile, ZipArchive }

// legacy types
case class TypeCompletionsReq(prefix: String, maxResults: Int)
case class SourceFileCandidatesReq(enclosingPackage: String, classNamePrefix: String)
case class AbstractFiles(files: Set[AbstractFile])

//@deprecated("there is no good reason for this to be an actor, plus it enforces single-threaded badness", "fommil")
class Indexer(
    config: EnsimeConfig,
    index: SearchService,
    project: Project,
    protocol: ProtocolConversions) extends Actor with ActorLogging {

  import protocol._

  private def typeResult(hit: FqnSymbol) = TypeSearchResult(
    hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
    LineSourcePosition.fromFqnSymbol(hit)(config)
  )

  def oldSearchTypes(query: String, max: Int) =
    index.searchClasses(query, max).filterNot {
      name => name.fqn.endsWith("$") || name.fqn.endsWith("$class")
    }.map(typeResult)

  def oldSearchSymbols(query: String, max: Int) =
    index.searchClassesFieldsMethods(query, max).flatMap {
      case hit if hit.declAs == 'class => Some(typeResult(hit))
      case hit if hit.declAs == 'method => Some(MethodSearchResult(
        hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
        LineSourcePosition.fromFqnSymbol(hit)(config),
        hit.fqn.split("\\.").init.mkString(".")
      ))
      case _ => None // were never supported
    }

  def toAbstractFile(f: FileObject): AbstractFile = {
    val name = f.getName.getURI
    if (name.startsWith("jar") || name.startsWith("zip"))
      ZipArchive.fromURL(f.getURL)
    else
      AbstractFile.getFile(new File(new URI(name)))
  }

  override def receive = {
    case SourceFileCandidatesReq(enclosingPackage, classNamePrefix) =>
      val classes = index.searchClasses(enclosingPackage + "." + classNamePrefix, 10)
      val srcs = classes.flatMap(_.sourceFileObject).map(toAbstractFile).toSet
      sender ! AbstractFiles(srcs)

    case TypeCompletionsReq(query: String, maxResults: Int) =>
      sender ! SymbolSearchResults(oldSearchTypes(query, maxResults))

    case RPCRequestEvent(req: Any, callId: Int) =>
      try {
        req match {
          case ImportSuggestionsReq(file, point, names, maxResults) =>
            val suggestions = names.map(oldSearchTypes(_, maxResults))
            project ! RPCResultEvent(toWF(ImportSuggestions(suggestions)), callId)

          case PublicSymbolSearchReq(keywords, maxResults) =>
            val suggestions = keywords.flatMap(oldSearchSymbols(_, maxResults))
            project ! RPCResultEvent(toWF(SymbolSearchResults(suggestions)), callId)
        }
      } catch {
        case e: Exception =>
          log.error(e, "Error handling RPC: " + req)
          project.sendRPCError(
            ErrExceptionInIndexer,
            "Error occurred in indexer. Check the server log.",
            callId
          )
      }

    case other =>
      log.warning("Indexer: WTF, what's " + other)
  }
}
