package org.ensime.core

import akka.actor.{ Actor, ActorLogging }
import akka.event.LoggingReceive

import org.ensime.api._

import org.ensime.indexer.DatabaseService.FqnSymbol
import org.ensime.indexer.{ EnsimeVFS, SearchService }
import org.ensime.model._
import org.ensime.server.protocol._
import org.ensime.server.protocol.ProtocolConst._

//@deprecated("there is no good reason for this to be an actor, plus it enforces single-threaded badness", "fommil")
class Indexer(
    config: EnsimeConfig,
    index: SearchService,
    val vfs: EnsimeVFS
) extends Actor with ActorLogging {

  private def typeResult(hit: FqnSymbol) = TypeSearchResult(
    hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
    LineSourcePositionHelper.fromFqnSymbol(hit)(config, vfs)
  )

  def oldSearchTypes(query: String, max: Int) =
    index.searchClasses(query, max).filterNot {
      name => name.fqn.endsWith("$") || name.fqn.endsWith("$class")
    }.map(typeResult)

  def oldSearchSymbols(terms: List[String], max: Int) =
    index.searchClassesMethods(terms, max).flatMap {
      case hit if hit.declAs == DeclaredAs.Class => Some(typeResult(hit))
      case hit if hit.declAs == DeclaredAs.Method => Some(MethodSearchResult(
        hit.fqn, hit.fqn.split("\\.").last, hit.declAs,
        LineSourcePositionHelper.fromFqnSymbol(hit)(config, vfs),
        hit.fqn.split("\\.").init.mkString(".")
      ))
      case _ => None // were never supported
    }

  override def receive = LoggingReceive {
    case req: RpcRequest =>
      try {
        req match {
          case ImportSuggestionsReq(file, point, names, maxResults) =>
            val suggestions = names.map(oldSearchTypes(_, maxResults))
            sender ! ImportSuggestions(suggestions)

          case PublicSymbolSearchReq(keywords, maxResults) =>
            val suggestions = oldSearchSymbols(keywords, maxResults)
            sender ! SymbolSearchResults(suggestions)

          case unexpected =>
            // TODO compiler blew up... too many missing cases
            require(false, unexpected.toString)
        }
      } catch {
        case e: Exception =>
          log.error(e, "Error handling RPC: " + req)
          sender ! RPCError(ErrExceptionInIndexer, "Error occurred in indexer. Check the server log.")
      }
    case TypeCompletionsReq(query: String, maxResults: Int) =>
      try {
        sender ! SymbolSearchResults(oldSearchTypes(query, maxResults))
      } catch {
        case e: Exception =>
          log.error(e, "Error handling internal call: " + query)
          sender ! RPCError(ErrExceptionInIndexer, "Error occurred in indexer. Check the server log.")
      }

    case other =>
      log.warning("Indexer: WTF, what's " + other)
  }
}
