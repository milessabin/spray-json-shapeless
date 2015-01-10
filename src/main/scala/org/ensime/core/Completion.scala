package org.ensime.core

import akka.pattern.Patterns
import akka.util.Timeout
import org.ensime.model.CompletionInfoList
import scala.collection.mutable
import scala.concurrent.{ Future, Await }
import scala.reflect.internal.util.{ SourceFile, BatchSourceFile }
import scala.reflect.runtime.universe.{ showRaw }
import org.ensime.util.Arrays
import org.ensime.model.{ CompletionInfo, CompletionSignature, SymbolSearchResults }
import scala.concurrent.duration._

trait CompletionControl {
  self: RichPresentationCompiler =>

  sealed trait CompletionContext {
    val source: SourceFile
    val offset: Int
    val prefix: String
    val constructing: Boolean
  }

  case class ScopeContext(
    source: SourceFile,
    offset: Int,
    prefix: String,
    constructing: Boolean) extends CompletionContext

  case class MemberContext(
    source: SourceFile,
    offset: Int,
    prefix: String,
    constructing: Boolean) extends CompletionContext

  private val IdentRegexp = """([a-zA-Z0-9_#:<=>@!%&*+/?\\^|~-]*)$""".r
  private val ImportTopLevelRegexp = """import [^\.]*$""".r
  private val ConstructingRegexp = """new [\.a-zA-Z0-9_]*$""".r
  private val TypeNameRegex = """^[A-Z][a-zA-Z0-9]*$""".r

  def completionsAt(inputP: Position, maxResultsArg: Int, caseSens: Boolean): CompletionInfoList = {
    val maxResults = if (maxResultsArg == 0) Int.MaxValue else maxResultsArg

    val preceding = inputP.source.content.slice(
      Math.max(0, inputP.point - 100), inputP.point)

    val defaultPrefix = IdentRegexp.findFirstMatchIn(preceding) match {
      case Some(m) => m.group(1)
      case _ => ""
    }

    val constructing = ConstructingRegexp.findFirstMatchIn(preceding).isDefined
    logger.debug("In constructing context: " + constructing)

    val (src, p, patched) = if (defaultPrefix.isEmpty) {
      // Add a fake prefix if none was provided by the user. Otherwise the
      // compiler will give us a weird tree.
      val src = spliceSource(inputP.source, inputP.end, inputP.end, "a")
      (src, inputP.withSource(src).withShift(1), true)
    } else {
      (inputP.source, inputP, false)
    }
    askReloadFile(src)
    val x = new Response[Tree]
    askTypeAt(p, x)

    val context = x.get match {
      case Left(tree) => {
        logger.debug("Completing at tree:" + tree.summaryString)
        tree match {
          case Apply(fun, _) => {
            fun match {
              case Select(qual: New, name) => {
                Some(ScopeContext(src, qual.pos.end, defaultPrefix, true))
              }
              case Select(qual, name) if qual.pos.isDefined && qual.pos.isRange => {
                val prefix = if (patched) "" else name.decoded
                Some(MemberContext(src, qual.pos.end, prefix, constructing))
              }
              case _ => {
                val prefix = if (patched) "" else src.content.slice(fun.pos.start, fun.pos.end).mkString
                Some(ScopeContext(src, fun.pos.end, prefix, constructing))
              }
            }
          }
          case Literal(Constant(_)) => None
          case New(name) => {
            Some(ScopeContext(src, name.pos.end, defaultPrefix, true))
          }
          case Select(qual, name) if qual.pos.isDefined && qual.pos.isRange => {
            Some(MemberContext(src, qual.pos.end, defaultPrefix, constructing))
          }
          case Import(expr, _) => {
            val topLevel = ImportTopLevelRegexp.findFirstMatchIn(preceding).isDefined
            if (topLevel) {
              Some(ScopeContext(src, expr.pos.end, defaultPrefix, false))
            } else {
              Some(MemberContext(src, expr.pos.end, defaultPrefix, false))
            }
          }
          case x => {
            Some(ScopeContext(src, p.point, defaultPrefix, constructing))
          }
        }
      }
      case _ => {
        logger.error("Unrecognized completion context.")
        None
      }
    }
    context match {
      case Some(context) => {
        CompletionInfoList(
          context.prefix,
          makeAll(context, maxResults, caseSens).sortWith({ (c1, c2) =>
            c1.relevance > c2.relevance ||
              (c1.relevance == c2.relevance &&
                c1.name.length < c2.name.length)
          }).take(maxResults))
      }
      case _ => CompletionInfoList("", Nil)
    }
  }

  private def spliceSource(s: SourceFile, start: Int, end: Int,
    replacement: String): SourceFile = {
    new BatchSourceFile(s.file,
      Arrays.splice(s.content, start, end, replacement.toArray))
  }

  def fetchTypeSearchCompletions(prefix: String, maxResults: Int): Future[Option[List[CompletionInfo]]] = {
    val req = TypeCompletionsReq(prefix, maxResults)
    import scala.concurrent.ExecutionContext.Implicits.{ global => exe }
    val askRes = Patterns.ask(indexer, req, Timeout(1000.milliseconds))
    askRes.map { result =>
      result match {
        case s: SymbolSearchResults =>
          s.syms.map { s =>
            CompletionInfo(
              s.localName, CompletionSignature(List.empty, s.name),
              -1, isCallable = false, 40, Some(s.name))
          }.toList
        case unknown => {
          throw new IllegalStateException("Unexpected response type from request:" + unknown)
          List.empty
        }
      }
    }.map(Some(_)).recover { case _ => None }
  }

  def makeAll(context: CompletionContext, maxResults: Int, caseSens: Boolean): List[CompletionInfo] = {

    def toCompletionInfo(
      context: CompletionContext,
      sym: Symbol,
      tpe: Type,
      inherited: Boolean,
      viaView: Symbol): List[CompletionInfo] = {

      var score = 0
      if (sym.nameString.startsWith(context.prefix)) score += 10
      if (!inherited) score += 10
      if (!sym.hasPackageFlag) score += 10
      if (!sym.isType) score += 10
      if (sym.isLocalToBlock) score += 10
      if (sym.isPublic) score += 10
      if (viaView == NoSymbol) score += 10
      if (sym.owner != definitions.AnyClass &&
        sym.owner != definitions.AnyRefClass &&
        sym.owner != definitions.ObjectClass) score += 30

      val infos = List(CompletionInfo.fromSymbolAndType(sym, tpe, score))

      if (context.constructing) {
        val constructorSyns = constructorSynonyms(sym).map {
          c => CompletionInfo.fromSymbolAndType(sym, c.tpe, score + 50)
        }
        infos ++ constructorSyns
      } else {
        val applySyns = applySynonyms(sym).map {
          c => CompletionInfo.fromSymbolAndType(sym, c.tpe, score)
        }
        infos ++ applySyns
      }
    }

    val buff = new mutable.LinkedHashSet[CompletionInfo]()

    // Kick off an index search if the name looks like a type.
    // Do this before the lookups below, so the two can
    // proceed concurrently.
    val typeSearch = context match {
      case ScopeContext(_, _, prefix, _) => {
        if (TypeNameRegex.findFirstMatchIn(prefix).isDefined) {
          Some(fetchTypeSearchCompletions(prefix, maxResults))
        } else None
      }
      case _ => None
    }

    var members = List[Member]()
    val x = new Response[List[Member]]
    context match {
      case ScopeContext(src, offset, _, _) =>
        askScopeCompletion(rangePos(src, offset, offset, offset), x)
      case MemberContext(src, offset, _, _) =>
        askTypeCompletion(rangePos(src, offset, offset, offset), x)
    }
    do {
      x.get match {
        case Left(mems) => members ++= mems
        case _ =>
      }
    } while (!x.isComplete)

    logger.info("Found " + members.size + " members.")

    // Any interaction with the members (their types and symbols) must be done
    // on the compiler thread.
    askOption[Unit] {
      val filtered = filterMembersByPrefix(members, context.prefix, matchEntire = false, caseSens = caseSens)
      logger.info("Filtered down to " + filtered.size + ".")
      for (m <- filtered) {
        m match {
          case m @ ScopeMember(sym, tpe, accessible, viaView) =>
            if (!sym.isConstructor) {
              buff ++= toCompletionInfo(context, sym, tpe, inherited = false, NoSymbol)
            }
          case m @ TypeMember(sym, tpe, accessible, inherited, viaView) =>
            if (!sym.isConstructor) {
              buff ++= toCompletionInfo(context, sym, tpe, inherited, viaView)
            }
          case _ =>
        }
      }
    }

    val typeSearchResults = typeSearch.flatMap(Await.result(_, Duration.Inf))
    buff.toList ++ typeSearchResults.getOrElse(Nil)
  }

}

trait Completion { self: RichPresentationCompiler =>

  def completePackageMember(path: String, prefix: String): List[CompletionInfo] = {
    packageSymFromPath(path) match {
      case Some(sym) =>
        val memberSyms = packageMembers(sym).filterNot { s =>
          s == NoSymbol || s.nameString.contains("$")
        }
        memberSyms.flatMap { s =>
          val name = if (s.hasPackageFlag) { s.nameString } else { typeShortName(s) }
          if (name.startsWith(prefix)) {
            Some(CompletionInfo(name, CompletionSignature(List.empty, ""), -1, isCallable = false, 50, None))
          } else None
        }.toList.sortBy(ci => (ci.relevance, ci.name))
      case _ => List.empty
    }
  }

}
