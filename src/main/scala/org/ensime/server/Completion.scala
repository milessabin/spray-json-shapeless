package org.ensime.server

import akka.pattern.Patterns
import akka.util.Timeout
import org.ensime.model.CompletionInfoList
import scala.collection.mutable
import scala.concurrent.Await
import scala.reflect.internal.util.{ SourceFile, BatchSourceFile }
import org.ensime.util.Arrays
import org.ensime.model.{ CompletionInfo, CompletionSignature, SymbolSearchResults }
import scala.concurrent.duration._

trait CompletionControl {
  self: RichPresentationCompiler =>

  def makeCompletions(
    prefix: String,
    sym: Symbol,
    tpe: Type,
    constructing: Boolean,
    inherited: Boolean,
    viaView: Symbol): List[CompletionInfo] = {

    var score = 0
    if (sym.nameString.startsWith(prefix)) score += 10
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

    if (constructing) {
      val constructorSyns = constructorSynonyms(sym).map {
        c => CompletionInfo.fromSymbolAndType(sym, c.tpe, score)
      }
      infos ++ constructorSyns
    } else {
      val applySyns = applySynonyms(sym).map {
        c => CompletionInfo.fromSymbolAndType(sym, c.tpe, score)
      }
      infos ++ applySyns
    }
  }

  def completionsAt(p: Position, maxResultsArg: Int, caseSens: Boolean): CompletionInfoList = {

    val maxResults = if (maxResultsArg == 0) Int.MaxValue else maxResultsArg

    def makeTypeSearchCompletions(prefix: String): List[CompletionInfo] = {
      val req = TypeCompletionsReq(prefix, maxResults)

      import scala.concurrent.ExecutionContext.Implicits.{ global => exe }

      val askRes = Patterns.ask(indexer, req, Timeout(1000.milliseconds))
      val optFut = askRes.map(Some(_)).recover { case _ => None }
      val result = Await.result(optFut, Duration.Inf)

      result match {
        case Some(s: SymbolSearchResults) =>
          s.syms.map { s =>
            CompletionInfo(s.localName, CompletionSignature(List.empty, s.name),
              -1, isCallable = false, 40, Some(s.name))
          }.toList
        case None =>
          logger.warn("request timed out")
          List.empty
        case unknown =>
          throw new IllegalStateException("Unexpected response type from request:" + unknown)
      }
    }

    def makeAll(
      x: Response[List[Member]],
      prefix: String,
      constructing: Boolean): List[CompletionInfo] = {
      val buff = new mutable.LinkedHashSet[CompletionInfo]()
      var members = List[Member]()
      do {
        x.get match {
          case Left(mems) => members ++= mems
          case _ =>
        }
      } while (!x.isComplete)

      logger.info("Found " + members.size + " members.")

      askOption[Unit] {
        val filtered = filterMembersByPrefix(members, prefix, matchEntire = false, caseSens = caseSens)
        logger.info("Filtered down to " + filtered.size + ".")
        for (m <- filtered) {
          m match {
            case m @ ScopeMember(sym, tpe, accessible, viaView) =>
              if (!sym.isConstructor) {
                buff ++= makeCompletions(prefix, sym, tpe, constructing, inherited = false, NoSymbol)
              }
            case m @ TypeMember(sym, tpe, accessible, inherited, viaView) =>
              if (!sym.isConstructor) {
                buff ++= makeCompletions(prefix, sym, tpe, constructing, inherited, viaView)
              }
            case _ =>
          }
        }
      }
      buff.toList
    }

    val (prefix, results) = completionContext(p) match {
      case Some(PackageContext(path, prefix)) =>
        askReloadFile(p.source)
        val typeSearchSyms = if (path.isEmpty) {
          makeTypeSearchCompletions(prefix)
        } else List.empty
        (prefix,
          askCompletePackageMember(path, prefix) ++
          typeSearchSyms)
      case Some(SymbolContext(p, prefix, constructing)) =>
        askReloadFile(p.source)
        val x = new Response[List[Member]]
        askScopeCompletion(p, x)
        (prefix, makeAll(x, prefix, constructing))
      case Some(MemberContext(p, prefix, constructing)) =>
        askReloadFile(p.source)
        val x = new Response[List[Member]]
        askTypeCompletion(p, x)
        (prefix, makeAll(x, prefix, constructing))
      case _ =>
        logger.error("Unrecognized completion context.")
        ("", List.empty)
    }
    CompletionInfoList(prefix, results.sortWith({ (c1, c2) =>
      c1.relevance > c2.relevance ||
        (c1.relevance == c2.relevance &&
          c1.name.length < c2.name.length)
    }).take(maxResults))
  }

  private val ident = "[a-zA-Z0-9_]"
  private val nonIdent = "[^a-zA-Z0-9_]"
  private val ws = "[ \n\r\t]"

  trait CompletionContext

  private val packRE = ("^.*?(?:package|import)[ ]+((?:[a-z0-9]+\\.)*)(?:(" + ident + "*)|\\{.*?(" + ident + "*))$").r
  case class PackageContext(path: String, prefix: String) extends CompletionContext
  def packageContext(preceding: String): Option[PackageContext] = {
    if (packRE.findFirstMatchIn(preceding).isDefined) {
      val m = packRE.findFirstMatchIn(preceding).get
      if (m.group(2) != null) {
        logger.info("Matched package context: " + m.group(1) + "," + m.group(2))
        Some(PackageContext(m.group(1), m.group(2)))
      } else {
        logger.info("Matched package context: " + m.group(1) + "," + m.group(3))
        Some(PackageContext(m.group(1), m.group(3)))
      }
    } else None
  }

  private val nameFollowingWhiteSpaceRE =
    List("^", ws, "*", "(", ident, "*)$").mkString.r

  private val nameFollowingReservedRE =
    List("(?:", nonIdent, "|", ws, ")",
      "(?:else|case|new|with|extends|yield|return|throw|do)",
      ws, "+",
      "(", ident, "*)$").mkString.r

  private val nameFollowingSyntaxRE =
    List("([!:=><\\(,;\\}\\[\\{\n+*/\\^&~%\\-])",
      ws, "*", "(", ident, "*)$").mkString.r

  private val constructorNameRE =
    List("(?:", nonIdent, "|", ws, ")", "(?:new)", ws, "+",
      "(", ident, "*)$").mkString.r

  private val nameFollowingControl =
    List("(?:for|while|if)", ws, "*", "\\((.*?)\\)", ws, "*",
      "(", ident, "*)$").mkString.r

  // Return true if the given string is balanced with respect
  // to parenthesis. Ignores the possibility of comments and strings.
  private def parenBalanced(s: String): Boolean = {
    var i = 0
    var p = 0
    while (i < s.length) {
      val ch = s.charAt(i)
      if (ch == '(') p += 1
      else if (ch == ')') p -= 1
      i += 1
    }
    p == 0
  }

  case class SymbolContext(p: Position, prefix: String,
    constructing: Boolean) extends CompletionContext
  def symContext(p: Position, preceding: String): Option[SymbolContext] = {
    var mo = nameFollowingWhiteSpaceRE.findFirstMatchIn(preceding)
    if (mo.isDefined) {
      val m = mo.get
      logger.info("Matched sym after ws context.")
      return Some(SymbolContext(p, m.group(1), constructing = false))
    }
    mo = nameFollowingSyntaxRE.findFirstMatchIn(preceding)
    if (mo.isDefined) {
      val m = mo.get
      logger.info("Matched sym following syntax context.")
      return Some(SymbolContext(p, m.group(2), constructing = false))
    }
    mo = constructorNameRE.findFirstMatchIn(preceding)
    if (mo.isDefined) {
      val m = mo.get
      logger.info("Matched constructing context.")
      return Some(SymbolContext(p, m.group(1), constructing = true))
    }
    mo = nameFollowingReservedRE.findFirstMatchIn(preceding)
    if (mo.isDefined) {
      val m = mo.get
      logger.info("Matched sym following reserved context.")
      return Some(SymbolContext(p, m.group(1), constructing = false))
    }
    mo = nameFollowingControl.findFirstMatchIn(preceding)
    if (mo.isDefined) {
      val m = mo.get

      // Check that we're actually inside the control structure's
      // parens.
      if (parenBalanced(m.group(1))) {

        logger.info("Matched sym following control structure context.")
        return Some(SymbolContext(p, m.group(2), constructing = false))
      }
    }
    None
  }

  private def spliceSource(s: SourceFile, start: Int, end: Int,
    replacement: String): SourceFile = {
    new BatchSourceFile(s.file,
      Arrays.splice(s.content, start, end, replacement.toArray))
  }

  private val memberRE = "([\\. ]+)([^\\. ]*)$".r
  private val memberConstructorRE = ("new ((?:[a-z0-9]+\\.)*)(" + ident + "*)$").r
  case class MemberContext(p: Position, prefix: String, constructing: Boolean) extends CompletionContext

  def memberContext(p: Position, preceding: String): Option[MemberContext] = {
    memberRE.findFirstMatchIn(preceding) match {
      case Some(m) =>
        val constructing = memberConstructorRE.findFirstMatchIn(preceding).isDefined
        logger.info("Matched member context. Constructing? " + constructing)
        val dot = m.group(1)
        val prefix = m.group(2)

        // Replace prefix with ' ()' so parser doesn't
        // pick up next line as a continuation of current.
        val src = spliceSource(p.source,
          p.point - prefix.length,
          p.point - 1,
          " ()")

        // Move point back to target of method selection.
        val newP = p.withSource(src).withShift(-(prefix.length + dot.length))

        Some(MemberContext(newP, prefix, constructing))
      case None => None
    }
  }

  protected def completionContext(p: Position): Option[CompletionContext] = {
    val src = p.source
    val lineNum = src.offsetToLine(p.point - 1)
    val bol = src.lineToOffset(lineNum)
    val line = src.lineToString(lineNum)
    val preceding = line.take(p.point - bol)
    logger.info("Line: " + line)
    logger.info("Preceding: " + preceding)
    packageContext(preceding).
      orElse(symContext(p, preceding)).
      orElse(memberContext(p, preceding))
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
        }.toList
      case _ => List.empty
    }
  }

}
