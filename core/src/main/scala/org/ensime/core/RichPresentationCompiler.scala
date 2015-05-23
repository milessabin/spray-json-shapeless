package org.ensime.core

import java.io.File
import java.nio.charset.Charset

import org.ensime.api._

import akka.actor.ActorRef
import org.ensime.config._
import org.ensime.indexer.{ EnsimeVFS, SearchService }
import org.ensime.model._
import org.ensime.util.FileUtils
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.reflect.internal.util.{ BatchSourceFile, RangePosition, SourceFile }
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.{ CompilerControl, Global }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.util._
import scala.tools.refactoring.analysis.GlobalIndexes

trait RichCompilerControl extends CompilerControl with RefactoringControl with CompletionControl with DocFinding {
  self: RichPresentationCompiler =>

  def charset: Charset = Charset.forName(settings.encoding.value)

  def askOption[A](op: => A): Option[A] =
    try {
      Some(ask(() => op))
    } catch {
      case fi: FailedInterrupt =>
        fi.getCause match {
          case e: InterruptedException =>
            Thread.currentThread().interrupt()
            logger.error("interrupted exception in askOption", e)
            None
          case e =>
            logger.error("Error during askOption", e)
            None
        }
      case e: Throwable =>
        logger.error("Error during askOption", e)
        None
    }

  def askDocSignatureAtPoint(p: Position): Option[DocSigPair] =
    askOption {
      symbolAt(p).orElse(typeAt(p).map(_.typeSymbol)).flatMap(docSignature(_, Some(p)))
    }.flatten

  def askDocSignatureForSymbol(typeFullName: String, memberName: Option[String],
    signatureString: Option[String]): Option[DocSigPair] =
    askOption {
      symbolMemberByName(
        typeFullName, memberName, signatureString
      ).flatMap(docSignature(_, None))
    }.flatten

  def askSymbolInfoAt(p: Position): Option[SymbolInfo] =
    askOption(symbolAt(p).map(SymbolInfo(_))).flatten

  def askSymbolByName(fqn: String, memberName: Option[String], signatureString: Option[String]): Option[SymbolInfo] =
    askOption(symbolMemberByName(fqn, memberName, signatureString).map(SymbolInfo(_))).flatten

  def askTypeInfoAt(p: Position): Option[TypeInfo] =
    askOption(typeAt(p).map(TypeInfo(_, PosNeededYes))).flatten

  def askTypeInfoById(id: Int): Option[TypeInfo] =
    askOption(typeById(id).map(TypeInfo(_, PosNeededYes))).flatten

  def askTypeInfoByName(name: String): Option[TypeInfo] =
    askOption(typeByName(name).map(TypeInfo(_, PosNeededYes))).flatten

  def askTypeInfoByNameAt(name: String, p: Position): Option[TypeInfo] = {
    val nameSegs = name.split("\\.")
    val firstName: String = nameSegs.head
    val x = new Response[List[Member]]()
    askScopeCompletion(p, x)
    (for (
      members <- x.get.left.toOption;
      infos <- askOption {
        val roots = filterMembersByPrefix(
          members, firstName, matchEntire = true, caseSens = true
        ).map { _.sym }
        val restOfPath = nameSegs.drop(1).mkString(".")
        val syms = roots.flatMap { symbolByName(restOfPath, _) }
        syms.find(_.tpe != NoType).map { sym => TypeInfo(sym.tpe) }
      }
    ) yield infos).flatten
  }

  def askCallCompletionInfoById(id: Int): Option[CallCompletionInfo] =
    askOption(typeById(id).map(CallCompletionInfo(_))).flatten

  def askPackageByPath(path: String): Option[PackageInfo] =
    askOption(PackageInfo.fromPath(path))

  def askReloadFile(f: SourceFile): Unit = {
    askReloadFiles(List(f))
  }

  def askReloadFiles(files: Iterable[SourceFile]): Either[Unit, Throwable] = {
    val x = new Response[Unit]()
    askReload(files.toList, x)
    x.get
  }

  def askLoadedTyped(f: SourceFile): Either[Tree, Throwable] = {
    val x = new Response[Tree]()
    askLoadedTyped(f, x)
    x.get
  }

  def askUnloadAllFiles(): Unit = askOption(unloadAllFiles())

  def askRemoveAllDeleted(): Option[Unit] = askOption(removeAllDeleted())

  def askRemoveDeleted(f: File) = askOption(removeDeleted(AbstractFile.getFile(f)))

  def askReloadAllFiles() = {
    val all = {
      for {
        file <- config.sourceFiles
        source = getSourceFile(file.getAbsolutePath)
      } yield source
    }.toSet ++ activeUnits().map(_.source)
    askReloadFiles(all)
  }

  def loadedFiles: List[SourceFile] = activeUnits().map(_.source)

  def askReloadExistingFiles() =
    askReloadFiles(loadedFiles)

  def askInspectTypeById(id: Int): Option[TypeInspectInfo] =
    askOption(typeById(id).map(inspectType)).flatten

  def askInspectTypeAt(p: Position): Option[TypeInspectInfo] =
    askOption(inspectTypeAt(p)).flatten

  def askInspectTypeByName(name: String): Option[TypeInspectInfo] =
    askOption(typeByName(name).map(inspectType)).flatten

  def askCompletePackageMember(path: String, prefix: String): List[CompletionInfo] =
    askOption(completePackageMember(path, prefix)).getOrElse(List.empty)

  def askCompletionsAt(p: Position, maxResults: Int, caseSens: Boolean): CompletionInfoList =
    completionsAt(p, maxResults, caseSens)

  def askReloadAndTypeFiles(files: Iterable[SourceFile]) =
    askOption(reloadAndTypeFiles(files))

  def askUsesOfSymAtPoint(p: Position): List[RangePosition] =
    askOption(usesOfSymbolAtPoint(p).toList).getOrElse(List.empty)

  // force the full path of Set because nsc appears to have a conflicting Set....
  def askSymbolDesignationsInRegion(p: RangePosition, tpes: List[SourceSymbol]): SymbolDesignations =
    askOption(
      new SemanticHighlighting(this).symbolDesignationsInRegion(p, tpes)
    ).getOrElse(SymbolDesignations(new File("."), List.empty))

  def askImplicitInfoInRegion(p: Position): List[ImplicitInfo] =
    askOption(
      new ImplicitAnalyzer(this).implicitDetails(p)
    ).getOrElse(List.empty)

  def askClearTypeCache(): Unit = clearTypeCache()

  def askNotifyWhenReady(): Unit = ask(setNotifyWhenReady)

  def createSourceFile(path: String) = getSourceFile(path)
  def createSourceFile(file: AbstractFile) = getSourceFile(file)
  def createSourceFile(file: SourceFileInfo) = file match {
    case SourceFileInfo(f, None, None) => getSourceFile(f.getCanonicalPath)
    case SourceFileInfo(f, Some(contents), None) => new BatchSourceFile(AbstractFile.getFile(f.getCanonicalPath), contents)
    case SourceFileInfo(f, None, Some(contentsIn)) =>
      val contents = FileUtils.readFile(contentsIn, charset) match {
        case Right(contentStr) => contentStr
        case Left(e) => throw e
      }
      new BatchSourceFile(AbstractFile.getFile(f.getCanonicalPath), contents)
  }
  def findSourceFile(path: String): Option[SourceFile] = allSources.find(
    _.file.path == path
  )

  // TODO: friends should not give friends other people's types (Position)
  def askLinkPos(sym: Symbol, path: AbstractFile): Option[Position] =
    askOption(linkPos(sym, createSourceFile(path)))
}

class RichPresentationCompiler(
  val config: EnsimeConfig,
  settings: Settings,
  val richReporter: Reporter,
  val parent: ActorRef,
  val indexer: ActorRef,
  val search: SearchService
)(
  implicit
  val vfs: EnsimeVFS
) extends Global(settings, richReporter)
    with ModelBuilders with RichCompilerControl
    with RefactoringImpl with Completion with Helpers {

  val logger = LoggerFactory.getLogger(this.getClass)

  private val symsByFile = new mutable.HashMap[AbstractFile, mutable.LinkedHashSet[Symbol]] {
    override def default(k: AbstractFile) = {
      val v = new mutable.LinkedHashSet[Symbol]
      put(k, v)
      v
    }
  }

  def activeUnits(): List[CompilationUnit] = {
    val invalidSet = toBeRemoved.synchronized { toBeRemoved.toSet }
    unitOfFile.filter { kv => !invalidSet.contains(kv._1) }.values.toList
  }

  /** Called from typechecker every time a top-level class or object is entered.*/
  override def registerTopLevelSym(sym: Symbol): Unit = {
    super.registerTopLevelSym(sym)
    symsByFile(sym.sourceFile) += sym
  }

  def unloadAllFiles(): Unit = {
    allSources.foreach(removeUnitOf)
  }

  /**
   * Remove symbols defined by files that no longer exist.
   * Note that these symbols will not be collected by
   * syncTopLevelSyms, since the units in question will
   * never be reloaded again.
   */
  def removeAllDeleted(): Unit = {
    allSources = allSources.filter { _.file.exists }
    val deleted = symsByFile.keys.filter { !_.exists }
    for (f <- deleted) {
      removeDeleted(f)
    }
  }

  /** Remove symbols defined by file that no longer exist. */
  def removeDeleted(f: AbstractFile): Unit = {
    val syms = symsByFile(f)
    for (s <- syms) {
      s.owner.info.decls unlink s
    }
    symsByFile.remove(f)
    unitOfFile.remove(f)
  }

  private def typePublicMembers(tpe: Type): Iterable[TypeMember] = {
    val members = new mutable.LinkedHashMap[Symbol, TypeMember]
    def addTypeMember(sym: Symbol, pre: Type, inherited: Boolean, viaView: Symbol): Unit = {
      try {
        val m = new TypeMember(
          sym,
          sym.tpe,
          sym.isPublic,
          inherited,
          viaView
        )
        members(sym) = m
      } catch {
        case e: Throwable =>
          logger.error("Error: Omitting member " + sym + ": " + e)
      }
    }
    for (sym <- tpe.decls) {
      addTypeMember(sym, tpe, inherited = false, NoSymbol)
    }
    for (sym <- tpe.members) {
      addTypeMember(sym, tpe, inherited = true, NoSymbol)
    }
    members.values
  }

  protected def getMembersForTypeAt(tpe: Type, p: Position): Iterable[Member] = {
    if (isNoParamArrowType(tpe)) {
      typePublicMembers(typeOrArrowTypeResult(tpe))
    } else {
      val members: Iterable[Member] = try {
        wrapTypeMembers(p)
      } catch {
        case e: Throwable =>
          logger.error("Error retrieving type members:", e)
          List.empty
      }
      // Remove duplicates
      // Filter out synthetic things
      val bySym = new mutable.LinkedHashMap[Symbol, Member]
      for (m <- members ++ typePublicMembers(tpe)) {
        if (!m.sym.nameString.contains("$")) {
          bySym(m.sym) = m
        }
      }
      bySym.values
    }
  }

  protected def inspectType(tpe: Type): TypeInspectInfo = {
    val parents = tpe.parents
    new TypeInspectInfo(
      TypeInfo(tpe, PosNeededAvail),
      companionTypeOf(tpe).map(cacheType),
      prepareSortedInterfaceInfo(typePublicMembers(tpe.asInstanceOf[Type]), parents)
    )
  }

  protected def inspectTypeAt(p: Position): Option[TypeInspectInfo] = {
    typeAt(p).map(tpe => {
      val members = getMembersForTypeAt(tpe, p)
      val parents = tpe.parents
      val preparedMembers = prepareSortedInterfaceInfo(members, parents)
      new TypeInspectInfo(
        TypeInfo(tpe, PosNeededAvail),
        companionTypeOf(tpe).map(cacheType),
        preparedMembers
      )
    }).orElse {
      logger.error("ERROR: Failed to get any type information :(  ")
      None
    }
  }

  private def typeOfTree(t: Tree): Option[Type] = {
    val tree = t match {
      case Select(qualifier, name) if t.tpe == ErrorType =>
        qualifier
      case t: ImplDef if t.impl != null =>
        t.impl
      case t: ValOrDefDef if t.tpt != null =>
        t.tpt
      case t: ValOrDefDef if t.rhs != null =>
        t.rhs
      case otherTree =>
        otherTree
    }

    Option(tree.tpe)
  }

  protected def typeAt(p: Position): Option[Type] = {
    val tree = wrapTypedTreeAt(p)
    typeOfTree(tree)
  }

  protected def typeByName(name: String): Option[Type] =
    symbolByName(name).flatMap {
      case NoSymbol => None
      case sym: Symbol => sym.tpe match {
        case NoType => None
        case tpe: Type => Some(tpe)
      }
    }

  protected def symbolMemberByName(
    fqn: String, memberName: Option[String], signatureString: Option[String]
  ): Option[Symbol] = {
    symbolByName(fqn).flatMap { owner =>
      memberName.flatMap { rawName =>
        val module = rawName.endsWith("$")
        val nm = if (module) rawName.dropRight(1) else rawName
        val candidates = owner.info.members.filter { s =>
          s.nameString == nm && ((module && s.isModule) || (!module && (!s.isModule || s.hasPackageFlag)))
        }
        val exact = signatureString.flatMap { s => candidates.find(_.signatureString == s) }
        exact.orElse(candidates.headOption)
      }.orElse(Some(owner))
    }
  }

  protected def filterMembersByPrefix(members: List[Member], prefix: String,
    matchEntire: Boolean, caseSens: Boolean): List[Member] = members.filter { m =>
    val prefixUpper = prefix.toUpperCase
    val sym = m.sym
    val ns = sym.nameString
    (((matchEntire && ns == prefix) ||
      (!matchEntire && caseSens && ns.startsWith(prefix)) ||
      (!matchEntire && !caseSens && ns.toUpperCase.startsWith(prefixUpper)))
      && !sym.nameString.contains("$"))
  }

  protected def symbolAt(pos: Position): Option[Symbol] = {
    val tree = wrapTypedTreeAt(pos)
    // This code taken mostly verbatim from Scala IDE sources. Licensed
    // under SCALA LICENSE.
    val wannabes =
      tree match {
        case Import(expr, selectors) =>
          if (expr.pos.includes(pos)) {
            @annotation.tailrec
            def locate(p: Position, inExpr: Tree): Symbol = inExpr match {
              case Select(qualifier, name) =>
                if (qualifier.pos.includes(p)) locate(p, qualifier)
                else inExpr.symbol
              case tree => tree.symbol
            }
            List(locate(pos, expr))
          } else {
            selectors.filter(_.namePos < pos.point).sortBy(_.namePos).lastOption map { sel =>
              val tpe = stabilizedType(expr)
              List(tpe.member(sel.name), tpe.member(sel.name.toTypeName))
            } getOrElse Nil
          }
        case Annotated(atp, _) =>
          List(atp.symbol)
        case st: SymTree =>
          logger.debug("using symbol of " + tree.getClass + " tree")
          List(tree.symbol)
        case _ =>
          logger.warn("symbolAt for " + tree.getClass + ": " + tree)
          Nil
      }
    wannabes.find(_.exists)
  }

  protected def specificOwnerOfSymbolAt(pos: Position): Option[Symbol] = {
    val tree = wrapTypedTreeAt(pos)
    tree match {
      case tree @ Select(qualifier, name) =>
        qualifier match {
          case t: ApplyImplicitView => t.args.headOption.map(_.tpe.typeSymbol)
          case _ => Some(qualifier.tpe.typeSymbol)
        }
      case _ => None
    }
  }

  protected def linkPos(sym: Symbol, source: SourceFile): Position = {
    wrapLinkPos(sym, source)
  }

  protected def usesOfSymbolAtPoint(point: Position): Iterable[RangePosition] = {
    symbolAt(point) match {
      case Some(s) =>
        class CompilerGlobalIndexes extends GlobalIndexes {
          val global = RichPresentationCompiler.this
          val sym = s.asInstanceOf[global.Symbol]
          val cuIndexes = this.global.unitOfFile.values.map { u =>
            CompilationUnitIndex(u.body)
          }
          val index = GlobalIndex(cuIndexes.toList)
          val result = index.occurences(sym).map {
            _.pos match {
              case p: RangePosition => p
              case p =>
                new RangePosition(
                  p.source, p.point, p.point, p.point
                )
            }
          }
        }
        val gi = new CompilerGlobalIndexes
        gi.result
      case None => List.empty
    }
  }

  private var notifyWhenReady = false

  override def isOutOfDate: Boolean = {
    if (notifyWhenReady && !super.isOutOfDate) {
      parent ! FullTypeCheckCompleteEvent
      notifyWhenReady = false
    }
    super.isOutOfDate
  }

  protected def setNotifyWhenReady(): Unit = {
    notifyWhenReady = true
  }

  protected def reloadAndTypeFiles(sources: Iterable[SourceFile]) = {
    wrapReloadSources(sources.toList)
    sources.foreach { s =>
      wrapTypedTree(s, forceReload = true)
    }
  }

  override def askShutdown(): Unit = {
    super.askShutdown()
  }

  /*
    * The following functions wrap up operations that interact with
    * the presentation compiler. The wrapping just helps with the
    * create response / compute / get result pattern.
    *
    * These units of work should return `Future[T]`.
    */
  def wrap[A](compute: Response[A] => Unit, handle: Throwable => A): A = {
    val result = new Response[A]
    compute(result)
    result.get.fold(o => o, handle)
  }

  def wrapReloadPosition(p: Position): Unit =
    wrapReloadSource(p.source)

  def wrapReloadSource(source: SourceFile): Unit =
    wrapReloadSources(List(source))

  def wrapReloadSources(sources: List[SourceFile]): Unit = {
    val superseded = scheduler.dequeueAll {
      case ri: ReloadItem if ri.sources == sources => Some(ri)
      case _ => None
    }
    superseded.foreach(_.response.set(()))
    wrap[Unit](r => new ReloadItem(sources, r).apply(), _ => ())
  }

  def wrapTypeMembers(p: Position): List[Member] =
    wrap[List[Member]](r => new AskTypeCompletionItem(p, r).apply(), _ => List.empty)

  def wrapTypedTree(source: SourceFile, forceReload: Boolean): Tree =
    wrap[Tree](r => new AskTypeItem(source, forceReload, r).apply(), t => throw t)

  def wrapTypedTreeAt(position: Position): Tree =
    wrap[Tree](r => new AskTypeAtItem(position, r).apply(), t => throw t)

  def wrapLinkPos(sym: Symbol, source: SourceFile): Position =
    wrap[Position](r => new AskLinkPosItem(sym, source, r).apply(), t => throw t)

}

