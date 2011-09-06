/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

package org.ensime.server
import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.model._
import scala.actors.Actor._
import scala.actors.Actor
import scala.collection.mutable
import scala.tools.nsc.interactive.{CompilerControl, Global}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.symtab.Types
import scala.tools.nsc.util.{Position, RangePosition, SourceFile}
import scala.tools.nsc.Settings
import scala.tools.refactoring.analysis.GlobalIndexes

trait RichCompilerControl extends CompilerControl with RefactoringControl { self: RichPresentationCompiler =>

  def askOr[A](op: => A, handle: Throwable => A): A = {
    val result = new Response[A]()
    scheduler doQuickly new WorkItem {
      def apply() = respond(result)(op)
    }
    result.get.fold(o => o, handle)
  }

  def askSymbolInfoAt(p: Position): Option[SymbolInfo] = askOr(
    symbolAt(p).fold(s => Some(SymbolInfo(s)), t => None), t => None)

  def askTypeInfoAt(p: Position): Option[TypeInfo] = askOr(
    typeAt(p).fold(s => Some(TypeInfo(s)), t => None), t => None)

  def askTypeInfoById(id: Int): Option[TypeInfo] = askOr(
    typeById(id).map { t => TypeInfo(t) }, t => None)

  def askTypeInfoByName(name: String): Option[TypeInfo] = askOr(
    typeByName(name).map { t => TypeInfo(t) }, t => None)

  def askTypeInfoByNameAt(name: String, p: Position): Option[TypeInfo] = askOr(
    typeByNameAt(name, p).map { t => TypeInfo(t) }, t => None)

  def askCallCompletionInfoById(id: Int): Option[CallCompletionInfo] = askOr(
    typeById(id).map { t => CallCompletionInfo(t) }, t => None)

  def askPackageByPath(path: String): Option[PackageInfo] = askOr(
    Some(PackageInfo.fromPath(path)), t => None)

  def askReloadFile(f: SourceFile) {
    askReloadFiles(List(f))
  }

  def askReloadFiles(files: Iterable[SourceFile]) {
    val x = new Response[Unit]()
    askReload(files.toList, x)
    x.get
  }

  def askRemoveAllDeleted() = askOr(removeAllDeleted(), t => ())

  def askRemoveDeleted(f: File) = askOr(removeDeleted(AbstractFile.getFile(f)), t => ())

  def askReloadAllFiles() = {
    val all = ((config.sourceFilenames.map(getSourceFile(_))) ++
      allSources).toSet.toList
    askReloadFiles(all)
  }

  def askInspectTypeById(id: Int): Option[TypeInspectInfo] = askOr(
    typeById(id).map { t => inspectType(t) }, t => None)

  def askInspectTypeAt(p: Position): Option[TypeInspectInfo] = askOr({
      inspectTypeAt(p)
    }, t => None)

  def askCompletePackageMember(path: String, prefix: String): Iterable[PackageMemberInfoLight] = askOr({
      completePackageMember(path, prefix)
    }, t => List())

  def askCompleteSymbolAt(p: Position, prefix: String, constructor: Boolean): List[SymbolInfoLight] = {
    // Make sure new tree is loaded
    askReloadFile(p.source)
    askOr({
	completeSymbolAt(p, prefix, constructor)
      }, t => List())
  }

  def askCompleteMemberAt(p: Position, prefix: String): List[NamedTypeMemberInfoLight] = {
    askReloadFile(p.source)
    askOr({
	completeMemberAt(p, prefix)
      }, t => List())
  }

  def askReloadAndTypeFiles(files: Iterable[SourceFile]) = askOr({
      reloadAndTypeFiles(files)
    }, t => ())

  def askUsesOfSymAtPoint(p: Position): List[RangePosition] = askOr({
      usesOfSymbolAtPoint(p).toList
    }, t => List())

  def askClearTypeCache() = clearTypeCache

  def askNotifyWhenReady() = ask(setNotifyWhenReady)

  def sourceFileForPath(path: String) = getSourceFile(path)

}

class RichPresentationCompiler(
  settings: Settings,
  reporter: Reporter,
  var parent: Actor,
  var indexer: Actor,
  val config: ProjectConfig) extends Global(settings, reporter)
with Helpers with NamespaceTraversal with ModelBuilders with RichCompilerControl
with RefactoringImpl with IndexerInterface {

  import ModelHelpers._

  private val symsByFile = new mutable.HashMap[AbstractFile, mutable.LinkedHashSet[Symbol]] {
    override def default(k: AbstractFile) = {
      val v = new mutable.LinkedHashSet[Symbol]
      put(k, v)
      v
    }
  }

  override val debugIDE: Boolean = true
  override val verboseIDE: Boolean = true

  private val newTopLevelSyms = new mutable.LinkedHashSet[Symbol]

  /** Called from typechecker every time a top-level class or object is entered.*/
  override def registerTopLevelSym(sym: Symbol) {
    super.registerTopLevelSym(sym)
    symsByFile(sym.sourceFile) += sym
    newTopLevelSyms += sym
  }

  /**
  * Remove symbols defined by files that no longer exist.
  * Note that these symbols will not be collected by
  * syncTopLevelSyms, since the units in question will
  * never be reloaded again.
  */
  def removeAllDeleted() {
    allSources = allSources.filter { _.file.exists }
    val deleted = symsByFile.keys.filter { !_.exists }
    for (f <- deleted) {
      removeDeleted(f)
    }
  }

  /** Remove symbols defined by file that no longer exist. */
  def removeDeleted(f: AbstractFile) {
    val syms = symsByFile(f)
    unindexTopLevelSyms(syms)
    for (s <- syms) {
      s.owner.info.decls unlink s
    }
    symsByFile.remove(f)
    unitOfFile.remove(f)
  }

  override def syncTopLevelSyms(unit: RichCompilationUnit) {
    super.syncTopLevelSyms(unit)
    unindexTopLevelSyms(deletedTopLevelSyms)
    indexTopLevelSyms(newTopLevelSyms)
    
    //    WARNING: Clearing the set here makes 
    //    recentlyDeleted useless.
    deletedTopLevelSyms.clear()
    newTopLevelSyms.clear()
  }

  private def typePublicMembers(tpe: Type): Iterable[TypeMember] = {
    val members = new mutable.LinkedHashMap[Symbol, TypeMember]
    def addTypeMember(sym: Symbol, pre: Type, inherited: Boolean, viaView: Symbol) {
      try {
	val m = new TypeMember(
          sym,
          sym.tpe,
          sym.isPublic,
          inherited,
          viaView)
	members(sym) = m
      } catch {
	case e =>
	System.err.println("Error: Omitting member " + sym + ": " + e)
      }
    }
    for (sym <- tpe.decls) {
      addTypeMember(sym, tpe, false, NoSymbol)
    }
    for (sym <- tpe.members) {
      addTypeMember(sym, tpe, true, NoSymbol)
    }
    members.values
  }

  private def getMembersForTypeAt(p: Position): Iterable[Member] = {
    typeAt(p) match {
      case Left(tpe) => {
	if (isNoParamArrowType(tpe)) {
          typePublicMembers(typeOrArrowTypeResult(tpe))
	} else {
          val members: Iterable[Member] = try {

            wrapTypeMembers(p)

          } catch {
            case e => {
	      System.err.println("Error retrieving type members:")
	      e.printStackTrace(System.err)
	      List()
            }
          }
          // Remove duplicates
          // Filter out synthetic things
          val bySym = new mutable.LinkedHashMap[Symbol, Member]
          for (m <- (members ++ typePublicMembers(tpe))) {
            if (!m.sym.nameString.contains("$")) {
	      bySym(m.sym) = m
            }
          }
          bySym.values
	}
      }
      case Right(e) => {
	System.err.println("ERROR: Failed to get any type information :(  " + e)
	List()
      }
    }
  }

  protected def inspectType(tpe: Type): TypeInspectInfo = {
    new TypeInspectInfo(
      TypeInfo(tpe),
      companionTypeOf(tpe).map(cacheType),
      prepareSortedInterfaceInfo(typePublicMembers(tpe.asInstanceOf[Type])))
  }

  protected def inspectTypeAt(p: Position): Option[TypeInspectInfo] = {
    val members = getMembersForTypeAt(p)
    val preparedMembers = prepareSortedInterfaceInfo(members)
    typeAt(p) match {
      case Left(t) => {
	Some(new TypeInspectInfo(
            TypeInfo(t),
            companionTypeOf(t).map(cacheType),
            preparedMembers))
      }
      case Right(_) => None
    }
  }

  private def typeOfTree(t: Tree): Either[Type, Throwable] = {
    var tree = t
    tree = tree match {
      case Select(qual, name) if tree.tpe == ErrorType => {
	qual
      }
      case t: ImplDef if t.impl != null => {
	t.impl
      }
      case t: ValOrDefDef if t.tpt != null => {
	t.tpt
      }
      case t: ValOrDefDef if t.rhs != null => {
	t.rhs
      }
      case t => t
    }
    if (tree.tpe != null) {
      Left(tree.tpe)
    } else {
      Right(new Exception("Null tpe"))
    }
  }

  protected def typeAt(p: Position): Either[Type, Throwable] = {
    val tree = wrapTypedTreeAt(p)
    typeOfTree(tree)
  }

  protected def typeByName(name: String): Option[Type] = {
    def maybeType(sym: Symbol) = sym match {
      case NoSymbol => None
      case sym: Symbol => Some(sym.tpe)
      case _ => None
    }
    try {
      if (name.endsWith("$")) {
	maybeType(definitions.getModule(name.substring(0, name.length - 1)))
      } else {
	maybeType(definitions.getClass(name))
      }
    } catch {
      case e => None
    }
  }

  protected def typeByNameAt(nameStr: String, p: Position): Option[Type] = {
    val matchingSyms = lookupSymbolByNameAt(nameStr, p)
    matchingSyms.filter { _.tpe != NoType }.headOption.map { _.tpe }
  }

  protected def lookupSymbolByNameAt(nameStr: String, p: Position): List[Symbol] = {
    val nameSegs = nameStr.split("\\.")
    val firstName: String = nameSegs.head

    val roots = scopeMembers(p, firstName, true).map { _.sym }

    if (nameSegs.length > 1) {
      val restOfPath: String = nameSegs.drop(1).mkString(".")
      roots.flatMap { r => symsAtQualifiedPath(restOfPath, r) }
    } else {
      roots
    }
  }

  protected def symbolAt(p: Position): Either[Symbol, Throwable] = {
    p.source.file
    val tree = wrapTypedTreeAt(p)
    if (tree.symbol != null) {
      Left(tree.symbol)
    } else {
      Right(new Exception("Null sym"))
    }
  }

  /**
  * Override scopeMembers to fix issues with finding method params
  * and occasional exception in pre.memberType. Hopefully we can
  * get these changes into Scala.
  */
  def scopeMembers(pos: Position, prefix: String, exactMatch: Boolean): List[ScopeMember] = {
    wrapTypedTreeAt(pos) // to make sure context is entered
    locateContext(pos) match {
      case Some(context) => {
	val locals = new mutable.LinkedHashMap[Symbol, ScopeMember]
	def addSymbol(sym: Symbol, pre: Type, viaImport: Tree) = {
          try {
            val ns = sym.nameString
            val accessible = context.isAccessible(sym, pre, false)
            if (accessible && ((exactMatch && ns == prefix)
		|| (!exactMatch && ns.startsWith(prefix))) &&
	      !sym.nameString.contains("$") &&
	      !locals.contains(sym)) {
	      val member = new ScopeMember(
		sym,
		sym.tpe,
		accessible,
		viaImport)
	      locals(sym) = member
            }
          } catch {
            case e: Exception => {
	      System.err.println("Error: Omitting scope member.")
	      e.printStackTrace(System.err)
            }
	  }
	}
	var cx = context
	while (cx != NoContext) {
	  for (sym <- cx.scope) {
            addSymbol(sym, NoPrefix, EmptyTree)
	  }
	  if (cx.prefix != null) {
            for (sym <- cx.prefix.members) {
	      addSymbol(sym, cx.prefix, EmptyTree)
            }
	  }
	  cx = cx.outer
	}
	for (imp <- context.imports) {
	  val pre = imp.qual.tpe
	  val importedSyms = pre.members.flatMap(transformImport(
	      imp.tree.selectors, _))
	  for (sym <- importedSyms) {
            addSymbol(sym, pre, imp.qual)
	  }
	}
	val result = locals.values.toList
	result
      }
      case _ => List()
    }
  }

  // TODO: 
  // This hides the core implementation is Contexts.scala, which
  // has been patched. Once this bug is fixed, we can get rid of 
  // this workaround.
  private def transformImport(selectors: List[ImportSelector], sym: Symbol): List[Symbol] = selectors match {
    case List() => List()
    case List(ImportSelector(nme.WILDCARD, _, _, _)) => List(sym)
    case ImportSelector(from, _, to, _) :: _ if (from.toString == sym.name.toString) =>
    if (to == nme.WILDCARD) List()
    else { val sym1 = sym.cloneSymbol; sym1.name = to; List(sym1) }
    case _ :: rest => transformImport(rest, sym)
  }

  protected def completePackageMember(path: String, prefix: String): Iterable[PackageMemberInfoLight] = {
    packageSymFromPath(path) match {
      case Some(sym) => {
	val memberSyms = packageMembers(sym).filterNot { s =>
          s == NoSymbol || s.nameString.contains("$")
	}
	memberSyms.flatMap { s =>
          val name = if (s.isPackage) { s.nameString } else { typeShortName(s) }
          if (name.startsWith(prefix)) {
            Some(new PackageMemberInfoLight(name))
          } else None
	}
      }
      case _ => List()
    }
  }

  protected def completeSymbolAt(p: Position, prefix: String, constructor: Boolean): List[SymbolInfoLight] = {
    val names = scopeMembers(p, prefix, false)
    val result = new mutable.LinkedHashSet[SymbolInfoLight]
    names.foreach { m =>
      m match {
	case ScopeMember(sym, tpe, true, _) => {
          if (constructor) {
            result ++= SymbolInfoLight.constructorSynonyms(sym)
          } else {
            result += SymbolInfoLight(sym, tpe)
            result ++= SymbolInfoLight.applySynonyms(sym)
          }
	}
	case _ => {}
      }
    }
    result.toList.sortWith((a, b) => a.name.length <= b.name.length)
  }

  protected def completeMemberAt(p: Position, prefix: String): List[NamedTypeMemberInfoLight] = {
    val members = getMembersForTypeAt(p)
    val visibleMembers = members.flatMap {
      case tm @ TypeMember(sym, tpe, true, _, _) => {
	val s = sym.nameString
	if (s.startsWith(prefix) &&
          !(s == "this") &&
          !(s == "→")) {
          List(NamedTypeMemberInfoLight(tm))
	} else {
          List()
	}
      }
      case _ => List()
    }.toList.sortWith((a, b) => a.name.length <= b.name.length)
    visibleMembers
  }

  protected def usesOfSymbolAtPoint(p: Position): Iterable[RangePosition] = {
    symbolAt(p) match {
      case Left(s) => {
	val gi = new GlobalIndexes {
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
		p.source, p.point, p.point, p.point)
            }
          }
	}
	gi.result
      }
      case Right(e) => List()
    }
  }

  private var notifyWhenReady = false

  override def isOutOfDate():Boolean = {
    if (notifyWhenReady && !super.isOutOfDate) {
      parent ! FullTypeCheckCompleteEvent()
      notifyWhenReady = false
    }
    super.isOutOfDate
  }

  protected def setNotifyWhenReady() {
    notifyWhenReady = true
  }

  protected def reloadAndTypeFiles(sources: Iterable[SourceFile]) = {
    wrapReloadSources(sources.toList)
    sources.foreach { s =>
      wrapTypedTree(s, true)
    }
  }

  override def askShutdown() {
    super.askShutdown()
    parent = null
    indexer = null
  }

  override def finalize() {
    System.out.println("Finalizing Global instance.")
  }

  /*
  * The following functions wrap up operations that interact with
  * the presentation compiler. The wrapping just helps with the
  * create response / compute / get result pattern.
  *
  * These units of work should probably be wrapped up into a
  * Work monad that will make it easier to compose the operations.
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
    val superseeded = scheduler.dequeueAll {
      case ri: ReloadItem if ri.sources == sources => Some(ri)
      case _ => None
    }
    superseeded.foreach(_.response.set())
    wrap[Unit](r => new ReloadItem(sources, r).apply(), _ => ())
  }

  def wrapTypeMembers(p: Position): List[Member] =
  wrap[List[Member]](r => new AskTypeCompletionItem(p, r).apply(), _ => List())

  def wrapTypedTree(source: SourceFile, forceReload: Boolean): Tree =
  wrap[Tree](r => new AskTypeItem(source, forceReload, r).apply(), t => throw t)

  def wrapTypedTreeAt(position: Position): Tree =
  wrap[Tree](r => new AskTypeAtItem(position, r).apply(), t => throw t)

}

