package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings, FatalError}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition, NoPosition}
import scala.actors._  
import scala.actors.Actor._  
import com.ensime.server.model._
import com.ensime.config.ProjectConfig
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap,LinkedHashMap }
import scala.tools.nsc.symtab.Types
import scala.tools.nsc.symtab.Flags



trait RichCompilerControl extends CompilerControl{ self: RichPresentationCompiler =>


  def askOr[A](op: () => A, handle:Throwable => A): A = {
    val result = new Response[A]()
    scheduler postWorkItem new WorkItem {
      def apply() = respond(result)(op())
    }
    result.get.fold( o => o, handle)
  }

  def askSymbolInfoAt(p: Position):SymbolInfo = askOr(
    () => symbolAt(p).fold(s => SymbolInfo(s), t => SymbolInfo.nullInfo), 
    t => SymbolInfo.nullInfo)

  def askTypeInfoAt(p: Position):TypeInfo = askOr(
    () => typeAt(p).fold(s => TypeInfo(s), t => TypeInfo.nullInfo), 
    t => TypeInfo.nullInfo)

  def askTypeInfoById(id:Int):TypeInfo = askOr(
    () => typeById(id) match {
      case Some(t) => TypeInfo(t)
      case None => TypeInfo.nullInfo
    }, 
    t => TypeInfo.nullInfo)

  def askCallCompletionInfoById(id:Int):CallCompletionInfo = askOr(
    () => typeById(id) match {
      case Some(t) => CallCompletionInfo(t)
      case None => CallCompletionInfo.nullInfo
    }, 
    t => CallCompletionInfo.nullInfo)

  def askPackageByPath(path:String):PackageInfo = askOr(
    () => PackageInfo.fromPath(path),
    t => PackageInfo.nullInfo)

  def askQuickReloadFile(f:SourceFile){
    askOr( () => reloadSources(List(f)), t => ())
  }

  def askReloadFile(f:SourceFile){
    val x = new Response[Unit]()
    askReload(List(f), x)
    x.get
  }

  def askReloadAllFiles() {
    val all = ((config.sourceFilenames.map(getSourceFile(_))) ++ firsts).toSet.toList
    val x = new Response[Unit]()
    askReload(all, x)
    x.get
  }

  def askInspectTypeById(id:Int):TypeInspectInfo = askOr(
    () => typeById(id) match {
      case Some(t) => inspectType(t)
      case None => TypeInspectInfo.nullInfo
    }, 
    t => TypeInspectInfo.nullInfo)

  def askInspectTypeAt(p: Position):TypeInspectInfo = askOr(
    () => inspectTypeAt(p),
    t => TypeInspectInfo.nullInfo)


  def askCompleteSymbolAt(p: Position, prefix:String, constructor:Boolean):List[SymbolInfoLight] = askOr(
    () => {
      reloadAndRecompileFiles(List(p.source))
      completeSymbolAt(p, prefix, constructor)
    },
    t => List())

  def askCompleteMemberAt(p: Position, prefix:String):List[NamedTypeMemberInfoLight] = askOr(
    () => {
      reloadAndRecompileFiles(List(p.source))
      completeMemberAt(p, prefix)
    },
    t => List())

  def askClearTypeCache() = clearTypeCache

  def askNewRunnerThread() = newRunnerThread

  def sourceFileForPath(path:String) = getSourceFile(path)


}


class RichPresentationCompiler(settings:Settings, reporter:Reporter, var parent:Actor, val config:ProjectConfig) extends Global(settings,reporter) with ModelBuilders with RichCompilerControl{

  import Helpers._

  import analyzer.{SearchResult, ImplicitSearch}

  private def typePublicMembers(tpe:Type):List[TypeMember] = {
    val scope = new Scope
    val members = new LinkedHashMap[Symbol, TypeMember]
    def addTypeMember(sym:Symbol, pre:Type, inherited:Boolean, viaView:Symbol) {
      val symtpe = pre.memberType(sym)
      if (scope.lookupAll(sym.name) forall (sym => !(members(sym).tpe matches symtpe))) {
	scope enter sym
	members(sym) = new TypeMember(
	  sym,
	  symtpe,
	  sym.isPublic,
	  inherited,
	  viaView)
      }
    }
    for (sym <- tpe.decls){
      addTypeMember(sym, tpe, false, NoSymbol)
    }
    for (sym <- tpe.members){
      addTypeMember(sym, tpe, true, NoSymbol)
    }
    members.values.toList
  }


  private def getMembersForTypeAt(p:Position):List[Member] = {
    typeAt(p) match{
      case Left(tpe) => {
	if(isNoParamArrowType(tpe)){
	  typePublicMembers(typeOrArrowTypeResult(tpe))
	}
	else{
	  val members = typeMembers(p)
	  (members ++ typePublicMembers(tpe)).toSet.toList
	}
      }
      case Right(e) => {
	System.err.println("ERROR: Failed to get any type information :(  " + e)
	List()
      }
    }
  }

  private def prepareSortedInterfaceInfo(members:List[Member]):Iterable[InterfaceInfo] = {
    // ...filtering out non-visible and non-type members
    val visMembers:List[TypeMember] = members.flatMap {
      case m@TypeMember(sym, tpe, true, _, _) => List(m)
      case _ => List()
    }


    // Create a list of pairs [(typeSym, membersOfSym)]
    val membersByOwner = visMembers.groupBy{
      case TypeMember(sym, _, _, _, _) => {
	sym.owner
      }
    }.toList.sortWith{
      // Sort the pairs on the subtype relation
      case ((s1,_),(s2,_)) => s1.tpe <:< s2.tpe
    }

    membersByOwner.map{
      case (ownerSym, members) => {

	// If all the members in this interface were
	// provided by the same view, remember that 
	// view for later display to user.
	val byView = members.groupBy(_.viaView)
	val viaView = if(byView.size == 1){
	  byView.keys.headOption.filter(_ != NoSymbol)
	} else {None}

	// Transform to [typeInfo]*
	val memberInfos = members.map{ m =>
	  NamedTypeMemberInfo(m)
	}

	val nestedTypes = (memberInfos.filter(_.declaredAs != 'method)
	  .sortWith((a,b) => a.name <= b.name))	
	val constructors = memberInfos.filter(_.name == "this")
	val others = (memberInfos.filter(m => 
	    m.declaredAs == 'method && m.name != "this")
	  .sortWith((a,b) => a.name <= b.name))

	val sortedInfos = nestedTypes ++ constructors ++ others

	new InterfaceInfo(TypeInfo(ownerSym.tpe, sortedInfos),
	  viaView.map(_.name.toString))
      }
    }
  }

  protected def inspectType(tpe:Type):TypeInspectInfo = {
    new TypeInspectInfo(
      TypeInfo(tpe),
      prepareSortedInterfaceInfo(typePublicMembers(tpe.asInstanceOf[Type]))
    )
  }

  protected def inspectTypeAt(p: Position):TypeInspectInfo = {
    val members = getMembersForTypeAt(p)
    val preparedMembers = prepareSortedInterfaceInfo(members)
    typeAt(p) match {
      case Left(t) => new TypeInspectInfo(TypeInfo(t), preparedMembers)
      case Right(_) => TypeInspectInfo.nullInfo
    }
  }

  private def typeOfTree(t:Tree):Either[Type, Throwable] = {
    var tree = t
    tree = tree match {
      case Select(qual, name) if tree.tpe == ErrorType => 
      {
	qual
      }
      case t:ImplDef if t.impl != null => 
      {
	t.impl
      }
      case t:ValOrDefDef if t.tpt != null => 
      {
	t.tpt
      }
      case t:ValOrDefDef if t.rhs != null => 
      {
	t.rhs
      }
      case t => t
    }
    if(tree.tpe != null) {
      Left(tree.tpe)
    }
    else {
      Right(new Exception("Null tpe"))
    }
  }

  protected def typeAt(p: Position):Either[Type, Throwable] = {
    val tree = typedTreeAt(p)
    typeOfTree(tree)
  }

  protected def symbolAt(p: Position):Either[Symbol, Throwable] = {
    p.source.file
    val tree = typedTreeAt(p)
    if(tree.symbol != null){
      Left(tree.symbol)
    }
    else{
      Right(new Exception("Null sym"))
    }
  }

  protected def completeSymbolAt(p: Position, prefix:String, constructor:Boolean):List[SymbolInfoLight] = {
    val names = scopeMembers(p)
    val visibleNames = names.flatMap{ m => 
      m match{
	case ScopeMember(sym, tpe, true, viaImport) => {
	  if(sym.nameString.startsWith(prefix)){
	    if(constructor){
	      SymbolInfoLight.constructorSynonyms(sym)
	    }
	    else{
	      val synonyms = SymbolInfoLight.applySynonyms(sym)
	      List(SymbolInfoLight(sym, tpe)) ++ synonyms
	    }
	  }
	  else{
	    List()
	  }
	}
	case _ => List()
      }
    }.sortWith((a,b) => a.name <= b.name)
    visibleNames
  }

  protected def completeMemberAt(p: Position, prefix:String):List[NamedTypeMemberInfoLight] = {
    val members = getMembersForTypeAt(p)
    val visibleMembers = members.flatMap{
      case tm@TypeMember(sym, tpe, true, _, _) => {
	if(sym.nameString.startsWith(prefix)){
	  List(NamedTypeMemberInfoLight(tm))
	}
	else{
	  List()
	}
      }
      case _ => List()
    }.sortWith((a,b) => a.name <= b.name)
    visibleMembers
  }


  // We ocassionally need to invoke recompile(...)
  // manually, in which case we don't want to 
  // send error/warning notes.
  private var enableFullTypeCheckEvents = true
  protected def withoutTypeCheckEvents(action:() => Unit) = {
    enableFullTypeCheckEvents = false
    try{
      action()
    }
    finally{
      enableFullTypeCheckEvents = true
    }
  }

  /**
  * Override so we send a notification to compiler actor when finished..
  */
  override def recompile(units: List[RichCompilationUnit]) {
    super.recompile(units)
    if(enableFullTypeCheckEvents){
      parent ! FullTypeCheckCompleteEvent()
    }
  }

  def reloadAndRecompileFiles(sources:List[SourceFile]) = {
    val units = sources.map{ s =>
      val unit = new RichCompilationUnit(s)
      unitOfFile(s.file) = unit
      unit
    }
    withoutTypeCheckEvents(() => recompile(units))
  }

  override def askShutdown(){
    super.askShutdown()
    parent = null
  }

  override def finalize() {
    System.out.println("Finalizing Global instance.")
  }

}


