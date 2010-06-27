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



class RichPresentationCompiler(settings:Settings, reporter:Reporter, var parent:Actor, config:ProjectConfig) extends Global(settings,reporter) with ModelBuilders{

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
    getTypeAt(p) match{
      case Left(tpe) => {
	if(isNoParamArrowType(tpe)){
	  typePublicMembers(typeOrArrowTypeResult(tpe))	  
	}
	else{
	  val x2 = new Response[List[Member]]()
	  askTypeCompletion(p, x2)
	  x2.get match{
	    case Left(m) => {
	      (m ++ typePublicMembers(tpe)).toSet.toList
	    }
	    case Right(e) => {
	      typePublicMembers(tpe)
	    }
	  }
	}
      }
      case Right(e) => {
	System.err.println("ERROR: Failed to get any type information :(  " + e)
	List()
      }
    }
  }

  def prepareSortedInterfaceInfo(members:List[Member]):Iterable[InterfaceInfo] = {
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
	  new NamedTypeMemberInfo(m.sym.nameString, TypeInfo(m.tpe), m.sym.pos)
	}
	
	// Sort constructors to front.
	val sortedInfos = memberInfos.sortWith{(a,b) => 
	  if(a.name.equals("this")) true
	  else if(b.name.equals("this")) false
	  else a.name <= b.name
	}
	new InterfaceInfo(TypeInfo(ownerSym.tpe, memberInfos),
	  viaView.map(_.name.toString))
      }
    }
  }

  def symbolInfoAt(p: Position):SymbolInfo = {
    getSymbolAt(p) match{
      case Left(sym:Symbol) => SymbolInfo(sym)
      case _ => SymbolInfo.nullInfo
    }
  }

  def inspectType(tpe:Type):TypeInspectInfo = {
    new TypeInspectInfo(
      TypeInfo(tpe),
      prepareSortedInterfaceInfo(typePublicMembers(tpe.asInstanceOf[Type]))
    )
  }

  def inspectTypeAt(p: Position):TypeInspectInfo = {
    val members = getMembersForTypeAt(p)
    val preparedMembers = prepareSortedInterfaceInfo(members)
    val typeInfo = getTypeInfoAt(p)
    new TypeInspectInfo(typeInfo, preparedMembers)
  }


  def getTypeInfoAt(p: Position):TypeInfo = {
    getTypeAt(p) match{
      case Left(tpe) => {
	TypeInfo(tpe)
      }
      case Right(e) => {
	TypeInfo.nullInfo
      }
    }
  }

  def typeOfTree(t:Tree):Either[Type, Throwable] = {
    var tree = t
    println("Class of tree: " + tree.getClass)
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

  def getTypeAt(p: Position, retryOnFatal:Boolean = true):Either[Type, Throwable] = {
    // Grab the type at position..
    val x1 = new Response[Tree]()
    askTypeAt(p, x1)
    x1.get match{
      case Left(tree) => {
	typeOfTree(tree)
      }
      case Right(e:FatalError) => {
	if(retryOnFatal){
	  System.err.println("Got fatal error.. retrying...")
	  getTypeAt(p, false)
	}
	else{
	  Right(e)
	}
      }
      case Right(e) => {
	Right(e)
      }
    }
  }

  def getSymbolAt(p: Position):Either[Symbol, Throwable] = {
    val x1 = new Response[Tree]()
    askTypeAt(p, x1)
    x1.get match{
      case Left(t) => Left(t.symbol)
      case Right(e) => Right(e)
    }
  }

  def completeSymbolAt(p: Position, prefix:String, constructor:Boolean):List[SymbolInfoLight] = {
    val x = new Response[List[Member]]()
    askScopeCompletion(p, x)
    val names = x.get match{
      case Left(m) => m
      case Right(e) => List()
    }
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

  def completeMemberAt(p: Position, prefix:String):List[NamedTypeMemberInfoLight] = {
    blockingQuickReloadFile(p.source)
    val members = getMembersForTypeAt(p)
    val visibleMembers = members.flatMap{
      case TypeMember(sym, tpe, true, _, _) => {
	if(sym.nameString.startsWith(prefix)){
	  List(new NamedTypeMemberInfoLight(sym.nameString, tpe.underlying.toString, cacheType(tpe), isArrowType(tpe)))
	}
	else{
	  List()
	}
      }
      case _ => List()
    }.sortWith((a,b) => a.name <= b.name)
    visibleMembers
  }

  def blockingQuickReloadFile(f:SourceFile){
    val x = new Response[Unit]()
    askQuickReload(List(f), x)
    x.get
  }

  def blockingReloadFile(f:SourceFile){
    val x = new Response[Unit]()
    askReload(List(f), x)
    x.get
  }

  def blockingReloadAllFiles() {
    val all = ((config.sourceFilenames.map(getSourceFile(_))) ++ firsts).toSet.toList
    val x = new Response[Unit]()
    askReload(all, x)
    x.get
  }


  /**
  * Override so we send a notification to compiler actor when finished..
  */
  override def recompile(units: List[RichCompilationUnit]) {
    println("RECOMPILING: " + units)
    super.recompile(units)
    parent ! FullTypeCheckCompleteEvent()
  }

  /** 
  *  Make sure a set of compilation units is loaded and parsed,
  *  but do not trigger a full recompile.
  *  Return () to syncvar `result` on completion.
  */
  def askQuickReload(sources: List[SourceFile], result: Response[Unit]) = 
  scheduler postWorkItem new WorkItem {
    def apply() = respond(result)(reloadSources(sources))
    override def toString = "quickReload " + sources
  }


  override def askShutdown(){
    super.askShutdown()
    parent = null
  }

  override def finalize() {
    System.out.println("Finalizing Global instance.")
  }

}


