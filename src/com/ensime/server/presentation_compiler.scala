package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.actors._  
import scala.actors.Actor._  
import com.ensime.server.model._
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap,LinkedHashMap }
import scala.tools.nsc.symtab.Types



class PresentationCompiler(settings:Settings, reporter:Reporter, parent:Actor, srcFiles:Iterable[String]) extends Global(settings,reporter) with ModelBuilders{


  /**
  * Override so we send a notification to compiler actor when finished..
  */
  override def recompile(units: List[RichCompilationUnit]) {
    super.recompile(units)
    parent ! FullTypeCheckCompleteEvent()
    parent
  }

  def blockingReloadAll() {
    val all = ((srcFiles.map(getSourceFile(_))) ++ firsts).toSet.toList
    val x = new Response[Unit]()
    askReload(all, x)
    x.get
  }

  /** 
  *  Make sure a set of compilation units is loaded and parsed,
  *  but do not trigger a full recompile.
  */
  private def quickReload(sources: List[SourceFile], result: Response[Unit]) {
    respond(result)(reloadSources(sources))
  }

  /** 
  *  Make sure a set of compilation units is loaded and parsed,
  *  but do not trigger a full recompile.
  *  Return () to syncvar `result` on completion.
  */
  def askQuickReload(sources: List[SourceFile], result: Response[Unit]) = 
  scheduler postWorkItem new WorkItem {
    def apply() = quickReload(sources, result)
    override def toString = "quickReload " + sources
  }

  import analyzer.{SearchResult, ImplicitSearch}

  private def typePublicMembers(tpe:Type):List[TypeMember] = {
    val scope = new Scope
    val members = new LinkedHashMap[Symbol, TypeMember]
    def addTypeMember(sym: Symbol, pre: Type, inherited: Boolean, viaView: Symbol) {
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
    val x2 = new Response[List[Member]]()
    askTypeCompletion(p, x2)
    x2.get match{
      case Left(m) => m
      case Right(e) => {
	// Oops, failed to get the members for type at p.
	// Try again, just asking for the type.
	//
	// TODO: why are these answers different?
	//
	getTypeAt(p) match{
	  case Left(tpe) => {
	    typePublicMembers(tpe)
	  }
	  case Right(e) => {
	    // Still nothing :'(
	    System.err.println("ERROR: Failed to get any type information :(  " + e)
	    List()
	  }
	}
      }
    }
  }

  def prepareSortedSupersInfo(members:List[Member]):Iterable[NamedTypeInfo] = {
    // ...filtering out non-visible and non-type members
    val visMembers:List[TypeMember] = members.flatMap {
      case m@TypeMember(sym, tpe, true, _, _) => List(m)
      case _ => List()
    }

    // create a list of pairs [(sym, members-of-sym)]
    // ..sort the pairs on the subtype relation
    val membersByOwner = visMembers.groupBy{
      case TypeMember(sym, _, _, _, _) => {
	sym.owner
      }
    }.toList.sortWith{
      case ((s1,_),(s2,_)) => s1.tpe <:< s2.tpe
    }

    // transform to [named-type-info]..
    membersByOwner.map{
      case (ownerSym, members) => {
	val memberInfos = members.map{
	  case TypeMember(sym, tpe, _, _, _) => {
	    val typeInfo = TypeInfo(tpe)
	    new NamedTypeMemberInfo(sym.nameString, typeInfo, sym.pos)
	  }
	}.sortWith{(a,b) => a.name <= b.name}
	val ownerTpeInfo = TypeInfo(ownerSym.tpe)
	NamedTypeInfo(ownerTpeInfo, memberInfos)
      }
    }
  }


  def inspectType(tpe:Type):TypeInspectInfo = {
    new TypeInspectInfo(
      TypeInfo(tpe),
      prepareSortedSupersInfo(typePublicMembers(tpe.asInstanceOf[Type]))
    )
  }

  def inspectTypeAt(p: Position):TypeInspectInfo = {
    blockingQuickReload(p.source)
    val members = getMembersForTypeAt(p)
    val preparedMembers = prepareSortedSupersInfo(members)
    // Grab the type at position..
    val typeInfo = getTypeAt(p) match{
      case Left(tpe) => {
	TypeInfo(tpe)
      }
      case Right(e) => {
	TypeInfo.nullInfo
      }
    }
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

  def getTypeAt(p: Position):Either[Type, Throwable] = {
    // Grab the type at position..
    val x1 = new Response[Tree]()
    askTypeAt(p, x1)
    x1.get match{
      case Left(tree) => {
	if(tree.tpe != null) Left(tree.tpe)
	else Right(new Exception("Null tpe"))
      }
      case Right(e) => {
	Right(e)
      }
    }
  }

  def completeSymbolAt(p: Position, prefix:String):List[ScopeNameInfoLight] = {
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
	    List(new ScopeNameInfoLight(
		sym.nameString, 
		tpe.underlying.toString,
		cacheType(tpe)))
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
    blockingQuickReload(p.source)
    val members = getMembersForTypeAt(p)
    val visibleMembers = members.flatMap{
      case TypeMember(sym, tpe, true, _, _) => {
	if(sym.nameString.startsWith(prefix)){
	  List(new NamedTypeMemberInfoLight(sym.nameString, tpe.underlying.toString, cacheType(tpe)))
	}
	else{
	  List()
	}
      }
      case _ => List()
    }.sortWith((a,b) => a.name <= b.name)
    visibleMembers
  }

  def blockingQuickReload(f:SourceFile){
    val x = new Response[Unit]()
    askQuickReload(List(f), x)
    x.get
  }

  def blockingFullReload(f:SourceFile){
    val x = new Response[Unit]()
    askReload(List(f), x)
    x.get
  }

}


