package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._  
import scala.actors.Actor._  
import scala.tools.nsc.io.{AbstractFile}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.reporters.Reporter
import scala.concurrent.SyncVar
import scala.collection.{Iterable, Map}
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }
import scala.collection.mutable.{ ArrayBuffer, SynchronizedMap,LinkedHashMap }
import scala.collection.immutable.TreeSet
import java.io.File
import scala.tools.nsc.ast._
import com.ensime.util.RichFile._ 
import scala.tools.nsc.symtab.Types


case class CompilationResultEvent(notes:List[Note])
case class TypeCompletionResultEvent(members:List[MemberInfoLight], callId:Int)
case class InspectTypeResultEvent(info:TypeInspectInfo, callId:Int)
case class TypeByIdResultEvent(info:TypeInfo, callId:Int)
case class InspectTypeByIdResultEvent(info:TypeInspectInfo, callId:Int)
case class ReloadFileEvent(file:File)
case class RemoveFileEvent(file:File)
case class ScopeCompletionEvent(file:File, point:Int)
case class TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int)
case class InspectTypeEvent(file:File, point:Int, callId:Int)
case class InspectTypeByIdEvent(id:Int, callId:Int)
case class TypeByIdEvent(id:Int, callId:Int)
case class CompilerShutdownEvent()

case class BackgroundCompileCompleteEvent()

class Compiler(project:Project, config:ProjectConfig) extends Actor{

  val rootDir = new File(config.rootDir)
  val cpFiles = config.classpath.map{ s =>
    val f = new File(rootDir, s)
    f.getAbsolutePath
  }
  val srcFiles = (
    for(s <- config.srcDirs;
      val srcRoot = new File(rootDir, s);
      f <- srcRoot.andTree if (f.getName.endsWith(".scala") && !f.isHidden))
    yield{
      f.getAbsolutePath
    })
  val args = List(
    "-cp", cpFiles.mkString(":"),
    "-verbose",
    srcFiles.mkString(" ")
  )

  println("Processed compiler args:" + args)

  val settings = new Settings(Console.println)
  settings.processArguments(args, false)
  val reporter = new PresentationReporter()


  class PresentationCompiler(settings:Settings, reporter:Reporter, parent:Actor) extends Global(settings,reporter){

    private val typeCache = new HashMap[Int, Types#Type]
    private val typeCacheReverse = new HashMap[Types#Type, Int]
    def clearTypeCache(){ 
      typeCache.clear
      typeCacheReverse.clear
    }
    def typeById(id:Int):Option[Types#Type] = { 
      typeCache.get(id)
    }
    def cacheType(tpe:Types#Type):Int = {
      if(typeCacheReverse.contains(tpe)){
	typeCacheReverse(tpe)
      }
      else{
	val id = typeCache.size + 1
	typeCache(id) = tpe
	typeCacheReverse(tpe) = id
	id
      }
    }

    /**
    * Override so we send a notification to compiler actor when finished..
    */
    override def recompile(units: List[RichCompilationUnit]) {
      super.recompile(units)
      parent ! BackgroundCompileCompleteEvent()
    }

    def blockingReloadAll() {
      val all = srcFiles.map(nsc.getSourceFile(_)).toList
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

    def prepareSortedMemberInfo(members:List[Member]):Iterable[(TypeInfo, Iterable[MemberInfo])] = {
      // ...filtering out non-visible and non-type members
      val visMembers:List[TypeMember] = members.flatMap {
	case m@TypeMember(sym, tpe, true, _, _) => List(m)
	case _ => List()
      }

      // create a list of pairs [(type, members-of-type)]
      // ..sort the pairs on the subtype relation
      val membersByOwner = visMembers.groupBy{
	case TypeMember(sym, _, _, _, _) => {
	  val ownerSym = sym.owner
	  val ownerType = ownerSym.tpe
	  ownerType
	}
      }.iterator.toList.sortWith{
	case ((t1,_),(t2,_)) => t1 <:< t2
      }

      // transform to [(type-info, member-infos-of-type)]..
      val memberInfosByOwnerInfo = membersByOwner.map{
	case (ownerTpe, members) => {
	  val memberInfos = members.map{
	    case TypeMember(sym, tpe, _, _, _) => {
	      val typeInfo = TypeInfo(tpe, cacheType)
	      MemberInfo(sym.nameString, typeInfo, sym.pos)
	    }
	  }.sortWith{(a,b) => a.name <= b.name}
	  (TypeInfo(ownerTpe, cacheType), memberInfos)
	}
      }

      memberInfosByOwnerInfo
    }

    def inspectType(tpe:Types#Type):TypeInspectInfo = {
      TypeInspectInfo(
	TypeInfo(tpe, cacheType), 
	prepareSortedMemberInfo(typePublicMembers(tpe.asInstanceOf[Type]))
      )
    }

    def inspectTypeAt(p: Position):TypeInspectInfo = {

      blockingQuickReload(p.source)

      // Grab the members at this position..
      val x2 = new nsc.Response[List[Member]]()
      askTypeCompletion(p, x2)
      val members:List[Member] = x2.get match{
	case Left(m) => m
	case Right(e) => List()
      }
      val preparedMembers = prepareSortedMemberInfo(members)

      // Grab the type at position..
      val x1 = new Response[Tree]()
      askTypeAt(p, x1)
      val typeInfo = x1.get match{
	case Left(tree) => {
	  TypeInfo(tree.tpe, cacheType)
	}
	case Right(e) => {
	  TypeInfo.nullTypeInfo
	}
      }

      TypeInspectInfo(typeInfo, preparedMembers)
    }

    def completeMemberAt(p: Position, prefix:String):List[MemberInfoLight] = {
      blockingQuickReload(p.source)
      val x2 = new Response[List[Member]]()
      askTypeCompletion(p, x2)
      val members = x2.get match{
	case Left(m) => m
	case Right(e) => List()
      }
      val visibleMembers = members.flatMap{
	case TypeMember(sym, tpe, true, _, _) => {
	  if(sym.nameString.startsWith(prefix)){
	    List(MemberInfoLight(sym.nameString, tpe.toString, cacheType(tpe)))
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
      nsc.askQuickReload(List(f), x)
      x.get
    }

    def blockingFullReload(f:SourceFile){
      val x = new Response[Unit]()
      nsc.askReload(List(f), x)
      x.get
    }

  }

  val nsc = new PresentationCompiler(settings, reporter, this)

  def act() {
    nsc.newRunnerThread
    project ! SendBackgroundMessageEvent("Compiler is parsing sources...")
    nsc.blockingReloadAll
    project ! SendBackgroundMessageEvent("Compiler finished parsing!")
    loop {
      try{
	receive {

	  case CompilerShutdownEvent => 
	  {
	    nsc.clearTypeCache
	    nsc.askShutdown()
	    exit('stop)
	  }

	  case ReloadFileEvent(file:File) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    nsc.blockingFullReload(f)
	    project ! CompilationResultEvent(reporter.allNotes)
	  }

	  case RemoveFileEvent(file:File) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    nsc.removeUnitOf(f)
	  }

	  case ScopeCompletionEvent(file:File, point:Int) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val x = new nsc.Response[List[nsc.Member]]()
	    nsc.askScopeCompletion(p, x)
	    val members = x.get match{
	      case Left(m) => m
	      case Right(e) => List()
	    }
	  }

	  case TypeCompletionEvent(file:File, point:Int, prefix:String, callId:Int) => 
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val members = nsc.completeMemberAt(p, prefix)
	    project ! TypeCompletionResultEvent(members, callId)
	  }

	  case InspectTypeEvent(file:File, point:Int, callId:Int) =>
	  {
	    val f = nsc.getSourceFile(file.getAbsolutePath())
	    val p = new OffsetPosition(f, point)
	    val inspectInfo = nsc.inspectTypeAt(p)
	    project ! InspectTypeResultEvent(inspectInfo, callId)
	  }

	  case InspectTypeByIdEvent(id:Int, callId:Int) =>
	  {
	    val inspectInfo = nsc.typeById(id) match{
	      case Some(tpe) => nsc.inspectType(tpe)
	      case None => TypeInspectInfo.nullInspectInfo
	    }
	    project ! InspectTypeResultEvent(inspectInfo, callId)
	  }

	  case TypeByIdEvent(id:Int, callId:Int) =>
	  {
	    val tpeInfo = nsc.typeById(id) match{
	      case Some(tpe) => TypeInfo(tpe, nsc.cacheType)
	      case None => TypeInfo.nullTypeInfo
	    }
	    project ! TypeByIdResultEvent(tpeInfo, callId)
	  }

	  case BackgroundCompileCompleteEvent() =>
	  {
	    project ! CompilationResultEvent(reporter.allNotes)
	  }

	  case other => 
	  {
	    println("Compiler: WTF, what's " + other)
	  }
	}

      }
      catch{
	case e:Exception =>
	{
	  System.err.println("Error at Compiler message loop: " + e + " :\n" + e.getStackTraceString)
	}
      }
    }
  }
}

case class MemberInfo(name:String, tpe:TypeInfo, pos:Position){}
case class MemberInfoLight(name:String, tpeName:String, tpeId:Int){}
class TypeInfo(
  val name:String, 
  val id:Int, 
  val declaredAs:scala.Symbol, 
  val fullName:String, 
  val pos:Position){}

object TypeInfo{

  type TypeCacher = Types#Type => Int

  def apply(tpe:Types#Type, cache:TypeCacher):TypeInfo = {
    tpe match{
      case tpe:Types#MethodType => 
      {
	ArrowTypeInfo(tpe, cache)
      }
      case tpe:Types#PolyType => 
      {
	ArrowTypeInfo(tpe, cache)
      }
      case tpe:Types#Type =>
      {
	val typeSym = tpe.typeSymbol
	val declAs = (
	  if(typeSym.isTrait)
	  'trait
	  else if(typeSym.isInterface)
	  'interface
	  else if(typeSym.isClass)
	  'class
	  else if(typeSym.isAbstractClass)
	  'abstractclass
	  else 'nil
	)
	new TypeInfo(tpe.toString, cache(tpe), declAs, typeSym.fullName, typeSym.pos)
      }
      case _ => nullTypeInfo
    }
  }
  def nullTypeInfo() = {
    new TypeInfo("NA", -1, 'nil, "NA", NoPosition)
  }
}

class ArrowTypeInfo(
  override val name:String, 
  override val id:Int, 
  val resultType:TypeInfo, 
  val paramTypes:Iterable[TypeInfo]) extends TypeInfo(name, id, 'nil, name, NoPosition){}

object ArrowTypeInfo{

  type TypeCacher = Types#Type => Int

  def apply(tpe:Types#MethodType, cache:TypeCacher):ArrowTypeInfo = {
    new ArrowTypeInfo(
      tpe.toString, 
      cache(tpe), 
      TypeInfo(tpe.resultType, cache), 
      tpe.paramTypes.map(t => TypeInfo(t,cache)))
  }
  def apply(tpe:Types#PolyType, cache:TypeCacher):ArrowTypeInfo = {
    new ArrowTypeInfo(
      tpe.toString, 
      cache(tpe), 
      TypeInfo(tpe.resultType, cache), 
      tpe.paramTypes.map(t => TypeInfo(t,cache)))
  }
  def nullTypeInfo() = {
    new TypeInfo("NA", -1, 'class, "NA", NoPosition)
  }
}


case class TypeInspectInfo(tpe:TypeInfo, membersByOwner:Iterable[(TypeInfo, Iterable[MemberInfo])]){}
object TypeInspectInfo{
  def nullInspectInfo() = {
    TypeInspectInfo(TypeInfo.nullTypeInfo, Map())
  }
}

case class Note(file:String, msg:String, severity:Int, beg:Int, end:Int, line:Int, col:Int){

  private val tmp = "" + file + msg + severity + beg + end + line + col;
  override val hashCode = tmp.hashCode

  override def equals(other:Any):Boolean = {
    other match{
      case n:Note => n.hashCode == this.hashCode
      case _ => false
    }
  }

  def friendlySeverity = severity match {
    case 2 => 'error
    case 1 => 'warn
    case 0 => 'info
  }

}



class PresentationReporter extends Reporter {

  private val notes = new HashMap[SourceFile, HashSet[Note]] with SynchronizedMap[SourceFile, HashSet[Note]] {
    override def default(k : SourceFile) = { val v = new HashSet[Note] ; put(k, v); v }
  }

  def notesFor(file:SourceFile):List[Note] = {
    notes(file).toList
  }

  def allNotes():List[Note] = {
    notes.flatMap{ e => e._2 }.toList
  }
  
  override def reset{
    super.reset
    notes.clear
  }

  override def info(pos: Position, msg: String, force: Boolean){
    println("INFO: " + msg)
  }
  
  override def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = {
    severity.count += 1
    try {
      if(pos.isDefined) {
	val source = pos.source
	val note = new Note(
	  source.file.absolute.path,
	  formatMessage(msg),
	  severity.id,
	  pos.startOrPoint,
	  pos.endOrPoint,
	  pos.line,
	  pos.column
	)
	notes(source) += note
      }
    } catch {
      case ex : UnsupportedOperationException => 
    }
  }

  def formatMessage(msg:String):String = {
    augmentString(msg).map { 
      case '\n' => ' '
      case '\r' => ' '
      case c => c
    }
  }

}
