package com.ensime.server.model

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.symtab.Types
import scala.tools.nsc.symtab.Symbols
import com.ensime.util.SExp._
import com.ensime.util.{SExp, SExpable}
import scala.collection.mutable.{ HashMap, HashEntry, HashSet }


object SExpConversion{

  implicit def toSExp(pos:Position):SExp = {
    if(pos.isDefined){
      SExp(
	key(":file"), pos.source.path,
	key(":offset"), pos.point
      )
    }
    else{
      'nil
    }
  }

}

abstract class EntityInfo(val name:String, val members:Iterable[EntityInfo]) extends SExpable{}

class PackageInfo(override val name:String, val fullname:String, override val members:Iterable[EntityInfo]) extends EntityInfo(name, members){
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":info-type"), 'package,
      key(":full-name"), fullname,
      key(":members"), SExp(members.map{_.toSExp})
    )
  }
}

abstract trait LooksLikeType{
  def name:String
  def id:Int 
  def declaredAs:scala.Symbol
  def fullName:String 
  def pos:Position
}

class NamedTypeInfo(
  override val name:String, 
  val pos:Position, 
  override val members:Iterable[EntityInfo],
  val declaredAs:scala.Symbol,
  val id:Int,
  val fullName:String) extends EntityInfo(name, members) with LooksLikeType{

  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":full-name"), fullName,
      key(":members"), SExp(members.map{_.toSExp}),
      key(":pos"), SExpConversion.toSExp(pos),
      key(":declared-as"), declaredAs,
      key(":type-id"), id
    )
  }

}


class NamedTypeMemberInfo(override val name:String, val tpe:TypeInfo, val pos:Position) extends EntityInfo(name, List()) with SExpable {
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type"), tpe.toSExp,
      key(":pos"), SExpConversion.toSExp(pos)
    )
  }
}


class NamedTypeMemberInfoLight(override val name:String, tpeName:String, tpeId:Int) extends EntityInfo(name, List()){
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type-name"), tpeName,
      key(":type-id"), tpeId
    )
  }
}


class ScopeNameInfoLight(override val name:String, tpeName:String, tpeId:Int) extends EntityInfo(name, List()){
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type-name"), tpeName,
      key(":type-id"), tpeId
    )
  }
}


class TypeInfo(
  val name:String, 
  val id:Int, 
  val declaredAs:scala.Symbol,
  val fullName:String, 
  val pos:Position) extends SExpable with LooksLikeType{

  implicit def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type-id"), id,
      key(":full-name"), fullName,
      key(":declared-as"), declaredAs,
      key(":pos"), SExpConversion.toSExp(pos)
    )
  }
}

class ArrowTypeInfo(
  override val name:String, 
  override val id:Int, 
  val resultType:TypeInfo, 
  val paramTypes:Iterable[TypeInfo]) extends TypeInfo(name, id, 'nil, name, NoPosition){

  override implicit def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type-id"), id,
      key(":arrow-type"), true,
      key(":result-type"), resultType.toSExp,
      key(":param-types"), SExp(paramTypes.map(_.toSExp))
    )
  }
}

class TypeInspectInfo(tpe:TypeInfo, supers:Iterable[NamedTypeInfo]) extends SExpable{
  def toSExp():SExp = {
    SExp(
      key(":type"), tpe.toSExp,
      key(":supers"), SExp(supers.map{_.toSExp})
    )
  }
}


trait ModelBuilders {  self: Global => 

  import self._
  import definitions.{ ObjectClass, ScalaObjectClass, RootPackage, EmptyPackage, NothingClass, AnyClass, AnyRefClass }

  private val typeCache = new HashMap[Int, Type]
  private val typeCacheReverse = new HashMap[Type, Int]
  
  def clearTypeCache(){
    typeCache.clear
    typeCacheReverse.clear
  }
  def typeById(id:Int):Option[Type] = { 
    typeCache.get(id)
  }
  def cacheType(tpe:Type):Int = {
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


  object Helpers{

    def normalizeSym(aSym: Symbol): Symbol = aSym match {
      case null | EmptyPackage | NoSymbol                   => normalizeSym(RootPackage)
      case ScalaObjectClass | ObjectClass                   => normalizeSym(AnyRefClass)
      case _ if aSym.isModuleClass || aSym.isPackageObject  => normalizeSym(aSym.sourceModule)
      case _                                                => aSym
    }

  }


  object PackageInfo{

    import Helpers._

    def root: PackageInfo = fromSymbol(RootPackage)

    def fromPath(path:String): PackageInfo = {
      val pack = packageSymFromPath(path)
      pack match{
	case Some(packSym) => fromSymbol(packSym)
	case None => nullInfo
      }
    }

    private def packageSymFromPath(path:String):Option[Symbol] = {
      val pathSegs = path.split("\\.")
      val pack = pathSegs.foldLeft(RootPackage){ (packSym,seg) =>
	val member = packSym.info.members find { s =>
	  s.nameString == seg && s != EmptyPackage && s != RootPackage
	}
	member.getOrElse(packSym)
      }
      if(pack == RootPackage) None
      else Some(pack)
    }

    def nullInfo = {
      new PackageInfo("NA", "NA", List())
    }
    
    def fromSymbol(aSym: Symbol): PackageInfo = {
      val bSym = normalizeSym(aSym)
      val bSymFullName = bSym.fullName
      def makeMembers(symbols:Iterable[Symbol]) = {
	val validSyms = symbols.filter { s => 
	  s != EmptyPackage && s != RootPackage && s.owner.fullName == bSymFullName
	}
	val members = validSyms.flatMap(packageMemberFromSym).toList
	members.sortWith{(a,b) => a.name <= b.name}
      }

      val pack = if (bSym == RootPackage) {
	val memberSyms = (bSym.info.members ++ EmptyPackage.info.members)
	new PackageInfo(
	  "root",
	  "_root_",
	  makeMembers(memberSyms)
	)
      }
      else{
	val memberSyms = bSym.info.members
	new PackageInfo(
	  bSym.name.toString,
	  bSym.fullName,
	  makeMembers(memberSyms)
	)
      }
      pack
    }

    def packageMemberFromSym(aSym:Symbol): Option[EntityInfo] ={
      val bSym = normalizeSym(aSym)
      if (bSym == RootPackage){
	Some(root)
      }
      else if (bSym.isPackage){	
	Some(fromSymbol(bSym))
      }
      else if(!(bSym.nameString.contains("$"))){
	NamedTypeInfo.fromSymNoMembers(bSym)
      }
      else{
	None
      }
    }

  }



  object NamedTypeInfo {

    def nullInfo(name:String = "NA") = {
      new NamedTypeInfo(name, NoPosition, List(), 'nil, -1, name)
    }

    def apply(tpe:TypeInfo, members:Iterable[EntityInfo]) = {
      new NamedTypeInfo(tpe.name, tpe.pos, members, tpe.declaredAs, tpe.id, tpe.fullName)
    }

    def fromSymNoMembers(sym:Symbol):Option[NamedTypeInfo] = {
      if(sym.isClass || sym.isTrait){
	Some(new NamedTypeInfo(sym.nameString, sym.pos, List(), TypeInfo.declaredAs(sym), cacheType(sym.tpe), sym.fullName))
      }
      else{
	None
      }
    }
  }


  object TypeInfo{


    def declaredAs(sym:Symbol):scala.Symbol = {
      if(sym.isTrait)
      'trait
      else if(sym.isInterface)
      'interface
      else if(sym.isClass)
      'class
      else if(sym.isAbstractClass)
      'abstractclass
      else 'nil
    }

    def apply(tpe:Type):TypeInfo = {
      tpe match{
	case tpe:MethodType => 
	{
	  ArrowTypeInfo(tpe)
	}
	case tpe:PolyType => 
	{
	  ArrowTypeInfo(tpe)
	}
	case tpe:Type =>
	{
	  val typeSym = tpe.typeSymbol
	  val underlying = tpe.underlying
	  new TypeInfo(underlying.toString,
	    cacheType(tpe), declaredAs(typeSym), typeSym.fullName, typeSym.pos)
	}
	case _ => nullInfo
      }
    }
    def nullInfo() = {
      new TypeInfo("NA", -1, 'nil, "NA", NoPosition)
    }
  }


  object ArrowTypeInfo{

    def apply(tpe:MethodType):ArrowTypeInfo = {
      new ArrowTypeInfo(
	tpe.toString, 
	cacheType(tpe), 
	TypeInfo(tpe.resultType), 
	tpe.paramTypes.map(t => TypeInfo(t)))
    }
    def apply(tpe:PolyType):ArrowTypeInfo = {
      new ArrowTypeInfo(
	tpe.toString, 
	cacheType(tpe), 
	TypeInfo(tpe.resultType), 
	tpe.paramTypes.map(t => TypeInfo(t)))
    }
    def nullInfo() = {
      new TypeInfo("NA", -1, 'class, "NA", NoPosition)
    }
  }

  object TypeInspectInfo{
    def nullInfo() = {
      new TypeInspectInfo(TypeInfo.nullInfo(), List())
    }
  }


}
