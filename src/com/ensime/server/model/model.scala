package com.ensime.server.model

import scala.tools.nsc.util.{SourceFile, Position, OffsetPosition}
import scala.tools.nsc.util.NoPosition
import scala.tools.nsc.symtab.Types
import com.ensime.util.SExp._
import com.ensime.util.{SExp, SExpable}


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

  implicit def toSExp(o:SExpable):SExp = {
    o.toSExp
  }

}


abstract class EntityInfo(val name:String, val members:Iterable[EntityInfo]) extends SExpable{}

class PackageInfo(override val name:String, override val members:Iterable[EntityInfo]) extends EntityInfo(name, members){
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
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
  val declaredAs:Symbol,
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
object NamedTypeInfo{
  def nullInfo() = {
    new NamedTypeInfo("NA", NoPosition, List(), 'nil, -1, "NA")
  }
  def apply(tpe:TypeInfo, members:Iterable[EntityInfo]) = {
    new NamedTypeInfo(tpe.name, tpe.pos, members, tpe.declaredAs, tpe.id, tpe.fullName)
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
      case _ => nullInfo
    }
  }
  def nullInfo() = {
    new TypeInfo("NA", -1, 'nil, "NA", NoPosition)
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
  def nullInfo() = {
    new TypeInfo("NA", -1, 'class, "NA", NoPosition)
  }
}

class TypeInspectInfo(tpe:NamedTypeInfo, supers:Iterable[NamedTypeInfo]) extends SExpable{
  def toSExp():SExp = {
    SExp(
      key(":named-type"), tpe.toSExp,
      key(":supers"), SExp(supers.map{_.toSExp})
    )
  }
}
object TypeInspectInfo{
  def nullInfo() = {
    new TypeInspectInfo(NamedTypeInfo.nullInfo, List())
  }
}

class Note(file:String, msg:String, severity:Int, beg:Int, end:Int, line:Int, col:Int) extends SExpable{

  private val tmp = "" + file + msg + severity + beg + end + line + col;
  override val hashCode = tmp.hashCode

  def toSExp() = {
    SExp(
      key(":severity"), friendlySeverity,
      key(":msg"), msg,
      key(":beg"), beg,
      key(":end"), end,
      key(":line"), line,
      key(":col"), col,
      key(":file"), file
    )
  }

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

