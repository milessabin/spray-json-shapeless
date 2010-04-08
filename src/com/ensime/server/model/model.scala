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

class MemberInfo(override val name:String, val tpe:TypeInfo, val pos:Position) extends EntityInfo(name, List()) with SExpable {
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type"), tpe.toSExp,
      key(":pos"), SExpConversion.toSExp(pos)
    )
  }
}

class MemberInfoLight(override val name:String, tpeName:String, tpeId:Int) extends EntityInfo(name, List()){
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type-name"), tpeName,
      key(":type-id"), tpeId
    )
  }
}


class ClassInfo(override val name:String, tpe:TypeInfo, pos:Position, override val members:Iterable[EntityInfo]) extends EntityInfo(name, members){
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type"), tpe.toSExp,
      key(":members"), SExp(members.map{_.toSExp}),
      key(":pos"), SExpConversion.toSExp(pos)
    )
  }
}

class TraitInfo(override val name:String, tpe:TypeInfo, pos:Position, override val members:Iterable[EntityInfo]) extends EntityInfo(name, members){
  def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":type"), tpe.toSExp,
      key(":members"), SExp(members.map{_.toSExp}),
      key(":pos"), SExpConversion.toSExp(pos)
    )
  }
}



class TypeInfo(
  val name:String, 
  val id:Int, 
  val declaredAs:scala.Symbol,
  val fullName:String, 
  val pos:Position) extends SExpable{

  implicit def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":id"), id,
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
  val paramTypes:Iterable[TypeInfo]) extends TypeInfo(name, id, 'nil, name, NoPosition){

  override implicit def toSExp():SExp = {
    SExp(
      key(":name"), name,
      key(":id"), id,
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
  def nullTypeInfo() = {
    new TypeInfo("NA", -1, 'class, "NA", NoPosition)
  }
}


class TypeInspectInfo(tpe:TypeInfo, membersByOwner:Iterable[(TypeInfo, Iterable[EntityInfo])]) extends SExpable{

  def toSExp():SExp = {
    SExp(
      key(":type"), tpe.toSExp,
      key(":members-by-owner"), SExp(membersByOwner.map{
	  case (ownerTpe, members) => {
	    SExp(ownerTpe.toSExp, SExp(members.map{_.toSExp}))
	  }})
    )
  }
}
object TypeInspectInfo{
  def nullInspectInfo() = {
    new TypeInspectInfo(TypeInfo.nullTypeInfo, Map())
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

