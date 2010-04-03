package com.ensime.util

import com.ensime.server._
import com.ensime.util.SExp._
import scala.tools.nsc.util.Position

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

  implicit def toSExp(m:MemberInfo):SExp = {
    SExp(
      key(":name"), m.name,
      key(":type"), m.tpe,
      key(":pos"), m.pos
    )
  }

  implicit def toSExp(m:MemberInfoLight):SExp = {
    SExp(
      key(":name"), m.name,
      key(":type-name"), m.tpeName
    )
  }

  implicit def toSExp(t:TypeInspectInfo):SExp = {
    SExp(
      key(":type"), t.tpe,
      key(":members"), SExp(t.members.map{toSExp(_)})
    )
  }

  implicit def toSExp(t:TypeInfo):SExp = {
    SExp(
      key(":name"), t.name,
      key(":general-name"), t.generalName,
      key(":pos"), t.pos
    )
  }

  implicit def toSExp(n:Note) = {
    SExp(
      key(":severity"), n.friendlySeverity,
      key(":msg"), n.msg,
      key(":beg"), n.beg,
      key(":end"), n.end,
      key(":line"), n.line,
      key(":col"), n.col,
      key(":file"), n.file
    )
  }


}
