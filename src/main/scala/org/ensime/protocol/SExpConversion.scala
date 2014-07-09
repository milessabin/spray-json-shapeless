package org.ensime.protocol

import org.ensime.util.SExp

import scala.reflect.internal.util.{ RangePosition, Position }
import scala.tools.nsc.io._

object SExpConversion {

  implicit def posToSExp(pos: Position): SExp = {
    if (pos.isDefined) {
      val underlying = pos.source.file.underlyingSource.orNull
      val archive: SExp = underlying match {
        case a: ZipArchive if a != pos.source.file => a.path
        case _ => 'nil
      }
      SExp.propList(
        (":file", pos.source.path),
        (":archive", archive),
        (":offset", pos.point))
    } else {
      'nil
    }
  }

  implicit def posToSExp(pos: RangePosition): SExp = {
    if (pos.isDefined) {
      val underlying = pos.source.file.underlyingSource.orNull
      val archive: SExp = underlying match {
        case a: ZipArchive if a != pos.source.file => a.path
        case _ => 'nil
      }
      SExp.propList(
        (":file", pos.source.path),
        (":archive", archive),
        (":offset", pos.point),
        (":start", pos.start),
        (":end", pos.end))
    } else {
      'nil
    }
  }
}
