package org.ensime.core

import java.io.{ File, InputStream, InputStreamReader }

import org.ensime.config._
import org.ensime.model._
import org.ensime.util._

import scala.collection.mutable.ListBuffer
import scala.collection.{ Iterable, mutable }

case object DebuggerShutdownEvent

abstract class DebugVmStatus

// must have redundant status: String to match legacy API
case class DebugVmSuccess(
  status: String = "success") extends DebugVmStatus
case class DebugVmError(
  errorCode: Int,
  details: String,
  status: String = "error") extends DebugVmStatus
