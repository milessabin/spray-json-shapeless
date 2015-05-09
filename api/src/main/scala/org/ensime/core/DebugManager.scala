package org.ensime.core

case object DebuggerShutdownEvent

sealed abstract class DebugVmStatus

// must have redundant status: String to match legacy API
case class DebugVmSuccess(
  status: String = "success"
) extends DebugVmStatus
case class DebugVmError(
  errorCode: Int,
  details: String,
  status: String = "error"
) extends DebugVmStatus
