package org.ensime.core

case class RPCError(code: Int, detail: String) extends RuntimeException("" + code + ": " + detail)
case class AsyncEvent(evt: EnsimeEvent)

case object ReloadExistingFilesEvent
case object AskReTypecheck

case object VoidResponse
