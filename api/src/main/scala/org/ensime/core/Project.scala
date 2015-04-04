package org.ensime.core

import java.io.File

import org.ensime.config._
import org.ensime.model._
import org.ensime.util._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise }
import scala.util.Try

case class RPCError(code: Int, detail: String) extends RuntimeException("" + code + ": " + detail)
case class AsyncEvent(evt: EnsimeEvent)

case object AnalyzerShutdownEvent
case object ReloadExistingFilesEvent
case object AskReTypecheck

case object VoidResponse
