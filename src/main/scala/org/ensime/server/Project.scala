package org.ensime.server

import java.io.File
import org.ensime.config.ProjectConfig
import org.ensime.debug.ProjectDebugInfo
import org.ensime.protocol._
import org.ensime.util._
import scala.actors._
import scala.actors.Actor._
import scala.tools.nsc.{Settings}


case class SendBackgroundMessageEvent(msg:String)
case class RPCResultEvent(value:WireFormat, callId:Int)
case class RPCErrorEvent(value:String, callId:Int)

class Project(val protocol:Protocol) extends Actor with RPCTarget{

  protocol.setRPCTarget(this)

  protected var compiler:Actor = actor{}
  protected var builder:Option[Actor] = None
  protected var config:ProjectConfig = ProjectConfig.nullConfig
  protected var debugInfo:Option[ProjectDebugInfo] = None


  def act() {
    println("Project waiting for init...")
    loop {
      try{
	receive {
	  case SendBackgroundMessageEvent(msg:String) =>
	  {
	    protocol.sendBackgroundMessage(msg)
	  }
	  case IncomingMessageEvent(msg:WireFormat) =>
	  {
	    protocol.handleIncomingMessage(msg)
	  }
	  case msg:AnalyzerReadyEvent =>
	  {
	    protocol.sendCompilerReady
	  }
	  case result:FullTypeCheckResultEvent =>
	  {
	    protocol.sendFullTypeCheckResult(result)
	  }
	  case result:QuickTypeCheckResultEvent =>
	  {
	    protocol.sendQuickTypeCheckResult(result)
	  }
	  case RPCResultEvent(value, callId) =>
	  {
	    protocol.sendRPCReturn(value, callId)
	  }
	  case RPCErrorEvent(msg, callId) =>
	  {
	    protocol.sendRPCReturnError(msg, callId)
	  }
	}
      }
      catch{
	case e: Exception => 
	{
	  println("Error at Project message loop: " + e + " :\n" + e.getStackTraceString)
	}
      }
    }
  }

  protected def initProject(conf:ProjectConfig){
    this.config = conf;
    restartCompiler
    shutdownBuilder
  }

  protected def restartCompiler() {
    compiler ! AnalyzerShutdownEvent
    compiler = new Analyzer(this, protocol, this.config)
    compiler.start
  }

  protected def getOrStartBuilder():Actor = {
    builder match{
      case Some(b) => b
      case None => 
      {
	val b = new IncrementalBuilder(this, protocol, this.config)
	builder = Some(b)
	b.start
	b
      }
    }
  }

  protected def shutdownBuilder() {
    for(b <- builder){
      b ! BuilderShutdownEvent
    }
    builder = None
  }

}


