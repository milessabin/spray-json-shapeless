package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._ 
import scala.actors.Actor._ 
import com.ensime.util._
import com.ensime.util.SExp._
import com.ensime.server.model._
import java.io.File

case class ProjectConfig(rootDir:String, srcList:Iterable[String], excludeSrcList:Iterable[String], classpath:Iterable[String])
case class SendBackgroundMessageEvent(msg:String)
case class RPCResultEvent(value:SExpable, callId:Int)

class Project extends Actor with SwankHandler{

  private var compiler:Actor = actor{}

  def act() {
    println("Project waiting for init...")
    loop {
      try{
	receive {
	  case SendBackgroundMessageEvent(msg) =>
	  {
	    sendEmacsBackgroundMessage(msg)
	  }
	  case msg:SwankInMessageEvent =>
	  {
	    handleIncomingSwankMessage(msg)
	  }
	  case result:FullTypeCheckResultEvent =>
	  {
	    send(SExp(
		key(":full-typecheck-result"),
		SExp(
		  key(":notes"),
		  SExp(result.notes.map{ _.toSExp })
		)
	      ))
	  }
	  case result:QuickTypeCheckResultEvent =>
	  {
	    send(SExp(
		key(":quick-typecheck-result"),
		SExp(
		  key(":notes"),
		  SExp(result.notes.map{ _.toSExp })
		)
	      ))
	  }
	  case result:RPCResultEvent =>
	  {
	    sendEmacsRexReturn(
	      SExp(
		key(":ok"),
		result.value
	      ),
	      result.callId)
	  }
	}
      }
      catch{
	case e: Exception => 
	{
	  System.err.println("Error at Project message loop: " + e + " :\n" + e.getStackTraceString)
	}
      }
      
    }
  }

  protected def initProject(config:SExpList){
    val m = config.toKeywordMap
    val rootDir = m.get(key(":root-dir")) match{
      case Some(StringAtom(str)) => str
      case _ => "."
    }
    val srcList = m.get(key(":source")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }
    val excludeSrcList = m.get(key(":exclude-source")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }
    val classpath = m.get(key(":classpath")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }
    val conf = ProjectConfig(rootDir, srcList, excludeSrcList, classpath)
    println("Got configuration: " + conf + ". Starting compiler..")
    compiler ! CompilerShutdownEvent
    compiler = new Compiler(this, conf)
    compiler.start
  }



  protected override def handleEmacsRex(name:String, form:SExp, callId:Int){

    def oops = sendEmacsRexErrorBadArgs(name, form, callId)

    name match {
      case "swank:connection-info" => {
	sendEmacsRexReturn(
	  SExp(key(":ok"), getConnectionInfo),
	  callId)
      }
      case "swank:init-project" => {
	form match{
	  case SExpList(head::(config:SExpList)::body) => {
	    initProject(config)
	    sendEmacsRexOkReturn(callId)
	  }
	  case _ => oops 
	}
      }
      case "swank:typecheck-file" => {
	form match{
	  case SExpList(head::StringAtom(file)::body) => {
	    compiler ! ReloadFileEvent(new File(file))
	    sendEmacsRexOkReturn(callId)
	  }
	  case _ => oops
	}
      }
      case "swank:scope-completion" => {
	form match{

	  case SExpList(head::StringAtom(file)::IntAtom(point)::StringAtom(prefix)::body) => {
	    compiler ! ScopeCompletionEvent(new File(file), point, prefix, callId)
	  }
	  case _ => oops
	}
      }
      case "swank:type-completion" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::StringAtom(prefix)::body) => {
	    compiler ! TypeCompletionEvent(new File(file), point, prefix, callId)
	  }
	  case _ => oops
	}
      }
      case "swank:inspect-type-at-point" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! InspectTypeEvent(new File(file), point, callId)
	  }
	  case _ => oops
	}
      }
      case "swank:inspect-type-by-id" => {
	form match{
	  case SExpList(head::IntAtom(id)::body) => {
	    compiler ! InspectTypeByIdEvent(id, callId)
	  }
	  case _ => oops
	}
      }
      case "swank:type-by-id" => {
	form match{
	  case SExpList(head::IntAtom(id)::body) => {
	    compiler ! TypeByIdEvent(id, callId)
	  }
	  case _ => oops
	}
      }
      case "swank:type-at-point" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! TypeAtPointEvent(new File(file), point, callId)
	  }
	  case _ => oops
	}
      }
      case "swank:inspect-package-by-path" => {
	form match{
	  case SExpList(head::StringAtom(path)::body) => {
	    compiler ! InspectPackageByPathEvent(path, callId)
	  }
	  case _ => oops
	}
      }
      case other => {
	sendEmacsRexError(
	  "Unknown :emacs-rex call: " + other, 
	  callId)
      }
    }
  }


}
