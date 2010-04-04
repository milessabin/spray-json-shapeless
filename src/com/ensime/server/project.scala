package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._ 
import scala.actors.Actor._ 
import com.ensime.util._
import com.ensime.util.SExp._
import com.ensime.util.SExpConversion._
import java.io.File

case class ProjectConfig(rootDir:String, srcDirs:Iterable[String], classpath:Iterable[String])
case class SendBackgroundMessageEvent(msg:String)

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
	  case result:CompilationResultEvent =>
	  {
	    sendCompilationResultEvent(result)
	  }
	  case result:TypeCompletionResultEvent =>
	  {
	    sendTypeCompletionReturn(result)
	  }
	  case result:InspectTypeResultEvent =>
	  {
	    sendInspectTypeReturn(result)
	  }
	  case FileModifiedEvent(file:File) =>
	  {
	    compiler ! ReloadFileEvent(file)
	  }
	  case FileCreatedEvent(file:File) =>
	  {
	    compiler ! ReloadFileEvent(file)
	  }
	  case FileRenamedEvent(old:File, file:File) =>
	  {
	    compiler ! RemoveFileEvent(file)
	    compiler ! ReloadFileEvent(file)
	  }
	  case FileDeletedEvent(file:File) =>
	  {
	    compiler ! RemoveFileEvent(file)
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
    val srcDirs = m.get(key(":source-dirs")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }
    val classpath = m.get(key(":classpath")) match{
      case Some(SExpList(items)) => items.map{_.toString}
      case _ => List()
    }
    val conf = ProjectConfig(rootDir, srcDirs, classpath)
    println("Got configuration: " + conf + ". Starting compiler..")
    compiler ! CompilerShutdownEvent
    compiler = new Compiler(this, conf)
    compiler.start
  }


  /*
  * Report compilation results to IDE.
  */
  protected def sendCompilationResultEvent(result:CompilationResultEvent){
    send(SExp(
	key(":compilation-result"),
	SExp(
	  key(":notes"),
	  SExp(result.notes.map{ toSExp(_) })
	)
      ))
  }

  /*
  * Return type completion results to IDE
  */
  protected def sendTypeCompletionReturn(result:TypeCompletionResultEvent){
    sendEmacsRexReturn(
      SExp(
	key(":ok"),
	SExp(
	  key(":members"),
	  SExp(result.members.map{ toSExp(_) })
	)
      ),
      result.callId)
  }


  /*
  * Return type completion results to IDE
  */
  protected def sendInspectTypeReturn(result:InspectTypeResultEvent){
    sendEmacsRexReturn(
      SExp(
	key(":ok"),
	toSExp(result.info)
      ),
      result.callId)
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
      case "swank:compile-file" => {
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
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! ScopeCompletionEvent(new File(file), point)
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
      case "swank:inspect-type" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! InspectTypeEvent(new File(file), point, callId)
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
