package com.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._ 
import scala.actors.Actor._ 
import com.ensime.server.SExp._
import java.io.File

case class ProjectConfig(rootDir:String, srcDir:String, srcFiles:String, classpath:String)

class Project(config:ProjectConfig) extends Actor with SwankHandler{

  private val compiler:Compiler = new Compiler(this, config)
  //private val fileChanges:FileChangeNotifier = new FileChangeNotifier(this, config)

  def act() {
    println("Project starting with config: " + config)
    compiler.start
    //fileChanges.start
    loop {
      receive {
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
  }


  /*
  * Report compilation results to IDE.
  */
  protected def sendCompilationResultEvent(result:CompilationResultEvent){
    send(SExp(
	":compilation-result",
	SExp(
	  ":notes",
	  SExp(result.notes.map{ _.toEmacsSExp })
	)
      ))
  }

  /*
  * Return type completion results to IDE
  */
  protected def sendTypeCompletionReturn(result:TypeCompletionResultEvent){
    sendEmacsRexReturn(
      SExp(
	":ok",
	SExp(
	  ":members",
	  SExp(result.members.map{ _.toEmacsSExp })
	)
      ),
      result.callId)
  }

  /*
  * A sexp describing the server configuration, per the Swank standard.
  */
  protected def getConnectionInfo = {
    SExp(
      ":pid", 'nil,
      ":server-implementation", 
      SExp(
	":name", SERVER_NAME
      ),
      ":machine", 'nil,
      ":features", 'nil,
      ":version", PROTOCOL_VERSION
    )
  }

  protected override def handleEmacsRex(name:String, form:SExp, callIdSExp:SExp){
    val callId:Int = callIdSExp match{
      case IntAtom(value) => value
      case _ => -1
    }
    name match {
      case "swank:connection-info" => {
	sendEmacsRexReturn(
	  SExp(
	    ":ok",
	    getConnectionInfo
	  ),
	  callId)
      }
      case "swank:compile-file" => {
	form match{
	  case SExpList(head::StringAtom(file)::body) => {
	    compiler ! ReloadFileEvent(new File(file))
	    sendEmacsRexOkReturn(callId)
	  }
	  case _ => {}
	}
      }
      case "swank:scope-completion" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! ScopeCompletionEvent(new File(file), point)
	  }
	  case _ => {}
	}
      }
      case "swank:type-completion" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::StringAtom(prefix)::body) => {
	    compiler ! TypeCompletionEvent(new File(file), point, prefix, callId)
	  }
	  case _ => {}
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
