package com.ensime.server


import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._  
import scala.actors.Actor._  

class Project extends Actor with SwankHandler{

  private val compiler:Compiler = new Compiler(this)

  def act() {
    println("Project starting..")
    compiler.start
    loop {
      receive {
	case msg:SwankInMessage =>
	{
	  handleIncomingSwankMessage(msg)
	}
	case result:CompilationResult =>
	{
	  sendCompilationResultEvent(result)
	}
      }
    }
  }

  def compileFile(file:String){
    compiler ! ReloadFile(file)
  }

  protected def sendCompilationResultEvent(result:CompilationResult){
    send(SExpList(
	List(
	  KeywordAtom(":compilation-result"),
	  SExpList(List(
	      KeywordAtom(":notes"),
	      SExpList(result.notes.map{ _.toEmacsSExp })
	    )
	  )
	)))
  }


  protected def getConnectionInfo = {
    SExpList(List(
	KeywordAtom(":pid"), NilAtom(),
	KeywordAtom(":server-implementation"), SExpList(List(
	    KeywordAtom(":name"), StringAtom(SERVER_NAME)
	  )),
	KeywordAtom(":machine"), NilAtom(),
	KeywordAtom(":features"), NilAtom(),
	KeywordAtom(":version"), StringAtom(PROTOCOL_VERSION)
      ))
  }

  protected override def handleEmacsRex(name:String, form:SExp, callId:SExp){
    name match {
      case "swank:connection-info" => {
	sendEmacsRexReturn(
	  SExpList(List(	
	      KeywordAtom(":ok"),
	      getConnectionInfo
	    )),
	  callId)
      }
      case "swank:compile-file" => {
	println("project got compile file request: " + form)
	form match{
	  case SExpList(head::StringAtom(file)::body) => {
	    compileFile(file)
	    println("sent to compiler: " + file)
	    sendEmacsRexOkReturn(callId)
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
