package org.ensime.server

import scala.tools.nsc.interactive.{Global, CompilerControl}
import scala.tools.nsc.{Settings}
import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import scala.actors._ 
import scala.actors.Actor._ 
import org.ensime.util._
import org.ensime.util.SExp._
import org.ensime.server.model._
import org.ensime.config.ProjectConfig
import org.ensime.debug.ProjectDebugInfo
import java.io.File



case class SendBackgroundMessageEvent(msg:String)
case class RPCResultEvent(value:SExpable, callId:Int)
case class RPCErrorEvent(value:String, callId:Int)


class Project extends Actor with SwankHandler{

  private var compiler:Actor = actor{}
  private var builder:Option[Actor] = None
  private var config:ProjectConfig = ProjectConfig.nullConfig
  private var debugInfo:Option[ProjectDebugInfo] = None

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
	  case result:CompilerReadyEvent =>
	  {
	    send(SExp(key(":compiler-ready"),true))
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
	  case RPCResultEvent(value, callId) =>
	  {
	    sendEmacsRexReturn(
	      SExp(
		key(":ok"),
		value
	      ),
	      callId)
	  }
	  case RPCErrorEvent(msg, callId) =>
	  {
	    System.err.println(msg);
	    sendEmacsRexReturn(
	      SExp(
		key(":abort"),
		msg
	      ),
	      callId)
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
    compiler ! CompilerShutdownEvent
    compiler = new Compiler(this, this.config)
    compiler.start
  }

  protected def getOrStartBuilder():Actor = {
    builder match{
      case Some(b) => b
      case None => 
      {
	val b = new IncrementalBuilder(this, this.config)
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
	    val conf = ProjectConfig(config)
	    initProject(conf)
	    sendEmacsRexOkReturn(callId)
	  }
	  case _ => oops 
	}
      }
      case "swank:repl-config" => {
	sendEmacsRexReturn(
	  SExp.propList(
	    (":ok", SExp.propList(
		(":classpath", strToSExp(this.config.replClasspath))
	      ))),
	  callId)
      }
      case "swank:builder-init" => {
	val b = getOrStartBuilder
	b ! RPCRequestEvent(RebuildAllReq(), callId)	
      }
      case "swank:builder-add-files" => {
	form match{
	  case SExpList(head::SExpList(filenames)::body) => {
	    val files = filenames.map(s => new File(s.toString))
	    val b = getOrStartBuilder
	    b ! RPCRequestEvent(AddSourceFilesReq(files), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:builder-update-files" => {
	form match{
	  case SExpList(head::SExpList(filenames)::body) => {
	    val files = filenames.map(s => new File(s.toString))
	    val b = getOrStartBuilder
	    b ! RPCRequestEvent(UpdateSourceFilesReq(files), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:builder-remove-files" => {
	form match{
	  case SExpList(head::SExpList(filenames)::body) => {
	    val files = filenames.map(s => new File(s.toString))
	    val b = getOrStartBuilder
	    b ! RPCRequestEvent(RemoveSourceFilesReq(files), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:debug-config" => {
	debugInfo = Some(new ProjectDebugInfo(config))
	sendEmacsRexReturn(
	  SExp.propList(
	    (":ok", SExp.propList(
		(":classpath", strToSExp(this.config.debugClasspath)),
		(":sourcepath", strToSExp(this.config.debugSourcepath)),
		(":debug-class", strToSExp(this.config.debugClass.getOrElse(""))),
		(":debug-args", strToSExp(this.config.debugArgString))
	      ))),
	  callId)
      }
      case "swank:debug-unit-info" => {
	form match{
	  case SExpList(head::StringAtom(sourceName)::IntAtom(line)::StringAtom(packPrefix)::body) => {
	    val info = debugInfo.getOrElse(new ProjectDebugInfo(config))
	    debugInfo = Some(info)
	    val unit = info.findUnit(sourceName, line, packPrefix)
	    unit match{
	      case Some(unit) => {
		sendEmacsRexReturn(
		  SExp.propList(
		    (":ok", SExp.propList(
			(":full-name", strToSExp(unit.classQualName)),
			(":package", strToSExp(unit.packageName)),
			(":start-line", intToSExp(unit.startLine)),
			(":end-line", intToSExp(unit.endLine))
		      ))),
		  callId)
	      }
	      case None => {
		sendEmacsRexReturn(SExp(key(":ok"), NilAtom()), callId)
	      }
	    }

	  }
	  case _ => oops
	}
      }
      case "swank:debug-class-locs-to-source-locs" => {
	form match{
	  case SExpList(head::SExpList(pairs)::body) => {
	    val info = debugInfo.getOrElse(new ProjectDebugInfo(config))
	    debugInfo = Some(info)
	    val result = SExpList(pairs.flatMap{ p =>
		p match {
		  case SExpList((classname:StringAtom)::(line:IntAtom)::body) => {
		    info.findSourceForClass(classname.toString) match{
		      case Some(s) => Some(SExp(s, line))
		      case None => Some(SExp('nil, line))
		    }
		  }
		  case _ => Some(SExp('nil, 'nil))
		}
	      })
	    sendEmacsRexReturn(
	      SExpList(List(key(":ok"), result)),
	      callId)
	  }
	  case _ => oops
	}
      }
      case "swank:typecheck-file" => {
	form match{
	  case SExpList(head::StringAtom(file)::body) => {
	    compiler ! RPCRequestEvent(ReloadFileReq(new File(file)), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:typecheck-all" => {
	compiler ! RPCRequestEvent(ReloadAllReq(), callId)
      }
      case "swank:scope-completion" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::StringAtom(prefix)::BooleanAtom(constructor)::body) => {
	    compiler ! RPCRequestEvent(ScopeCompletionReq(new File(file), point, prefix, constructor), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:type-completion" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::StringAtom(prefix)::body) => {
	    compiler ! RPCRequestEvent(TypeCompletionReq(new File(file), point, prefix), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:inspect-type-at-point" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! RPCRequestEvent(InspectTypeReq(new File(file), point), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:inspect-type-by-id" => {
	form match{
	  case SExpList(head::IntAtom(id)::body) => {
	    compiler ! RPCRequestEvent(InspectTypeByIdReq(id), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:symbol-at-point" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! RPCRequestEvent(SymbolAtPointReq(new File(file), point), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:type-by-id" => {
	form match{
	  case SExpList(head::IntAtom(id)::body) => {
	    compiler ! RPCRequestEvent(TypeByIdReq(id), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:call-completion" => {
	form match{
	  case SExpList(head::IntAtom(id)::body) => {
	    compiler ! RPCRequestEvent(CallCompletionReq(id), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:type-at-point" => {
	form match{
	  case SExpList(head::StringAtom(file)::IntAtom(point)::body) => {
	    compiler ! RPCRequestEvent(TypeAtPointReq(new File(file), point), callId)
	  }
	  case _ => oops
	}
      }
      case "swank:inspect-package-by-path" => {
	form match{
	  case SExpList(head::StringAtom(path)::body) => {
	    compiler ! RPCRequestEvent(InspectPackageByPathReq(path), callId)
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
