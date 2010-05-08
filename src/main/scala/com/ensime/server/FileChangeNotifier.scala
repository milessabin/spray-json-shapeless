package com.ensime.server

import java.io.File
import net.contentobjects.jnotify._
import scala.actors._  
import scala.collection.mutable.ArrayBuffer

abstract class FileChangedEvent
case class FileRenamedEvent(old:File, file:File) extends FileChangedEvent
case class FileModifiedEvent(file:File) extends FileChangedEvent
case class FileDeletedEvent(file:File) extends FileChangedEvent
case class FileCreatedEvent(file:File) extends FileChangedEvent

case class ShutdownEvent()

class FileChangeNotifier(project:Actor, config:ProjectConfig) extends Actor{

  private var watchIDs:ArrayBuffer[Int] = ArrayBuffer()

  def act(){
    init()
    loop {
      receive {
	case msg:ShutdownEvent =>
	{
	  for(watchID <- watchIDs){
	    val res:Boolean = JNotify.removeWatch(watchID)
	    if (!res){
	      System.err.println("Invalid watch ID specified")
	    }
	  }
	}
      }
    }
  }

  private def interesting(file:File):Boolean = {
    file.exists() && !(file.isHidden()) && file.getAbsolutePath().matches(".+scala")
  }

  private def init(){
    val root = new File(config.rootDir)
    for(s <- config.srcList){
      val src = new File(config.rootDir, s)
      addWatch(src)
    }
  }

  private def addWatch(dir:File){
    if(!dir.isDirectory() || !dir.exists()){
      System.err.println("" + dir + "is not a valid source directory!")
      return
    }

    val mask:Int = (
      JNotify.FILE_CREATED | 
      JNotify.FILE_DELETED | 
      JNotify.FILE_MODIFIED| 
      JNotify.FILE_RENAMED );


    val watchSubtree:Boolean = true;

    watchIDs += JNotify.addWatch(dir.getAbsolutePath(), mask, watchSubtree, 
      new JNotifyListener(){
	def fileRenamed(wd:Int, rootPath:String, oldName:String, newName:String){
	  val dir = new File(rootPath)
	  val old = new File(dir, oldName)
	  val newF = new File(dir, newName)
	  if(interesting(newF)){
	    System.out.println("JNotifyTest.fileRenamed() : wd #" + wd + " root = " + rootPath + ", " + oldName + " -> " + newName);
	    project ! FileRenamedEvent(old, newF)
	  }
	}

	def fileModified(wd:Int, rootPath:String, name:String){
	  val dir = new File(rootPath);
	  val f = new File(dir, name)
	  if(interesting(f)){
	    System.out.println("JNotifyTest.fileModified() : wd #" + wd + " root = " + rootPath + ", " + name);
	    project ! FileModifiedEvent(f)
	  }
	}

	def fileDeleted(wd:Int, rootPath:String, name:String){
	  val dir = new File(rootPath);
	  val f = new File(dir, name)
	  if(interesting(f)){
	    System.out.println("JNotifyTest.fileDeleted() : wd #" + wd + " root = " + rootPath
	      + ", " + name);
	    project ! FileDeletedEvent(f)
	  }
	}

	def fileCreated(wd:Int, rootPath:String, name:String){
	  val dir = new File(rootPath);
	  val f = new File(dir, name)
	  if(interesting(f)){
	    System.out.println("JNotifyTest.fileCreated() : wd #" + wd + " root = " + rootPath
	      + ", " + name);
	    project ! FileCreatedEvent(f)
	  }
	}
      });
  }



}
