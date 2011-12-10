/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*  
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*  
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.config
import expectj.{ExpectJ, Spawn, Executor}
import java.io.File
import java.util.regex.Pattern
import scala.util.matching._
import org.ensime.util._
import scala.collection.mutable.ArrayBuffer
import FileUtils._


case class SbtSubproject(name: String, deps: List[String])

object Sbt extends ExternalConfigurator {

  val OutputFile = ".ensime-sbt-proj.json"

  private trait SbtInstance{
    def versionName:String
    def jarName:String
    def pathToSbtJar = (new File(".", "bin/" + jarName)).getCanonicalPath()
    def appArgs:Seq[String] = List()

    // TODO: 
    // We want JVM args passed via the environment to supercede default args,
    // but the handling of duplicate arguments is JVM implementation specific.
    // It seems they generally prefer the later arguments. We're just gonna go
    // with that assumption for now.
    // 
    // To do this right we need to parse the argument strings.
    // 
    def jvmArgs:Seq[String] = List("-Xmx512M") ++ (
      Option(System getenv "SBT_OPTS") map { _ split "\\s+" } map { arr => Vector(arr: _*) } getOrElse Vector())

    def run(baseDir: File): Either[Throwable,String] = {

      val reqArgs = Vector("-Dsbt.log.noformat=true","-jar", pathToSbtJar)
      val args = (Vector("java") ++ jvmArgs ++ reqArgs ++ appArgs) filter { _.trim().length > 0 }
      println("Starting sbt with command line: " + args.mkString(" "))
      import scala.collection.JavaConversions._
      val pb = new ProcessBuilder(args)
      pb.directory(baseDir)
      ProcessUtil.readAllOutputToConsole(pb.start()) match{
	case Right(status) => {
	  if(status == 0){
	    readFile(new File(baseDir, OutputFile))
	  }
	  else{
	    Left(new RuntimeException("Non-zero exit value for process: " + status))
	  }
	}
	case Left(t:Throwable) => Left(t)
      }
    }
  }

  private class Sbt10Style(val subProj:Option[String]) extends SbtInstance{
    def versionName:String = "0.10"
    def jarName:String = "sbt-launch-0.10.1.jar"
    override def appArgs:List[String] = List(
      List("ensime","dump", subProj.getOrElse("root"), OutputFile).mkString(" "))
  }

  def getConfig(sbt: SbtInstance, baseDir: File): Either[Throwable, ExternalConfig] = {
    sbt.run(baseDir) match{
      case Left(e) => Left(e)
      case Right(json) => {
	try{
	  import net.liftweb.json._
	  import net.liftweb.json.JsonAST._
	  val obj = parse(json)
	  implicit val formats = DefaultFormats
	  val name = (obj \ "name").extract[String]
	  val org = (obj \ "org").extract[String]
	  val projectVersion = (obj \ "projectVersion").extract[String]
	  val buildScalaVersion = (obj \ "buildScalaVersion").extract[String]
	  val compileDeps = (obj \ "compileDeps").extract[List[String]]
	  val testDeps = (obj \ "testDeps").extract[List[String]]
	  val runtimeDeps = (obj \ "runtimeDeps").extract[List[String]]
	  val sourceRoots = (obj \ "sourceRoots").extract[List[String]]
	  val target = (obj \ "target").extract[String]

	  val testDepFiles = maybeFiles(testDeps, baseDir)
	  val compileDepFiles = maybeFiles(compileDeps, baseDir) ++ testDepFiles
	  val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir) ++ testDepFiles
	  val sourceRootFiles = maybeDirs(sourceRoots, baseDir)
	  val targetFile = CanonFile(target)

	  Right(ExternalConfig(Some(name), sourceRootFiles,
	      runtimeDepFiles, compileDepFiles, testDepFiles,
	      Some(targetFile)))
	}
	catch{
	  case e => Left(e)
	}
      }
    }
  }

  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    val subProj = conf.sbtActiveSubproject.map(_.name)
    conf.sbtJar.flatMap(maybeFile(_, baseDir)) match{
      case Some(jarFile) => {
	val sbt = (new Sbt10Style(subProj){
	    override def versionName:String = "Custom"
	    override def pathToSbtJar:String = jarFile.getAbsolutePath
	  })
	getConfig(sbt, baseDir)
      }
      case None => {
	val props = JavaProperties.load(new File(baseDir, "project/build.properties"))
	val sbt11 = (new Sbt10Style(subProj){
	    override def versionName:String = "0.11"
	    override def jarName:String = "sbt-launch-0.11.2.jar"
	  })
	val sbt10 = (new Sbt10Style(subProj){})
	val sbt9 = (new Sbt10Style(subProj){
	    override def versionName:String = "0.9"
	  })
	props.get("sbt.version") match {
	  case Some(v:String) => {
	    if(v.startsWith("0.7")) {
	      println("Sbt 7 is no longer supported. Returning null config.")
	      Right(ExternalConfig(None,List(),List(),List(),List(),None))
	    }
	    else if(v.startsWith("0.9")) getConfig(sbt9, baseDir)

	    else if(v.startsWith("0.10")) getConfig(sbt10, baseDir)

	    else if(v.startsWith("0.11")) getConfig(sbt11, baseDir)

	    else {
	      println("Unrecognized sbt version " + v + ". Guessing sbt-11...")
	      getConfig(sbt11, baseDir)
	    }
	  }
	  case None => {
	    println("No sbt version specified. Guessing sbt-11...")
	    getConfig(sbt11, baseDir)
	  }
	}
      }
    }


  }

}
