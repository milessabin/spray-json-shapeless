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

import sbt._
import Keys._
import sbt.Path
import IO._
import java.io.FileInputStream

object EnsimeBuild extends Build {

  private def doSh(str:String, cwd:Option[File]) = Process("sh" :: "-c" :: str :: Nil, cwd)
  private def doSh(str:String) = Process("sh" :: "-c" :: str :: Nil, None)

  private def file(str:String) = new File(str)

  val root = Path(".")

  val TwoEightVersion = "2.8.3-SNAPSHOT"
  val TwoNineVersion = "2.9.2-SNAPSHOT"

  lazy val project = {
    Project(
      id = "ensime",
      base = file ("."),
      settings = Project.defaultSettings ++ 
      Seq(
	version := "0.9.3.RC3",
	organization := "org.ensime",
	scalaVersion := TwoNineVersion,
	crossScalaVersions := Seq(TwoEightVersion, TwoNineVersion),
	resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
	resolvers +=  "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2",
	libraryDependencies <++= (scalaVersion) { scalaVersion =>
	  val compilerVersion = scalaVersion
	  val scalatest = scalaVersion match {
	    case v if v == TwoEightVersion => 
	    "org.scalatest" % "scalatest_2.8.2" % "1.5.1" % "test"
	    case v if v == TwoNineVersion => 
	    "org.scalatest" % "scalatest_2.9.1" % "1.6.1" % "test"
	  }
	  val scalariform = scalaVersion match {
	    case v if v == TwoEightVersion => 
	    "org.scalariform" % "scalariform_2.8.3-SNAPSHOT" % "0.1.1" % "compile;runtime;test"
	    case v if v == TwoNineVersion => 
	    "org.scalariform" % "scalariform_2.9.1" % "0.1.1" % "compile;runtime;test"
	  }
	  val scalaRefactoring = scalaVersion match {
	    case v if v == TwoEightVersion => 
	    "org.scala-refactoring" % "org.scala-refactoring_2.8.2-SNAPSHOT" % "0.3.0-SNAPSHOT" from "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.8.3-SNAPSHOT/0.3.0-SNAPSHOT/org.scala-refactoring_2.8.3-SNAPSHOT-0.3.0-20111227.074430-50.jar"
	    case v if v == TwoNineVersion => 
	    "org.scala-refactoring" % "org.scala-refactoring_2.9.2-SNAPSHOT" % "0.3.0-SNAPSHOT" from "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.9.2-SNAPSHOT/0.3.0-SNAPSHOT/org.scala-refactoring_2.9.2-SNAPSHOT-0.3.0-20111227.121032-170.jar"
	  }
	  Seq(
	    "org.apache.ant" % "ant" % "1.8.1" % "compile;runtime;test",
	    "org.apache.ivy" % "ivy" % "2.1.0" % "compile;runtime;test",
	    "org.apache.maven" % "maven-ant-tasks" % "2.1.0" % "compile;runtime;test",
	    "org.sonatype.tycho" % "org.eclipse.jdt.core" % "3.6.0.v_A58" % "compile;runtime;test",
	    "asm" % "asm" % "3.2",
	    "asm" % "asm-commons" % "3.2",
	    scalatest,
	    scalariform,
	    scalaRefactoring,
	    "org.scala-lang" % "scala-compiler" % compilerVersion % "compile;runtime;test"
	  )},
	scalacOptions ++= Seq("-deprecation"),
	exportJars := true,
	stageTask,
	distTask,
	releaseTask,
	publishManualTask,
	
	{
	  import org.ensime.sbt.Plugin.Settings.ensimeConfig
	  import org.ensime.sbt.util.SExp._
	  ensimeConfig := sexp()
	}

      ))
  }



  val log = LogManager.defaultScreen

  var stage = TaskKey[Unit]("stage", 
    "Copy files into staging directory for a release.")
  lazy val stageTask:Setting[sbt.Task[Unit]] = 
  stage <<= (
    dependencyClasspath in Runtime,
    exportedProducts in Runtime,
    scalaVersion) map { (depCP, exportedCP, scalaBuildVersion) =>

    val distDir = "dist_" + scalaBuildVersion

    delete(file(distDir))

    log.info("Copying runtime environment to ./" + distDir + "....")
    createDirectories(List(
	file(distDir), 
	file(distDir + "/bin"),
	file(distDir + "/lib"),
	file(distDir + "/elisp")))

    // Copy the emacs lisp to dist
    val elisp = root / "src" / "main" / "elisp" ** "*.el"
    copy(elisp x flat(root / distDir / "elisp"))

    // Copy the runtime jars
    val deps = (depCP ++ exportedCP).map(_.data)
    copy(deps x flat(root / distDir / "lib"))

    // Grab all jars..
    val cpLibs = (root / distDir / "lib" ** "*.jar").get.flatMap(
      _.relativeTo(root / distDir))

    def writeScript(classpath:String, from:String, to:String){
      val tmplF = new File(from)
      val tmpl = read(tmplF)
      val s = tmpl.replace("<RUNTIME_CLASSPATH>", classpath)
      val f = new File(to)
      write(f, s)
      f.setExecutable(true)
    }

    // Expand the server invocation script templates.
    writeScript(cpLibs.mkString(":").replace("\\", "/"),
      "./etc/scripts/server",
      "./" + distDir + "/bin/server")

    writeScript("\"" + cpLibs.map{lib => "%~dp0/../" + lib}.mkString(";").replace("/", "\\") + "\"",
      "./etc/scripts/server.bat",
      "./" + distDir + "/bin/server.bat")

    copy(root / "etc" ** "sbt-launch-*.jar" x flat(root / distDir / "bin"))

    copyFile(root / "README.md", root / distDir / "README.md")
    copyFile(root / "LICENSE", root / distDir / "LICENSE")
  }


  var dist = TaskKey[Unit]("dist", "Create the release package.")
  lazy val distTask:Setting[sbt.Task[Unit]] = dist := {
    println("The 'dist' task is deprecated. Use 'stage' to create release directory structure. Use 'release' to create the release archive.")
    None
  }


  var release = TaskKey[Unit]("release", "Create the release package and tag the current commit.")
  lazy val releaseTask:Setting[sbt.Task[Unit]] = 
  release <<= (stage,version,scalaVersion) map {
    (_,version,scalaBuildVersion) =>

    val distDir = "dist_" + scalaBuildVersion
    val modName = "ensime_" + scalaBuildVersion + "-" + version
    val tagName = scalaBuildVersion + "-" + version

    val shallWeTag = false
    val tagArg = if(shallWeTag){ "-s" }else{ "" }
    doSh("git tag " + tagArg + " v" + tagName + 
      " -m 'Tag for release " + modName + "'") !! (log)

    val initialDir = new File(".")
    val archiveFile = new File(initialDir,
      modName + ".tar.gz").getCanonicalPath
    withTemporaryDirectory{ f =>
      val releaseDir = new File(
	f.getAbsolutePath + "/" + 
	modName)
      log.info("Copying ./" + distDir + " to temp directory: " + releaseDir)
      doSh("cp -r ./" + distDir + " " + releaseDir)!!(log)
      log.info("Compressing temp directory to " + archiveFile + "...")
      doSh("tar -pcvzf " + archiveFile + " " + 
	modName, Some(f)) !! (log)
      None
    }
    None
  }


  val publishManual = TaskKey[Unit]("publish-manual", "Publish the manual Create the release package.")
  lazy val publishManualTask:Setting[sbt.Task[Unit]] = publishManual := {
    log.info("Converting manual to html..")
    val target = "/tmp/ensime_manual.html"
    val cwd = Some(new File("etc"))
    doSh("pdflatex manual.ltx", cwd)!!log
    doSh("cat manual_head.html > " + target, cwd)!!log
    doSh("tth -r -u -e2 -Lmanual < manual.ltx >> " + target, cwd)!!(log)
    doSh("cat manual_tail.html >> " + target, cwd)!!log
    log.info("Publishing manual to web...")
    doSh("scp " + target + " www@aemon.com:~/public/aemon/file_dump/", cwd)!!(log)
    doSh("scp wire_protocol.png www@aemon.com:~/public/aemon/file_dump/", cwd)!!(log)
    None     
  }




}
