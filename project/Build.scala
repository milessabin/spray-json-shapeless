/**
*  Copyright (C) 2011 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
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


  lazy val project = Project(
    "ensime",
    file (".")) settings(
    version := "0.7.6",
    organization := "org.ensime",
    scalaVersion := "2.9.1",
    resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
    resolvers +=  "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.9.1" % "compile;runtime;test",
      "org.apache.ant" % "ant" % "1.8.1" % "compile;runtime;test",
      "org.apache.ivy" % "ivy" % "2.1.0" % "compile;runtime;test",
      "org.apache.maven" % "maven-ant-tasks" % "2.1.0" % "compile;runtime;test",
      "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test",
      "org.sonatype.tycho" % "org.eclipse.jdt.core" % "3.6.0.v_A58" % "compile;runtime;test",
      "org.scalariform" % "scalariform_2.9.0" % "0.1.0" % "compile;runtime;test",
      "org.scala-refactoring" % "org.scala-refactoring_2.9.1" % "0.3.0-SNAPSHOT" from "http://scala-tools.org/repo-snapshots/org/scala-refactoring/org.scala-refactoring_2.9.1/0.3.0-SNAPSHOT/org.scala-refactoring_2.9.1-0.3.0-20110901.093718-1.jar",
      "net.sourceforge.expectj" % "expectj" % "2.0.1" % "compile;runtime;test",
      "asm" % "asm" % "3.2",
      "asm" % "asm-commons" % "3.2"
    ),
    exportJars := true,
    stageTask,
    distTask,
    releaseTask,
    publishManualTask
  )



  val log = LogManager.defaultScreen

  var stage = TaskKey[Unit]("stage", 
    "Copy files into staging directory for a release.")
  lazy val stageTask:Setting[sbt.Task[Unit]] = 
  stage <<= (
    dependencyClasspath in Runtime,
    exportedProducts in Runtime) map { (depCP, exportedCP) =>

    delete(file("dist"))

    log.info("Copying runtime environment to ./dist....")
    createDirectories(List(
	file("dist"), 
	file("dist/bin"),
	file("dist/lib"),
	file("dist/elisp")))

    // Copy the emacs lisp to dist
    val elisp = root / "src" / "main" / "elisp" ** "*.el"
    copy(elisp x flat(root / "dist" / "elisp"))

    // Copy the runtime jars
    val deps = (depCP ++ exportedCP).map(_.data)
    copy(deps x flat(root / "dist" / "lib"))

    // Grab all jars..
    val cpLibs = (root / "dist" / "lib" ** "*.jar").get.flatMap(
      _.relativeTo(root / "dist"))

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
      "./dist/bin/server")

    writeScript("\"" + cpLibs.mkString(";").replace("/", "\\") + "\"",
      "./etc/scripts/server.bat",
      "./dist/bin/server.bat")

    copy(root / "etc" ** "sbt-launch-*.jar" x flat(root / "dist" / "bin"))

    copyFile(root / "README.md", root / "dist" / "README.md")
    copyFile(root / "LICENSE", root / "dist" / "LICENSE")
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
    val modName = "ensime_" + scalaBuildVersion + "-" + version

    doSh("git tag -s v" + version + 
      " -m 'Tag for release " + modName + "'") !! (log)

    val initialDir = new File(".")
    val archiveFile = new File(initialDir,
      modName + ".tar.gz").getCanonicalPath
    withTemporaryDirectory{ f =>
      val releaseDir = new File(
	f.getAbsolutePath + "/" + 
	modName)
      log.info("Copying ./dist to temp directory: " + releaseDir)
      doSh("cp -r ./dist " + releaseDir)!!(log)
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
