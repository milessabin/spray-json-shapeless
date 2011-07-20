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
    version := "0.6.RC2",
    organization := "org.ensime",
    scalaVersion := "2.9.0",
    resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
    resolvers +=  "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % "2.9.0" % "compile;runtime;test",
      "org.apache.ant" % "ant" % "1.8.1" % "compile;runtime;test",
      "org.apache.ivy" % "ivy" % "2.1.0" % "compile;runtime;test",
      "org.apache.maven" % "maven-ant-tasks" % "2.1.0" % "compile;runtime;test",
      "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test",
      "org.sonatype.tycho" % "org.eclipse.jdt.core" % "3.6.0.v_A58" % "compile;runtime;test",
      //  "org.scalariform" %% "scalariform" % "0.0.9" % "compile;runtime;test",
      //  "org.scala-refactoring" % "org.scala-refactoring.library" % "0.2.0-SNAPSHOT"%"compile;runtime;test",
      "net.sourceforge.expectj" % "expectj" % "2.0.1" % "compile;runtime;test",
      "asm" % "asm" % "3.2",
      "asm" % "asm-commons" % "3.2"
    ),
    exportJars := true,
    stageTask,
    distTask,
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
  lazy val distTask:Setting[sbt.Task[Unit]] = 
  dist <<= (stage,moduleID,version,scalaVersion) map {
    (_,id,version,scalaBuildVersion) =>
    val modName = id + "_" + scalaBuildVersion + "-" + version
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
    doSh("tth -r -u -Lmanual < manual.ltx >> " + target, cwd)!!(log)
    doSh("cat manual_tail.html >> " + target, cwd)!!log
    log.info("Publishing manual to web...")
    doSh("scp " + target + " www@aemon.com:~/public/aemon/file_dump/", cwd)!!(log)
    None     
  }




}
