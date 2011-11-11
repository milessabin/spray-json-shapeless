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

case class SbtSubproject(name: String, deps: List[String])

object Sbt extends ExternalConfigurator {

  private trait SbtInstance{

    class IndexTracker(var i: Int)
    protected implicit object IndexTracker extends IndexTracker(0)

    protected def mostRecentStr(implicit x: IndexTracker, shell: Spawn): String = {
      val all = shell.getCurrentStandardOutContents()
      val s = all.substring(x.i)
      x.i = all.length
      s
    }

    def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig]
    def versionName:String
    def jarName:String
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

    protected val delim = "%%ENSIME%%"
    protected def expandDelim = delim.map("\"" + _ + "\"").mkString(" + ")

    def spawn(baseDir: File): Spawn = {
      import scala.collection.JavaConversions._
      val expectinator = new ExpectJ()
      val pathToSbtJar = (new File(".", "bin/" + jarName)).getCanonicalPath()
      expectinator.spawn(new Executor(){
	  def execute():Process = {
	    val reqArgs = Vector(
	      "-Djline.terminal=jline.UnixTerminal",
	      "-Dsbt.log.noformat=true",
	      "-jar", pathToSbtJar)
	    val args = Vector("java") ++ jvmArgs ++ reqArgs ++ appArgs
	    println("Starting sbt with command line: " + args.mkString(" "))
	    val pb = new ProcessBuilder(args)
	    pb.directory(baseDir)
	    pb.start()
	  }
	  override def toString():String = "ENSIME-controlled sbt-" + versionName + " process"
	})
    }
  }


  private class Sbt7Style extends SbtInstance{

    def versionName:String = "0.7"
    def jarName:String = "sbt-launch-0.7.7.jar"
    override def appArgs:List[String] = List("console-project")

    def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
      try {

	// ExpectJ object with a timeout of 5s
	implicit val shell = spawn(baseDir)

	shell.expect(prompt)

	conf.sbtActiveSubproject match {
	  case Some(sub) => {
            evalUnit("val p = subProjects.values.find(_.name == \"" + sub.name + "\").get.asInstanceOf[DefaultProject]")
            // Fail fast if subproject was not found...
            eval("p.name")
	  }
	  case None => {
            evalUnit("val p = current")
	  }
	}

	val name = eval("p.name")
	val org = eval("p.organization")
	val sbtVersion = eval("sbtVersion.value")
	val projectVersion = eval("p.version")
	val buildScalaVersion = eval("p.buildScalaVersion")
	val compileDeps = evalList("(p.compileClasspath +++ p.fullClasspath(Configurations.Test)).get.toList.map(_.toString)")
	val testDeps = evalList("(p.testClasspath).get.toList.map(_.toString)")
	val runtimeDeps = evalList("(p.runClasspath  +++ p.fullClasspath(Configurations.Test)).get.toList.map(_.toString)")
	val sourceRoots = evalList("(p.mainSourceRoots +++ p.testSourceRoots).get.toList.map(_.toString)")
	val target = eval("p.outputPath.toString")

	shell.send(":q\n")
	shell.expectClose()
	shell.stop()

	import FileUtils._
	val testDepFiles = maybeFiles(testDeps, baseDir)
	val compileDepFiles = maybeFiles(compileDeps, baseDir) ++ testDepFiles
	val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir) ++ testDepFiles
	val sourceRootFiles = maybeDirs(sourceRoots, baseDir)
	val f = new File(baseDir, target)
	val targetDir = if (f.exists) { Some(toCanonFile(f)) } else { None }

	Right(ExternalConfig(Some(name), sourceRootFiles,
            runtimeDepFiles, compileDepFiles, testDepFiles,
            targetDir))

      } catch {
	case e: expectj.TimeoutException => Left(e)
	case e: Exception => Left(e)
      }

    }



    private def isolated(str: String) = expandDelim + " + " + str + " + " + expandDelim
    private def printIsolated(str: String) = "println(" + isolated(str) + ")\n"
    private val pattern: Pattern = Pattern.compile(delim + "(.+?)" + delim)
    private val prompt: String = "scala> "

    private def parseValues(input: String): Option[String] = {
      val m = pattern.matcher(input);
      if (m.find()) Some(m.group(1))
      else None
    }

    private def eval(expr: String)(implicit shell: Spawn): String = {
      shell.send(printIsolated(expr))
      shell.expect(prompt)
      parseValues(mostRecentStr) match {
	case Some(s) => s
	case _ => throw new RuntimeException("Failed to parse result of " + expr)
      }
    }

    private def evalUnit(expr: String)(implicit shell: Spawn): Unit = {
      shell.send(expr + "\n")
      shell.expect(prompt)
    }

    private def evalList(expr: String)(implicit shell: Spawn): List[String] = {
      shell.send(printIsolated(expr))
      shell.expect(prompt)
      parseValues(mostRecentStr) match {
	case Some(s) if (s.startsWith("List(") && s.endsWith(")")) => {
          (s.substring(5, s.length - 1)).split(", ").toList
	}
	case _ => throw new RuntimeException("Failed to parse result of " + expr)
      }
    }

  }


  private class Sbt10Style extends SbtInstance{

    def versionName:String = "0.10"

    def jarName:String = "sbt-launch-0.10.1.jar"

    def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
      implicit val shell = spawn(baseDir)

      evalUnit("set shellPrompt := {state => \"\"}")

      conf.sbtActiveSubproject match {
	case Some(sub) => {
	  evalUnit("project " + sub.name)
	}
	case None =>
      }

      def getList(key:String):List[String] = {
	parseAttributedFilesList(
	  showSetting(key).getOrElse("List()"))
      }

      val name = showSetting("name").getOrElse("NA")
      val org = showSetting("organization").getOrElse("NA")
      val projectVersion = showSetting("version").getOrElse("NA")
      val buildScalaVersion = showSetting("scala-version").getOrElse("2.9.1")

      val compileDeps = (
	getList("compile:unmanaged-classpath") ++ 
	getList("compile:managed-classpath") ++ 
	getList("compile:internal-dependency-classpath")
      )
      val testDeps = (
	getList("test:unmanaged-classpath") ++
	getList("test:managed-classpath") ++ 
	getList("test:internal-dependency-classpath") ++ 
	getList("test:exported-products")
      )
      val runtimeDeps = (
	getList("runtime:unmanaged-classpath") ++
	getList("runtime:managed-classpath") ++
	getList("runtime:internal-dependency-classpath") ++ 
	getList("runtime:exported-products")
      )

      val sourceRoots =  (
	getList("compile:source-directories") ++
	getList("test:source-directories")
      )
      println("parsed " + sourceRoots)
      val target = CanonFile(showSetting("class-directory").getOrElse("./classes"))

      shell.send("exit\n")
      shell.expectClose()
      shell.stop()

      import FileUtils._

      val testDepFiles = maybeFiles(testDeps, baseDir)
      val compileDepFiles = maybeFiles(compileDeps, baseDir) ++ testDepFiles
      val runtimeDepFiles = maybeFiles(runtimeDeps, baseDir) ++ testDepFiles
      val sourceRootFiles = maybeDirs(sourceRoots, baseDir)

      println("files " + sourceRoots)

      Right(ExternalConfig(Some(name), sourceRootFiles,
	  runtimeDepFiles, compileDepFiles, testDepFiles,
	  Some(target)))
    }

    private val singleLineSetting = Pattern.compile("^\\[info\\] (.+)$", Pattern.MULTILINE)

    private def parseSettingStr(input: String): Option[String] = {
      val m = singleLineSetting.matcher(input);
      var result: Option[String] = None
      while (m.find()) {
	val g = m.group(1)
	if(g.indexOf(delim) == -1){
	  result = Some(g)
	}
      }
      result
    }


    private def evalUnit(expr: String)(implicit shell: Spawn): Unit = {
      shell.send(expr + "\n")
      shell.send("eval " + expandDelim + "\n")
      shell.expect(delim)
      mostRecentStr
    }

    private def showSetting(expr: String)(implicit shell: Spawn): Option[String] = {
      shell.send("show " + expr + "\n")
      shell.send("eval " + expandDelim + "\n")
      shell.expect(delim)
      parseSettingStr(mostRecentStr)
    }

    import scala.util.parsing.input._
    import scala.util.parsing.combinator._
    private object ListParser extends RegexParsers {
      def listOpen = regex("([A-z]+)\\(".r)
      def listClose = regex("\\)".r)
      def attrOpen = regex("Attributed\\(".r)
      def attrClose = regex("\\)".r)
      def list = listOpen ~> repsep(file, ", ") <~ listClose
      def file = (attrFile | unAttrFile)
      def attrFile = attrOpen ~> unAttrFile <~ attrClose
      def unAttrFile = regex("[^\\),]+".r)
    }

    private def parseAttributedFilesList(s: String):List[String] = {
      val result: ListParser.ParseResult[List[String]] = ListParser.list(
	new CharSequenceReader(s))
      result match {
	case ListParser.Success(value, next) => value
	case ListParser.Failure(errMsg, next) => {
	  System.err.println(errMsg)
	  List()
	}
	case ListParser.Error(errMsg, next) => {
	  System.err.println(errMsg)
	  List()
	}
      }
    }
  }


  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    val props = JavaProperties.load(new File(baseDir, "project/build.properties"))
    val sbt11 = (new Sbt10Style(){
	override def versionName:String = "0.11"
	override def jarName:String = "sbt-launch-0.11.0.jar"
      })
    val sbt9 = (new Sbt10Style(){
	override def versionName:String = "0.9"
      })
    props.get("sbt.version").orElse(conf.sbtVersion) match {
      case Some(v:String) => {

	if(v.startsWith("0.7")) (new Sbt7Style()).getConfig(baseDir, conf)

	else if(v.startsWith("0.9")) sbt9.getConfig(baseDir, conf)

	else if(v.startsWith("0.10")) (new Sbt10Style()).getConfig(baseDir, conf)

	else if(v.startsWith("0.11")) sbt11.getConfig(baseDir, conf)

	else {
	  println("Unrecognized sbt version " + v + ". Guessing sbt-11...")
	  sbt11.getConfig(baseDir, conf)
	}
      }
      case None => {
	println("No sbt version specified. Guessing sbt-11...")
	sbt11.getConfig(baseDir, conf)
      }
    }
  }



  def main(args: Array[String]) {

    //    println(sbt10.parseAttributedFilesList("List(Attributed(/home/aemon/src/misc/ensime/lib/org.scala-refactoring_2.9.0-0.3.0-SNAPSHOT.jar), Attributed(/home/aemon/src/misc/ensime/lib/critbit-0.0.4.jar), Attributed(/home/aemon/src/misc/ensime/project/boot/scala-2.9.1/lib/scala-library.jar), Attributed(/home/aemon/src/misc/ensime/project/boot/scala-2.9.1/lib/scala-compiler.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.ant/ant/jars/ant-1.8.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.ant/ant-launcher/jars/ant-launcher-1.8.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.ivy/ivy/jars/ivy-2.1.0.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-ant-tasks/jars/maven-ant-tasks-2.1.0.jar), Attributed(/home/aemon/.ivy2/cache/ant/ant/jars/ant-1.6.5.jar), Attributed(/home/aemon/.ivy2/cache/classworlds/classworlds/jars/classworlds-1.1-alpha-2.jar), Attributed(/home/aemon/.ivy2/cache/org.codehaus.plexus/plexus-container-default/jars/plexus-container-default-1.0-alpha-9-stable-1.jar), Attributed(/home/aemon/.ivy2/cache/org.codehaus.plexus/plexus-utils/jars/plexus-utils-1.5.15.jar), Attributed(/home/aemon/.ivy2/cache/org.codehaus.plexus/plexus-interpolation/jars/plexus-interpolation-1.11.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-artifact/jars/maven-artifact-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-artifact-manager/jars/maven-artifact-manager-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-repository-metadata/jars/maven-repository-metadata-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-provider-api/jars/wagon-provider-api-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/backport-util-concurrent/backport-util-concurrent/jars/backport-util-concurrent-3.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-model/jars/maven-model-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-project/jars/maven-project-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-settings/jars/maven-settings-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-profile/jars/maven-profile-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-plugin-registry/jars/maven-plugin-registry-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven/maven-error-diagnostics/jars/maven-error-diagnostics-2.2.1.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-file/jars/wagon-file-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-http-lightweight/jars/wagon-http-lightweight-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/org.apache.maven.wagon/wagon-http-shared/jars/wagon-http-shared-1.0-beta-6.jar), Attributed(/home/aemon/.ivy2/cache/nekohtml/xercesMinimal/jars/xercesMinimal-1.9.6.2.jar), Attributed(/home/aemon/.ivy2/cache/nekohtml/nekohtml/jars/nekohtml-1.9.6.2.jar), Attributed(/home/aemon/.ivy2/cache/org.sonatype.tycho/org.eclipse.jdt.core/jars/org.eclipse.jdt.core-3.6.0.v_A58.jar), Attributed(/home/aemon/.ivy2/cache/org.scalariform/scalariform_2.9.0/jars/scalariform_2.9.0-0.1.0.jar), Attributed(/home/aemon/.ivy2/cache/net.sourceforge.expectj/expectj/jars/expectj-2.0.1.jar), Attributed(/home/aemon/.ivy2/cache/commons-logging/commons-logging/jars/commons-logging-1.1.1.jar), Attributed(/home/aemon/.ivy2/cache/asm/asm/jars/asm-3.2.jar), Attributed(/home/aemon/.ivy2/cache/asm/asm-commons/jars/asm-commons-3.2.jar), Attributed(/home/aemon/.ivy2/cache/asm/asm-tree/jars/asm-tree-3.2.jar))"))

    //    println(sbt10.parseAttributedFilesList("List(/Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/scala, /Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/java, /Users/daniel/Local/ensime_2.9.1-0.7.RC1/target/scala-2.8.1.final/src_managed/main)"))

    //    println(sbt10.parseAttributedFilesList("List(    /Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/scala,     /Users/daniel/Local/ensime_2.9.1-0.7.RC1/src/main/java,    /Users/daniel/Local/ensime_2.9.1-0.7.RC1/target/scala-2.8.1.final/src_managed/main)"))


  }

}
