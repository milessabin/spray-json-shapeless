import sbt._
import java.io._
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import org.scalastyle.sbt.ScalastylePlugin
//import CoverallsPlugin.CoverallsKeys._

organization := "org.ensime"

name := "ensime"

scalaVersion := "2.11.1"

version := "0.9.10-SNAPSHOT"

libraryDependencies <<= scalaVersion { scalaVersion => Seq(
  "org.apache.lucene"          %  "lucene-core"          % "3.5.0",
  "org.sonatype.tycho"         %  "org.eclipse.jdt.core" % "3.6.2.v_A76_R36x",
  "org.ow2.asm"                %  "asm-commons"          % "5.0.3",
  "org.ow2.asm"                %  "asm-util"             % "5.0.3",
  "com.googlecode.json-simple" %  "json-simple"          % "1.1.1" intransitive(),
  "org.scalatest"              %% "scalatest"            % "2.2.0" % "test",
  "com.danieltrinh"            %% "scalariform"          % "0.1.5",
  "org.scala-lang"             %  "scala-compiler"       % scalaVersion,
  "org.scala-lang"             %  "scala-reflect"        % scalaVersion,
  "com.typesafe.akka"          %% "akka-actor" 	         % "2.3.4",
  "com.typesafe.akka"          %% "akka-slf4j"           % "2.3.4",
  "com.typesafe.akka"          %% "akka-testkit"         % "2.3.4" % "test",
  "ch.qos.logback"             %  "logback-classic"      % "1.0.13",
  "org.slf4j"                  %  "jul-to-slf4j"         % "1.7.7",
  "org.scala-refactoring"      %% "org.scala-refactoring.library" % "0.6.2"
)}

// epic hack to get the tools.jar JDK dependency
val JavaTools = List[Option[String]] (
  // manual
  sys.env.get("JDK_HOME"),
  sys.env.get("JAVA_HOME"),
  // osx
  try Some("/usr/libexec/java_home".!!.trim)
  catch {
    case _: Throwable => None
  },
  // fallback
  sys.props.get("java.home").map(new File(_).getParent),
  sys.props.get("java.home")
).flatten.map { n =>
  new File(n + "/lib/tools.jar")
}.find(_.exists).getOrElse (
  throw new FileNotFoundException (
    """Could not automatically find the JDK/lib/tools.jar.
      |You must explicitly set JDK_HOME or JAVA_HOME.""".stripMargin
  )
)

internalDependencyClasspath in Compile += { Attributed.blank(JavaTools) }

// 0.10 is busted
addCompilerPlugin("org.brianmckenna" % "wartremover_2.11.0-RC4" % "0.9")

scalacOptions in Compile ++= Seq(
  "-encoding", "UTF-8", "-target:jvm-1.6", "-feature", "-deprecation",
  "-Xfatal-warnings",
  "-language:postfixOps", "-language:implicitConversions"
  //"-P:wartremover:only-warn-traverser:org.brianmckenna.wartremover.warts.Unsafe"
  //"-P:wartremover:traverser:org.brianmckenna.wartremover.warts.Unsafe"
)

javacOptions in (Compile, compile) ++= Seq (
  "-source", "1.6", "-target", "1.6", "-Xlint:all", //"-Werror",
  "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
)

javacOptions in doc ++= Seq("-source", "1.6")

maxErrors := 1

graphSettings

scalariformSettings

ScalastylePlugin.Settings

instrumentSettings

// let's bump this every time we get more tests
ScoverageKeys.minimumCoverage := 25

// might be buggy
ScoverageKeys.highlighting := true

ScoverageKeys.failOnMinimumCoverage := true

//coverallsSettings

licenses := Seq("BSD 3 Clause" -> url("http://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("http://github.com/ensime/ensime-server"))

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.contains("SNAP")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else                    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(
  "Sonatype Nexus Repository Manager", "oss.sonatype.org",
  sys.env.get("SONATYPE_USERNAME").getOrElse(""),
  sys.env.get("SONATYPE_PASSWORD").getOrElse("")
)
