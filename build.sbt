import sbt._
import java.io._
import sbt.IO
import net.virtualvoid.sbt.graph.Plugin.graphSettings
//import CoverallsPlugin.CoverallsKeys._

organization := "org.ensime"

name := "ensime"

scalaVersion := "2.10.4"

version := "0.9.10-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies <<= scalaVersion { scalaVersion => Seq(
  "org.ensime"                 %  "critbit"              % "0.0.5-SNAPSHOT",
  "org.apache.lucene"          %  "lucene-core"          % "3.5.0",
  "org.sonatype.tycho"         %  "org.eclipse.jdt.core" % "3.6.2.v_A76_R36x",
  "asm"                        %  "asm-commons"          % "3.3.1",
  "asm"                        %  "asm-util"             % "3.3.1",
  "com.googlecode.json-simple" %  "json-simple"          % "1.1.1" intransitive(),
  "org.scalatest"              %% "scalatest"            % "2.2.0" % "test",
  "org.scalariform"            %% "scalariform"          % "0.1.4",
  "org.scala-lang"             %  "scala-compiler"       % scalaVersion,
  "org.scala-lang"             %  "scala-reflect"        % scalaVersion,
  "org.scala-lang"             %  "scala-actors"         % scalaVersion,
  "org.scala-refactoring"      %% "org.scala-refactoring.library" % "0.6.2"
)}

val JavaTools = List[File](
  new File(Option(System.getenv("JAVA_HOME")).getOrElse("/tmp") + "/lib/tools.jar"),
  new File(new File(System.getProperty("java.home")).getParent + "/lib/tools.jar")
).find(_.exists).getOrElse(
  throw new FileNotFoundException("tools.jar")
)

internalDependencyClasspath in Compile += { Attributed.blank(JavaTools) }

// 0.10 is busted
addCompilerPlugin("org.brianmckenna" %% "wartremover" % "0.9")

scalacOptions in Compile ++= Seq(
  "-encoding", "UTF-8", "-target:jvm-1.6", "-feature", //"-Xfatal-warnings",
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

instrumentSettings

// let's bump this every time we get more tests
ScoverageKeys.minimumCoverage := 23

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
  Option(System.getenv("SONATYPE_USERNAME")).getOrElse(""),
  Option(System.getenv("SONATYPE_PASSWORD")).getOrElse("")
)
