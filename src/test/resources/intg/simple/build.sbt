import sbt._
import java.io._
import sbt.IO
import org.scalastyle.sbt.ScalastylePlugin

organization := "org.ensime"

name := "simple"

scalaVersion := "2.11.1"

version := "0.1-SNAPSHOT"

scalacOptions in Compile ++= Seq(
  "-encoding", "UTF-8", "-target:jvm-1.6", "-feature", "-deprecation",
  "-Xfatal-warnings",
  "-language:postfixOps", "-language:implicitConversions"
)

javacOptions in (Compile, compile) ++= Seq (
  "-source", "1.6", "-target", "1.6", "-Xlint:all", //"-Werror",
  "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
)

javacOptions in doc ++= Seq("-source", "1.6")

maxErrors := 1

scalariformSettings

ScalastylePlugin.Settings

instrumentSettings

// let's bump this every time we get more tests
ScoverageKeys.minimumCoverage := 0

// might be buggy
ScoverageKeys.highlighting := true

ScoverageKeys.failOnMinimumCoverage := true

licenses := Seq("BSD 3 Clause" -> url("http://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("http://github.com/ensime/ensime-server"))

