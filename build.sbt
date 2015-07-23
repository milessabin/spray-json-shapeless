import sbt._

organization := "com.github.fommil"

name := "spray-json-shapeless"

scalaVersion := "2.11.7"

version := "1.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5",
  "io.spray" %% "spray-json" % "1.3.2",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3.12" % "test"
) ++ {
  if (scalaVersion.value.startsWith("2.10.")) Seq(
    compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  ) else Nil
}

dependencyOverrides ++= Set(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
) ++ {
  if (scalaVersion.value.startsWith("2.11")) Set(
    "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
  ) else Nil
}

scalacOptions in Compile ++= Seq(
  "-encoding", "UTF-8", "-target:jvm-1.6", "-feature", "-deprecation",
  "-Xfatal-warnings",
  "-language:postfixOps", "-language:implicitConversions"
)

javacOptions in (Compile, compile) ++= Seq(
  "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
  "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
)

javacOptions in doc ++= Seq("-source", "1.6")

javaOptions ++= Seq("-XX:MaxPermSize=256m", "-Xmx2g", "-XX:+UseConcMarkSweepGC")

javaOptions in Test += "-Dlogback.configurationFile=../logback-test.xml"

// WORKAROUND: https://github.com/scalatest/scalatest/issues/511
def noColorIfEmacs = if (sys.env.get("TERM") == Some("dumb"))
                       Seq(Tests.Argument("-oWF"))
                     else
                       Seq(Tests.Argument("-oF"))

testOptions in Test ++= noColorIfEmacs

licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html"))

homepage := Some(url("http://github.com/fommil/spray-json-shapeless"))

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.contains("SNAP")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(
  "Sonatype Nexus Repository Manager", "oss.sonatype.org",
  sys.env.getOrElse("SONATYPE_USERNAME", ""),
  sys.env.getOrElse("SONATYPE_PASSWORD", "")
)

pomExtra :=
<scm>
  <url>git@github.com:fommil/spray-json-shapeless.git</url>
  <connection>scm:git:git@github.com:fommil/spray-json-shapeless.git</connection>
</scm>
<developers>
   <developer>
      <id>fommil</id>
      <name>Sam Halliday</name>
   </developer>
</developers>
