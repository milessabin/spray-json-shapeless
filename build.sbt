name := "ensime"

version := "0.6.RC2"

organization := "org.ensime"

scalaVersion := "2.9.0"

resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

resolvers +=  "JBoss Maven 2 Repo" at "http://repository.jboss.org/maven2"

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
)
