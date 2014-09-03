// WORKAROUND https://github.com/sbt/sbt/issues/1439
def plugin(m: ModuleID) =
  Defaults.sbtPluginExtra(m, "0.13", "2.10") excludeAll ExclusionRule("org.scala-lang")

libraryDependencies ++= Seq(
  plugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4"),
  plugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0"),
  plugin("org.scoverage" %% "sbt-scoverage" % "0.99.7.1"),
  plugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "0.94.4"),
  plugin("org.scoverage" %% "sbt-coveralls" % "0.99.0")
)
