resolvers += Resolver.sonatypeRepo("snapshots")

addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.5-SNAPSHOT")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.6")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.5.0")

addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "0.99.5.1")

//addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "0.98.0")
