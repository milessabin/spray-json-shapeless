import sbt._
import sbt.FileUtilities._

class EnsimeProject(info: ProjectInfo) extends DefaultProject(info){

  // Copy the ensime.jar, scala-library.jar and scala-compiler.jar to 
  // the bin directory, for conveniant running.
  lazy val dist = task {
    copyFile(jarPath, "bin" / "ensime.jar", log)
    val deps = mainDependencies
    copyFlat(deps.scalaJars.get, path("bin"), log)
    None 
  } dependsOn(`package`) describedAs("Copy jars to bin folder for end-user conveniance.")


}

