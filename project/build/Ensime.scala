import sbt._
import sbt.FileUtilities._

class EnsimeProject(info: ProjectInfo) extends DefaultProject(info){

  // Copy the ensime.jar, scala-library.jar and scala-compiler.jar to 
  // the bin directory, for conveniant running.
  lazy val dist = task {

    FileUtilities.clean(path("dist"), log)

    log.info("Preparing runtime environment to ./dist....")

    createDirectories(List(
	path("dist"), 
	"dist" / "bin",
	"dist" / "lib",
	"dist" / "elisp"
      ), log)

    copyFile(jarPath, "dist" / "lib" / "ensime.jar", log)

    val deps = mainDependencies
    copyFlat(deps.scalaJars.get, "dist" / "lib", log)

    val elisp = "src" / "main" / "elisp" ** "*.el"
    copyFlat(elisp.get, "dist" / "elisp", log)

    val scripts = "etc" ** "server.*"
    copyFlat(scripts.get, "dist" / "bin", log)
    val f = ("dist" / "bin" / "server.sh").asFile
    f.setExecutable(true)

    copyFile(path("README.md"), "dist" / "README.md", log)
    copyFile(path("LICENSE"), "dist" / "LICENSE", log)

    

    None 
  } dependsOn(`package`) describedAs("Copy jars to bin folder for end-user conveniance.")


}

