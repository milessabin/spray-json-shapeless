/**
*  Copyright (C) 2010 Aemon Cannon
*
*  This program is free software; you can redistribute it and/or
*  modify it under the terms of the GNU General Public License as
*  published by the Free Software Foundation; either version 2 of
*  the License, or (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public
*  License along with this program; if not, write to the Free
*  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
*  MA 02111-1307, USA.
*/

package org.ensime.config
import java.io.File
import org.apache.maven.artifact.ant.{MavenDepsTask, Pom}
import org.apache.tools.ant.{DefaultLogger, Project, Target}
import org.ensime.util.{CanonFile, FileUtils}
import scala.collection.JavaConversions._


object Maven extends ExternalConfigurator {

  import FileUtils._

  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    val srcPaths = maybeDirs(List(
      "src/main/scala",
      "src/main/java",
      "src/test/scala",
      "src/test/java"), baseDir)
    val runtimeDeps = resolveMavenDeps(baseDir, "runtime")
    val compileDeps = resolveMavenDeps(baseDir, "compile")
    val testDeps = resolveMavenDeps(baseDir, "test")

    val f = new File(baseDir, "target/classes")
    val buildTarget = if (f.exists) { Some(toCanonFile(f)) } else { None }

    Right(ExternalConfig(None, srcPaths, runtimeDeps,
      compileDeps, testDeps, buildTarget))
  }

  private def newConsoleLogger = {
    val consoleLogger = new DefaultLogger()
    consoleLogger.setErrorPrintStream(System.err)
    consoleLogger.setOutputPrintStream(System.out)
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO)
    consoleLogger
  }

  private def resolveMavenDeps(baseDir: File, conf: String): Iterable[CanonFile] = {

    // Recreate the default maven classpaths.
    val scopes = conf match {
      case "compile" => "compile,provided,system,test"
      case "runtime" => "compile,provided,system,runtime"
      case "test" => "compile,provided,system,runtime,test"
    }

    println("\n\nResolving Maven dependencies for ensime config: " + conf)
    val project = new Project()
    project.addBuildListener(newConsoleLogger)
    project.setBaseDir(baseDir)
    project.init()
    val target = new Target()
    target.setName("ResolveDependencies")
    target.setProject(project)

    val pom = new Pom()
    pom.setFile(new File(baseDir, "pom.xml"))
    pom.setOwningTarget(target)
    pom.setProject(project)
    pom.setId("pom")
    target.addTask(pom)

    val task = new MavenDepsTask()
    task.setOwningTarget(target)
    task.setProject(project)
    task.addPom(pom)
    println("Resolving with scopes: " + scopes)
    task.setScopes(scopes)
    target.addTask(task)

    project.addTarget("ResolveDependencies", target)

    try {
      project.executeTarget("ResolveDependencies")
    } catch {
      case e => {
        System.err.println("Failed to resolve Maven dependencies.")
        e.printStackTrace(System.err)
      }
    }

    task.deps.map(toCanonFile)
  }
}
