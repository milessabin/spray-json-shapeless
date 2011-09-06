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
import org.apache.ivy.ant.IvyCacheTask
import org.apache.ivy.core.report.ArtifactDownloadReport
import org.apache.tools.ant.{DefaultLogger, Project, Target}
import org.ensime.util.{CanonFile, FileUtils}
import scala.collection.JavaConversions._


object Ivy extends ExternalConfigurator {

  import FileUtils._

  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    
    val ivyFile = conf.ivyFile.map { new File(_) }
    val runtimeConf = conf.ivyRuntimeConf
    val compileConf = conf.ivyCompileConf
    val testConf = conf.ivyTestConf

    val srcPaths = maybeDirs(List(
	"src/main/scala",
	"src/main/java",
	"src/test/scala",
	"src/test/java"), baseDir)

    val resolve = { c: String => resolveIvyDeps(baseDir, ivyFile, c) }

    val defaultDeps = resolve("default")
    val runtimeDeps = runtimeConf.map(resolve(_)).getOrElse(defaultDeps)
    val compileDeps = compileConf.map(resolve(_)).getOrElse(defaultDeps)
    val testDeps = testConf.map(resolve(_)).getOrElse(defaultDeps)

    Right(ExternalConfig(None, srcPaths, runtimeDeps, 
	compileDeps, testDeps, None))
  }

  private def newConsoleLogger = {
    val consoleLogger = new DefaultLogger()
    consoleLogger.setErrorPrintStream(System.err)
    consoleLogger.setOutputPrintStream(System.out)
    consoleLogger.setMessageOutputLevel(Project.MSG_INFO)
    consoleLogger
  }

  private class IvyDepsTask extends IvyCacheTask() {
    var deps: Iterable[File] = List()
    def doExecute() {
      prepareAndCheck()
      deps = getArtifactReports().map { a =>
	val art = a.asInstanceOf[ArtifactDownloadReport]
	art.getLocalFile()
      }
    }
  }

  private def resolveIvyDeps(baseDir: File, ivyFile: Option[File], conf: String): Iterable[CanonFile] = {
    println("Resolving Ivy dependencies...")
    val project = new Project()
    project.addBuildListener(newConsoleLogger)
    project.setBaseDir(baseDir)
    project.init()
    val target = new Target()
    target.setName("ResolveDependencies")
    target.setProject(project)

    val task = new IvyDepsTask()
    task.setOwningTarget(target)
    task.setProject(project)
    for (f <- ivyFile) {
      task.setFile(f)
      println("Using ivy file '" + f + "'.")
    }
    println("Using config '" + conf + "'.")
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
