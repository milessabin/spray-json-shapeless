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
import org.apache.ivy.{ core, util }
import org.apache.ivy.ant.IvyCacheTask
import org.apache.ivy.core.report.ArtifactDownloadReport
import org.apache.maven.artifact.ant._
import org.apache.tools.ant._
import org.ensime.util._
import org.ensime.util.FileUtils._
import scala.collection.mutable.ListBuffer
import java.util.Properties

case class ExternalConfig(
  val projectName: Option[String],
  val sourceRoots: Iterable[CanonFile],
  val runtimeDepFiles: Iterable[CanonFile],
  val compileDepFiles: Iterable[CanonFile],
  val testDepFiles: Iterable[CanonFile],
  val target: Option[CanonFile]) {}

trait ExternalConfigurator {
  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig]
}






