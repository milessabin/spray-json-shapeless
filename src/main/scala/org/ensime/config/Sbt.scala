/**
*  Copyright (c) 2010, Aemon Cannon
*  All rights reserved.
*  
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above copyright
*        notice, this list of conditions and the following disclaimer in the
*        documentation and/or other materials provided with the distribution.
*      * Neither the name of ENSIME nor the
*        names of its contributors may be used to endorse or promote products
*        derived from this software without specific prior written permission.
*  
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
*  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.ensime.config
import expectj.{ExpectJ, Spawn, Executor}
import java.io.File
import java.util.regex.Pattern
import scala.util.matching._
import org.ensime.util._
import scala.collection.mutable.ArrayBuffer
import FileUtils._


case class SbtSubproject(name: String, deps: List[String])

object Sbt extends ExternalConfigurator {

  trait SbtFormatHandler {
    def name(): Option[String]
    def pack(): Option[String]
    def version(): Option[String]
    def compileDeps(): List[String]
    def runtimeDeps(): List[String]
    def testDeps(): List[String]
    def sourceRoots(): List[String]
    def target(): Option[String]
  }

  class SExpSbtFormatHandler(config: SExpList) extends SbtFormatHandler with SExpFormatHelper {
    override val m: Map[KeywordAtom, SExp] = config.toKeywordMap
    def name(): Option[String] = getStr(":name")
    def pack(): Option[String] = getStr(":package")
    def version(): Option[String] = getStr(":version")
    def compileDeps(): List[String] = getStrList(":compile-deps")
    def runtimeDeps(): List[String] = getStrList(":runtime-deps")
    def testDeps(): List[String] = getStrList(":test-deps")
    def sourceRoots(): List[String] = getStrList(":source-roots")
    def target(): Option[String] = getStr(":target")
  }

  def formatHandler(sexp: SExpList) = new SExpSbtFormatHandler(sexp)

  def getConfig(baseDir: File, conf: FormatHandler): Either[Throwable, ExternalConfig] = {
    conf.sbtActiveSubproject match{
      case Some(conf) => {
	val testDepFiles = maybeFiles(conf.testDeps, baseDir)
	val compileDepFiles = maybeFiles(conf.compileDeps, baseDir) ++ testDepFiles
	val runtimeDepFiles = maybeFiles(conf.runtimeDeps, baseDir) ++ testDepFiles
	val sourceRootFiles = maybeDirs(conf.sourceRoots, baseDir)
	Right(ExternalConfig(
	    conf.name, 
	    sourceRootFiles,
	    runtimeDepFiles, 
	    compileDepFiles, 
	    testDepFiles,
	    conf.target.map(CanonFile.apply)))
      }
      case None => Left(new RuntimeException("No sbt project selected."))
    }
  }
}
