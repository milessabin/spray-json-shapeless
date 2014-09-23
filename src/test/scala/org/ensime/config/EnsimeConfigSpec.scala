package org.ensime.config

import org.ensime.server.Server
import org.ensime.test.TestUtil
import org.scalatest.{ Matchers, FunSpec }

import pimpathon.file._

import java.io.{ PrintWriter, File }

class EnsimeConfigSpec extends FunSpec with Matchers {

  def writeToFile(contents: String, file: File): Unit = {
    val out = new PrintWriter(file, "UTF-8")
    try {
      out.print(contents)
    } finally { out.close() }
  }

  def test(dir: File, contents: String, testFn: (EnsimeConfig) => Unit): Unit = {
    val ensimeFile = dir / ".ensime"
    writeToFile(contents, ensimeFile)
    val config = Server.readEnsimeConfig(ensimeFile)
    testFn(config)
  }

  describe("ProjectConfigSpec") {

    it("should parse a simple config") {
      withTempDirectory { dir =>
        (dir / ".ensime_cache").mkdirs()
        (dir / "abc").mkdirs()
        test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :root-dir "$dir"
 :cache-dir "$dir/.ensime_cache"
 :reference-source-roots ()
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :depends-on-modules ()
                :target "abc"
                :test-target "abc"
                :source-roots ()
                :reference-source-roots ()
                :compiler-args ()
                :runtime-deps ()
                :test-deps ())))""", { implicit config =>

          assert(config.name == "project")
          assert(config.scalaVersion == "2.10.4")
          val module1 = config.modules("module1")
          assert(module1.name == "module1")
          assert(module1.dependencies.isEmpty)
        })
      }
    }
  }

}
