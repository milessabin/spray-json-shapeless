package org.ensime.config

import org.ensime.server.Server
import org.ensime.test.TestUtil
import org.scalatest.{ Matchers, FunSpec }

import pimpathon.file._

import java.io.{ PrintWriter, File }
import org.ensime.util.RichFile._

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
      withTempDirectory { dirRaw =>
        val dir = dirRaw.canon
        (dir / ".ensime_cache").mkdirs()
        (dir / "abc").mkdirs()

        val dirStr = TestUtil.fileToWireString(dir)
        val cacheStr = TestUtil.fileToWireString(dir / ".ensime_cache")

        test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :root-dir $dirStr
 :cache-dir $cacheStr
 :reference-source-roots ()
 :debug-args ("-Dthis=that")
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
          assert(config.sourceMode == false)
          assert(config.debugVMArgs === List("-Dthis=that"))
        })
      }
    }

    it("should parse a minimal config for a binary only project") {
      withTempDirectory { dirRaw =>
        val dir = dirRaw.canon
        (dir / ".ensime_cache").mkdirs()
        (dir / "abc").mkdirs()

        val dirStr = TestUtil.fileToWireString(dir)
        val cacheStr = TestUtil.fileToWireString(dir / ".ensime_cache")

        test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :root-dir $dirStr
 :cache-dir $cacheStr
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :targets ("abc"))))""", { implicit config =>

          assert(config.name == "project")
          assert(config.scalaVersion == "2.10.4")
          val module1 = config.modules("module1")
          assert(module1.name == "module1")
          assert(module1.dependencies.isEmpty)
          assert(module1.targets.size === 1)
        })
      }
    }

    it("should base class paths on source-mode value") {
      List(true, false) foreach { (sourceMode: Boolean) =>
        withTempDirectory { dirRaw =>
          val dir = dirRaw.canon
          (dir / ".ensime_cache").mkdirs()
          (dir / "abc").mkdirs()

          val dirStr = TestUtil.fileToWireString(dir)
          val cacheStr = TestUtil.fileToWireString(dir / ".ensime_cache")

          test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :root-dir $dirStr
 :cache-dir $cacheStr
 :source-mode ${if (sourceMode) "t" else "nil"}
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :targets ("abc"))))""", { implicit config =>
            assert(config.sourceMode == sourceMode)
            assert(config.runtimeClasspath == Set(dir / "abc"))
            assert(config.compileClasspath == (
              if (sourceMode) Set.empty else Set(dir / "abc")
            ))
          })
        }
      }
    }
  }
}
