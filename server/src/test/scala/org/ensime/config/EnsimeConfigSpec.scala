package org.ensime.config

import org.scalatest.{ Matchers, FunSpec }

import pimpathon.file._

import java.io.{ PrintWriter, File }
import org.ensime.util.RichFile._

import org.ensime.util.UnitTestUtils._
import scala.util.Properties

class EnsimeConfigSpec extends FunSpec with Matchers {

  def test(dir: File, contents: String, testFn: (EnsimeConfig) => Unit): Unit = {
    testFn(EnsimeConfigProtocol.parse(contents))
  }

  describe("ProjectConfigSpec") {

    it("should parse a simple config") {
      withCanonTempDir { dir =>
        (dir / ".ensime_cache").mkdirs()
        (dir / "abc").mkdirs()

        val dirStr = fileToWireString(dir)
        val abc = fileToWireString(dir / "abc")
        val cacheStr = fileToWireString(dir / ".ensime_cache")
        val javaHome = fileToWireString(file(Properties.javaHome))

        test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :java-home $javaHome
 :root-dir $dirStr
 :cache-dir $cacheStr
 :reference-source-roots ()
 :debug-args ("-Dthis=that")
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :depends-on-modules ()
                :target $abc
                :test-target $abc
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
          assert(!config.sourceMode)
          assert(config.debugVMArgs === List("-Dthis=that"))
        })
      }
    }

    it("should parse a minimal config for a binary only project") {
      withCanonTempDir { dir =>
        (dir / ".ensime_cache").mkdirs()
        (dir / "abc").mkdirs()

        val dirStr = fileToWireString(dir)
        val abc = fileToWireString(dir / "abc")
        val cacheStr = fileToWireString(dir / ".ensime_cache")
        val javaHome = fileToWireString(file(Properties.javaHome))

        test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :java-home $javaHome
 :root-dir $dirStr
 :cache-dir $cacheStr
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :targets ($abc))))""", { implicit config =>

          assert(config.name == "project")
          assert(config.scalaVersion == "2.10.4")
          val module1 = config.modules("module1")
          assert(module1.name == "module1")
          assert(module1.dependencies.isEmpty)
          assert(module1.targetDirs.size === 1)
        })
      }
    }

    it("should base class paths on source-mode value") {
      List(true, false) foreach { (sourceMode: Boolean) =>
        withCanonTempDir { dir =>
          (dir / ".ensime_cache").mkdirs()
          (dir / "abc").mkdirs()

          val dirStr = fileToWireString(dir)
          val cacheStr = fileToWireString(dir / ".ensime_cache")
          val abcDirStr = fileToWireString(dir / "abc")
          val javaHome = fileToWireString(file(Properties.javaHome))

          test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :java-home $javaHome
 :root-dir $dirStr
 :cache-dir $cacheStr
 :source-mode ${if (sourceMode) "t" else "nil"}
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :targets ($abcDirStr))))""", { implicit config =>
            assert(config.sourceMode == sourceMode)
            assert(config.runtimeClasspath == Set(dir / "abc"), config)
            assert(config.compileClasspath == (
              if (sourceMode) Set.empty else Set(dir / "abc")
            ))
          })
        }
      }
    }
  }
}
