package org.ensime.config

import org.ensime.server.Server
import org.ensime.test.TestUtil
import org.scalatest.{ Matchers, FunSpec }

import java.io.{ PrintWriter, File }

class EnsimeConfigSpec extends FunSpec with Matchers {

  def writeToFile(contents: String, file: File): Unit = {
    val out = new PrintWriter(file, "UTF-8")
    try {
      out.print(contents)
    } finally { out.close() }
  }

  def test(contents: String, testFn: (EnsimeConfig) => Unit): Unit = {
    TestUtil.withTemporaryDirectory { dir =>
      val ensimeFile = new File(dir, ".ensime")
      writeToFile(contents, ensimeFile)
      val config = Server.readEnsimeConfig(ensimeFile, "test", dir, new File(".ensime_cache)"))
      testFn(config)
    }
  }

  describe("ProjectConfigSpec") {

    it("should parse a simple config") {
      test(
        """(
          |:name "project"
          |:scala-version "2.10.4"
          |:reference-source-roots ()
          |:subprojects
          |(
          |  (
          |    :name "module1"
          |    :scala-version "2.10.4"
          |    :depends-on-modules ()
          |    :target "abc"
          |    :test-target "abc"
          |    :source-roots ()
          |    :reference-source-roots ()
          |    :compiler-args ()
          |    :runtime-deps ()
          |    :test-deps ()
          |  )
          |))""".stripMargin, { config =>

          assert(config.name == "project")
          assert(config.scalaVersion == "2.10.4")
          println(config.modules)
          val module1 = config.getModule("module1")
          assert(module1.name == "module1")
          assert(module1.dependsOnModules.isEmpty)
        })
    }
  }
}
