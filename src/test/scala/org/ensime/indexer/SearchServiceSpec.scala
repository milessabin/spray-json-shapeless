package org.ensime.indexer

import java.io.File
import org.scalatest.FunSpec
import org.scalatest.Matchers
import akka.event.slf4j.SLF4JLogging
import org.ensime.config._
import org.ensime.test.TestUtil._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import pimpathon.file._
import pimpathon.any._

class SearchServiceSpec extends FunSpec with Matchers with SLF4JLogging {

  def genProject(base: File): EnsimeConfig = {
    val config = basicConfig(base,
      sources = true, testSources = true,
      classes = true, testClasses = true
    )
    val module = config.modules.values.head

    // to reduce test time
    val trimmed = module.copy(
      compileJars = Nil,
      testJars = module.testJars filter (_.getName.contains("scalatest_"))
    )
    config.copy(
      modules = Map(trimmed.name -> trimmed)
    )
  }

  val base = tempDir()
  val config = genProject(base)
  val resolver = new SourceResolver(config)
  val service = new SearchService(config, resolver)

  describe("search refreshing") {
    // these tests run in sequence
    def refresh(): (Int, Int) = Await.result(service.refresh(), Duration.Inf)

    it("should parse all files on a prestine structure", SlowTest) {
      val (deleted, indexed) = refresh()
      assert(deleted == 0)
      assert(indexed > 0)
    }

    it("should not refresh files that have not changed", SlowTest) {
      assert(refresh() === (0, 0))
    }

    it("should refresh files that have 'changed'", SlowTest) {
      val now = System.currentTimeMillis()
      for {
        m <- config.modules.values
        r <- m.targets ++ m.testTargets
        f <- r.tree
      } {
        // simulate a full recompile
        f.setLastModified(now)
      }

      val (deleted, indexed) = refresh()
      assert(deleted > 0)
      assert(indexed > 0)
    }

    it("should remove classfiles that have been deleted", SlowTest) {
      val module = config.modules.values.toList.head
      val classfile = module.targets.head / "org/ensime/indexer/SearchService.class"
      assert(classfile.exists)
      classfile.delete()
      assert(refresh() === (1, 0))
    }
  }

  describe("class searching") {
    def search(expect: String, query: String) = {
      val max = 10
      val info = s"'$query' expected '$expect')"
      service.searchClasses(query, max) tap { results =>
        assert(results.size <= max, s"${results.size} $info")
        assert(results.nonEmpty, info)
        // when we improve the search quality, we could
        // make this really look only at #1
        val got = results.map(_.fqn)
        assert(got contains expect, s"$info got '$got'")
      }
    }
    def searches(expect: String, queries: String*) =
      (expect :: queries.toList).foreach(search(expect, _))

    it("should return results from J2SE", SlowTest) {
      searches(
        "java.lang.String",
        "String", "string",
        "j.l.str", "j l str"
      )
    }

    it("should return results from dependencies", SlowTest) {
      searches(
        "org.scalatest.FunSuite",
        "FunSuite", "funsuite", "funsu",
        "o s Fun"
      )
    }

    it("should return results from the project", SlowTest) {
      searches(
        "org.ensime.server.Server",
        "o e s ser"
      )

      searches(
        "org.ensime.server.RichPresentationCompiler",
        "RichPres", "richpres",
        "o e s Rich", "o.e.s.rich",
        "RPC" // <= CamelCaseAwesomeNess
      )
    }
  }

  describe("class, field and method searching") {
    def search(expect: String, query: String) = {
      val max = 10
      val info = s"'$query' expected '$expect')"
      service.searchClassesFieldsMethods(query, max) tap { results =>
        assert(results.size <= max, s"${results.size} $info")
        assert(results.nonEmpty, info)
        // when we improve the search quality, we could
        // make this really look only at #1
        val got = results.map(_.fqn)
        assert(got contains expect, s"$info got '$got'")
      }
    }
    def searches(expect: String, queries: String*) =
      (expect :: queries.toList).foreach(search(expect, _))

    it("should return results from classes", SlowTest) {
      searches(
        "java.lang.String",
        "String", "string",
        "j.l.str", "j l str"
      )
    }

    it("should return results from static fields", SlowTest) {
      searches(
        "java.util.regex.Pattern.CASE_INSENSITIVE",
        "CASE_INSENSITIVE", "case_insensitive",
        "case_"
      )

    }

    it("should return results from instance fields", SlowTest) {
      searches(
        "java.awt.Point.x"
      )
    }

    it("should return results from static methods", SlowTest) {
      searches(
        "java.lang.Runtime.addShutdownHook",
        "addShutdownHook"
      )
    }

    it("should return results from instance methods", SlowTest) {
      searches(
        "java.lang.Runtime.availableProcessors",
        "availableProcessors", "availableP"
      )
    }
  }

}
