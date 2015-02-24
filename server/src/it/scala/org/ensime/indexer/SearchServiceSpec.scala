package org.ensime.indexer

import org.scalatest._
import org.ensime.fixture._

import org.ensime.config._
import scala.concurrent._
import scala.concurrent.duration._
import pimpathon.file._
import pimpathon.any._

class SearchServiceSpec extends WordSpec with Matchers
    with SharedSearchServiceFixture with SearchServiceTestUtils {

  def original = EnsimeConfigFixture.SimpleTestProject

  "search refreshing" should {
    "parse all files on a prestine structure" in withSearchService { implicit service =>
      val (deleted, indexed) = refresh()
      deleted shouldBe 0
      indexed should be > 0
    }

    "not refresh files that have not changed" in withSearchService { implicit service =>
      refresh() shouldBe (0, 0)
    }

    "refresh files that have 'changed'" in withSearchService { (config, service) =>
      implicit val s = service
      val now = System.currentTimeMillis()
      for {
        m <- config.modules.values
        r <- m.targetDirs ++ m.testTargetDirs
        f <- r.tree
      } {
        // simulate a full recompile
        f.setLastModified(now)
      }

      val (deleted, indexed) = refresh()
      deleted should be > 0
      indexed should be > 0
    }

    "remove classfiles that have been deleted" in withSearchService { (config, service) =>
      implicit val s = service
      val classfile = config.subprojects.head.targetDirs.head / "org/example/Foo.class"

      classfile shouldBe 'exists

      classfile.delete()
      refresh() shouldBe (1, 0)
    }
  }

  "class searching" should {
    "return results from J2SE" in withSearchService { implicit service =>
      searchesClasses(
        "java.lang.String",
        "String", "string",
        "j.l.str", "j l str"
      )
    }

    "return results from dependencies" in withSearchService { implicit service =>
      searchesClasses(
        "org.scalatest.FunSuite",
        "FunSuite", "funsuite", "funsu",
        "o s Fun"
      )
    }

    "return results from the project" in withSearchService { implicit service =>
      searchesClasses(
        "org.example.Bloo",
        "o e bloo"
      )

      searchesClasses(
        "org.example.CaseClassWithCamelCaseName",
        "CaseClassWith", "caseclasswith",
        "o e Case", "o.e.caseclasswith",
        "CCWC" // <= CamelCaseAwesomeNess
      )
    }
  }

  "class and method searching" should {
    "return results from classes" in withSearchService { implicit service =>
      searchesClassesAndMethods(
        "java.lang.String",
        "String", "string",
        "j.l.str", "j l str"
      )
    }

    "return results from static fields" in withSearchService { implicit service =>
      searchesEmpty(
        "CASE_INSENSITIVE", "case_insensitive",
        "case_"
      )
    }

    "not return results from instance fields" in withSearchService { implicit service =>
      searchesEmpty(
        "java.awt.Point.x"
      )
    }

    "return results from static methods" in withSearchService { implicit service =>
      searchesClassesAndMethods(
        "java.lang.Runtime.addShutdownHook",
        "addShutdownHook"
      )
    }

    "return results from instance methods" in withSearchService { implicit service =>
      searchesClassesAndMethods(
        "java.lang.Runtime.availableProcessors",
        "availableProcessors", "availableP"
      )
    }
  }

  "exact searches" should {
    "find type aliases" in withSearchService { implicit service =>
      assert(service.findUnique("org.scalatest.fixture.ConfigMapFixture$FixtureParam").isDefined)
    }
  }
}

trait SearchServiceTestUtils {
  def refresh()(implicit service: SearchService): (Int, Int) =
    Await.result(service.refresh(), Duration.Inf)

  def searchClasses(expect: String, query: String)(implicit service: SearchService) = {
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

  def searchesClasses(expect: String, queries: String*)(implicit service: SearchService) =
    (expect :: queries.toList).foreach(searchClasses(expect, _))

  def searchClassesAndMethods(expect: String, query: String)(implicit service: SearchService) = {
    val max = 10
    val info = s"'$query' expected '$expect')"
    service.searchClassesMethods(List(query), max) tap { results =>
      assert(results.size <= max, s"${results.size} $info")
      assert(results.nonEmpty, info)
      // when we improve the search quality, we could
      // make this really look only at #1
      val got = results.map(_.fqn)
      assert(got contains expect, s"$info got '$got'")
    }
  }

  def searchExpectEmpty(query: String)(implicit service: SearchService) = {
    val max = 1
    service.searchClassesMethods(List(query), max) tap { results =>
      assert(results.isEmpty, "expected empty results from %s".format(query))
    }
  }

  def searchesEmpty(queries: String*)(implicit service: SearchService) =
    queries.toList.foreach(searchExpectEmpty)

  def searchesClassesAndMethods(expect: String, queries: String*)(implicit service: SearchService) =
    (expect :: queries.toList).foreach(searchClassesAndMethods(expect, _))

}
