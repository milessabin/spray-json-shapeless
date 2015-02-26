package org.ensime.core

import org.scalatest._

import akka.actor.{ ActorRef, ActorSystem, Props }
import akka.event.slf4j.SLF4JLogging
import akka.pattern.Patterns
import akka.util.Timeout
import java.io.File
import org.apache.commons.lang.StringEscapeUtils
import org.apache.commons.vfs2._
import org.ensime.config._
import org.ensime.model._
import pimpathon.file._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.internal.util.OffsetPosition
import scalariform.formatter.preferences.FormattingPreferences
import spray.http._
import spray.http.HttpMethods._
import java.net.URLDecoder

import org.ensime.fixture._

import pimpathon.java.io.inputStream._
import pimpathon.java.io.outputStream._
import com.google.common.io.Files

class DocServerSpec extends WordSpec with Matchers with SLF4JLogging
    with IsolatedRichPresentationCompilerFixture {

  // TODO: perhaps we should use a different project
  val original = EnsimeConfigFixture.EnsimeTestProject

  // confusing/bloated type signature, the key of the map is the
  // version of Java. It would be nice to refactor this so that the
  // DocServer is provided by a fixture, without the requirement of
  // the presentation compiler (use companion pattern as in
  // org.ensime.fixture). i.e. split the test into a full
  // integration-level test to test doc-at-point tests, and then test
  // the FQN lookup separately with a cleaner fixture (only needs nested
  // EnsimeConfig and TestKit)
  def withDocServerAndPresentationCompiler(javaVersions: String*)(
    testCode: (Map[String, ActorRef], EnsimeConfig, RichPresentationCompiler) => Any): Any = withRichPresentationCompiler {
    (testkit: TestKitFix, config: EnsimeConfig, pc: RichPresentationCompiler) =>
      import testkit._
      val servers = javaVersions.map { javaVersion =>
        (javaVersion, system.actorOf(Props(new DocServer(config, false, Some(javaVersion)))))
      }.toMap
      testCode(servers, config, pc)
  }

  "DocServer" should {
    "respond with correct URIs" in withDocServerAndPresentationCompiler(
      "1.6", "1.8"
    ) { (servers, config, cc) =>
        val serv = servers("1.6")
        val serv8 = servers("1.8")

        val docs = config.allDocJars.filter { f =>
          List(
            "scalap-",
            "jul-to-slf4j",
            "scala-library-",
            "ForecastIOLib", // Javadoc 1.8
            "guava-"
          ).exists(f.getName.startsWith(_))
        }

        def getUri(serv: ActorRef, sig: DocSig): Option[String] = {
          Await.result(Patterns.ask(serv, DocUriReq(DocSigPair(sig, sig)), Timeout(5.second)),
            Duration.Inf).asInstanceOf[Option[String]]
        }

        def getReponse(serv: ActorRef, path: Uri): HttpResponse = {
          Await.result(Patterns.ask(serv, HttpRequest(GET, path), Timeout(5.second)),
            Duration.Inf).asInstanceOf[HttpResponse]
        }

        def contentPath(uri: Uri): Uri = {
          if (uri.path.toString.endsWith("index.html")) {
            val path = uri.path.toString.replace(
              "index", uri.fragment.flatMap { s => s.split("@").headOption }.getOrElse("").replace(".", "/"))
            Uri(path = Uri.Path(path))
          } else {
            uri.path.toString
          }
        }

        def anchor(uri: Uri, java: Boolean): String = {
          val frag = uri.fragment.getOrElse("")
          if (java) URLDecoder.decode(frag, "UTF-8")
          else frag.split("@").drop(1).mkString
        }

        def findsContentFor(serv: ActorRef, sig: DocSig, java: Boolean): Boolean = {
          getUri(serv, sig).exists { url =>
            val uri = Uri(url)
            if (uri.authority.host.address != "localhost")
              throw new IllegalArgumentException("Refusing to download remote html: " + uri)
            val path = contentPath(uri)
            val response = getReponse(serv, path)
            if (response.status != StatusCode.int2StatusCode(200)) {
              throw new IllegalStateException(s"GET ${path} failed with status: ${response.status}")
            } else {
              val html = response.entity.data.asString
              if (sig.member.isDefined) {
                val attr = if (java) "name" else "id"
                val link = StringEscapeUtils.escapeHtml(anchor(uri, java))
                html.contains(s"""$attr="${link}"""")
              } else {
                html.contains("<html")
              }
            }
          }
        }

        def checkScala(sig: DocSigPair, expectedSig: DocSig) {
          assert(sig.scala == expectedSig)
          assert(findsContentFor(serv, expectedSig, false))
        }

        def checkJava(sig: DocSigPair, expectedSig: DocSig) {
          assert(sig.java == expectedSig)
          if (!sig.java.fqn.javaStdLib) {
            assert(findsContentFor(serv, expectedSig, true))
          }
        }

        import ReallyRichPresentationCompilerFixture._
        runForPositionInCompiledSource(config, cc,
          "package com.example",
          "import com.google.common.io.Files",
          "import java.nio.charset.StandardCharsets",
          "import java.nio.channels.FileChannel._",
          "import java.io.File",
          "class Thing {",
          "  def main(){",
          "    val o = Some(1)",
          "    val nums = o.m@0@ap(_ + 2)",
          "    val b:Boo@0.5@lean = false",
          "    nums.isDe@1@fined",
          "    val nums2 = o.flat@2@Map {i:Int => Some(i + 1)}",
          "    val x = Some(Some(1)).fla@3@tten",
          "    val y = Some(1).fo@4@ld(0) { ea => ea + 2 }",
          "    val z = Some(1).mkS@5@tring(\".\", \".\", \".\")",
          "    val zz = Some(1).mkS@6@tring",
          "    val zzz = Some(1).mkS@7@tring(\".\")",
          "    val q = Some(1).getOr@8@Else(2)",
          "    val r = Some(1).gro@9@uped(2)",
          "    val xx = List.emp@10@ty",
          "    val f = new File(\".\")",
          "    Files.mo@11@ve(f, f)",
          "    Files.asByte@12@Source(f)",
          "    Files.m@13@ap(f, MapMode.PRIVATE)",
          "    Files.m@14@ap(f, MapMode.PRIVATE, 5)",
          "    val a = Array[Byte]()",
          "    Files.wri@15@te(a, f)",
          "    val aa = So@16@me(4)",
          "    val sss = \"abcd@17@efg\"",
          "    val ii = 123@18@456",
          "    val fo@20@x = new File(\".\")",
          "    val c = classOf[File].isInst@21@ance(fox)",
          "    scala.Predef.DummyIm@22@plicit",
          "    val ha@23@sh = new java.util.HashMap[Int, Int]()",
          "    val entries = hash.entry@24@Set()",
          "    val en@25@try = entries.iterator().next()",
          "    import java.ut@26@il.Vector",
          "    import scala.collec@27@tion._",
          "    val thing: Ex@28@ception = null",
          "    val ou = Some(1) +@29@+ List(1, 2)",
          "    List(1, 2).flat@30@Map(Some(_))",
          "    List(1, 2).coll@31@ect { case 1 => 5 }",
          "  }",
          "}") { (p, label, cc) =>
            val sig = cc.askDocSignatureAtPoint(p).get
            label match {
              case "0" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("map[B](f:A=>B):Option[B]")))
              case "0.5" => checkScala(sig, DocSig(DocFqn("scala", "Boolean"), None))
              case "1" => checkScala(sig, DocSig(DocFqn("scala", "Option"), Some("isDefined:Boolean")))
              case "2" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("flatMap[B](f:A=>Option[B]):Option[B]")))
              case "3" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("flatten[B](implicitev:<:<[A,Option[B]]):Option[B]")))
              case "4" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("fold[B](ifEmpty:=>B)(f:A=>B):B")))
              case "5" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("mkString(start:String,sep:String,end:String):String")))
              case "6" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("mkString:String")))
              case "7" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("mkString(sep:String):String")))
              case "8" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("getOrElse[B>:A](default:=>B):B")))
              case "9" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("grouped(size:Int):Iterator[Repr]")))
              case "10" => checkScala(sig, DocSig(DocFqn("scala.collection.immutable", "List$"), Some("empty[A]:List[A]")))
              case "11" => checkJava(sig, DocSig(DocFqn("com.google.common.io", "Files"), Some("move(java.io.File, java.io.File)")))
              case "12" => checkJava(sig, DocSig(DocFqn("com.google.common.io", "Files"), Some("asByteSource(java.io.File)")))
              case "13" => checkJava(sig, DocSig(DocFqn("com.google.common.io", "Files"), Some("map(java.io.File, java.nio.channels.FileChannel.MapMode)")))
              case "14" => checkJava(sig, DocSig(DocFqn("com.google.common.io", "Files"), Some("map(java.io.File, java.nio.channels.FileChannel.MapMode, long)")))
              case "15" => checkJava(sig, DocSig(DocFqn("com.google.common.io", "Files"), Some("write(byte[], java.io.File)")))
              // TODO(fix this hack) - just goes to the class itself if companion
              // constructor is requested.
              case "16" => checkJava(sig, DocSig(DocFqn("scala", "Some"), None))
              case "17" => checkJava(sig, DocSig(DocFqn("java.lang", "String"), None))
              case "18" => checkScala(sig, DocSig(DocFqn("scala", "Int"), None))
              case "20" => checkJava(sig, DocSig(DocFqn("java.io", "File"), None))
              case "21" => checkJava(sig, DocSig(DocFqn("java.lang", "Class"), Some("isInstance(java.lang.Object)")))
              case "22" => checkScala(sig, DocSig(DocFqn("scala", "Predef$$DummyImplicit$"), None))
              case "23" => checkJava(sig, DocSig(DocFqn("java.util", "HashMap"), None))
              case "24" => checkJava(sig, DocSig(DocFqn("java.util", "HashMap"), Some("entrySet()")))
              case "25" => checkJava(sig, DocSig(DocFqn("java.util", "Map.Entry"), None))
              case "26" => checkJava(sig, DocSig(DocFqn("java.util", "package"), None))
              case "27" => checkJava(sig, DocSig(DocFqn("scala.collection", "package"), None))
              // TODO: Would be nice to be able to inspect a particular constructor. The problem is that
              // symbolAt returns the type itself when point is in 'File', and it's not totally clear
              // that's wrong.
              //            case "28" => checkJava(sig, DocSig("java.io.File", Some("File(java.lang.String, java.lang.String)"))
              case "28" => checkScala(sig, DocSig(DocFqn("scala", "package"), Some("Exception=Exception")))

              // Check @usecase handling.
              case "29" => checkScala(sig, DocSig(DocFqn("scala", "Some"), Some("++[B>:A,That](that:scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[Repr,B,That]):That")))
              case "30" => checkScala(sig, DocSig(DocFqn("scala.collection.immutable", "List"), Some("flatMap[B,That](f:A=>scala.collection.GenTraversableOnce[B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That")))
              case "31" => checkScala(sig, DocSig(DocFqn("scala.collection.immutable", "List"), Some("collect[B,That](pf:PartialFunction[A,B])(implicitbf:scala.collection.generic.CanBuildFrom[List[A],B,That]):That")))
            }
            checkJava(cc.askDocSignatureForSymbol("com.google.common.io.Files$", Some("simplifyPath"), None).get,
              DocSig(DocFqn("com.google.common.io", "Files"), Some("simplifyPath(java.lang.String)")))
          }

        val libdoc = docs.find { f => f.getName.startsWith("scala-library") }.get
        val java8doc = docs.find { f => f.getName.startsWith("ForecastIOLib") }.get
        assert(getUri(serv, DocSig(DocFqn("scala", "Some"), None)) ==
          Some("http://localhost:0/" + libdoc.getName + "/index.html#scala.Some"))
        assert(getUri(serv, DocSig(DocFqn("scala", "Some"), Some("mkString(sep:String):String"))) ==
          Some("http://localhost:0/" + libdoc.getName + "/index.html#scala.Some@mkString%28sep%3AString%29%3AString"))
        assert(getUri(serv, DocSig(DocFqn("java.io", "File"), None)) ==
          Some("http://docs.oracle.com/javase/6/docs/api/java/io/File.html"))
        assert(getUri(serv, DocSig(DocFqn("java.util", "Map.Entry"), None)) ==
          Some("http://docs.oracle.com/javase/6/docs/api/java/util/Map.Entry.html"))
        assert(getUri(serv, DocSig(DocFqn("java.util", "package"), None)) ==
          Some("http://docs.oracle.com/javase/6/docs/api/java/util/package-summary.html"))
        assert(getUri(serv, DocSig(DocFqn("scala.collection.immutable", "package"), None)).getOrElse("")
          .endsWith("/index.html#scala.collection.immutable.package"))

        assert(getUri(serv, DocSig(DocFqn("com.github.dvdme.ForecastIOLib", "ForecastIO"), Some("getForecast(com.eclipsesource.json.JsonObject)"))) ==
          Some("http://localhost:0/" + java8doc.getName +
            "/com/github/dvdme/ForecastIOLib/ForecastIO.html#getForecast-com.eclipsesource.json.JsonObject-"))

        assert(getUri(serv8, DocSig(DocFqn("java.io", "File"), Some("delete()"))) ==
          Some("http://docs.oracle.com/javase/8/docs/api/java/io/File.html#delete--"))
        assert(getUri(serv8, DocSig(DocFqn("java.lang", "Math"), Some("max(int, int)"))) ==
          Some("http://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#max-int-int-"))
        assert(getUri(serv8, DocSig(DocFqn("java.util", "Arrays"), Some("binarySearch(int[], int)"))) ==
          Some("http://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#binarySearch-int%3AA-int-"))

      }
  }
}
