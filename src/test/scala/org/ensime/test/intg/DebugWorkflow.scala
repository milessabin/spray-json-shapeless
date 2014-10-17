package org.ensime.test.intg

import org.ensime.test.TestUtil
import org.ensime.util._
import org.scalatest.{ FunSpec, Matchers }
import org.slf4j.LoggerFactory

import scala.concurrent.duration._

class DebugWorkflow extends FunSpec with Matchers {

  val log = LoggerFactory.getLogger(this.getClass)

  describe("Server (debug workflow)") {
    it("should allow debugging of a project") {
      IntgUtil.withTestProject("src/example-simple") { (config, interactor) =>
        val sourceRoot = config.modules.values.head.sourceRoots.head

        val fooFile = TestUtil.fileToWireString(CanonFile(sourceRoot + "/org/example/Foo.scala"))

        interactor.expectRPC(20 seconds, s"""(swank:debug-set-break "org/example/Foo.scala" 14)""", """(:ok t)""")

        interactor.expectRPC(20 seconds, s"""(swank:debug-start "org.example.Foo")""", """(:ok (:status "success"))""")

        interactor.expectAsync(30 seconds, s"""(:debug-event (:type breakpoint :thread-id "1" :thread-name "main" :file $fooFile :line 14))""")
        // ask for stack frames that are well out of scope so we get an empty result
        interactor.expectRPC(20 seconds, s"""(swank:debug-backtrace "1" 50 2)""", """(:ok (:frames () :thread-id "1" :thread-name "main"))""")

        // example detailed stack trace
        //      (:ok
        // (:frames
        //   (
        //     (:index 0 :locals () :num-args 0 :class-name "org.example.Foo$" :method-name "delayedEndpoint$org$example$Foo$1" :pc-location (:file "/private/var/folders/n5/xn63lvd15tnghpm211s72j7h0000gn/T/sbt_6d251965/src/main/scala/org/example/Foo.scala" :line 14) :this-object-id "587")
        //     (:index 1 :locals () :num-args 0 :class-name "org.example.Foo$delayedInit$body" :method-name "apply" :pc-location (:file "/private/var/folders/n5/xn63lvd15tnghpm211s72j7h0000gn/T/sbt_6d251965/src/main/scala/org/example/Foo.scala" :line 3) :this-object-id "588"))
        //  :thread-id "1"
        //  :thread-name "main"))

        interactor.expectRPC(20 seconds, s"""(swank:debug-stop)""", """(:ok t)""")
      }
    }
  }
}
