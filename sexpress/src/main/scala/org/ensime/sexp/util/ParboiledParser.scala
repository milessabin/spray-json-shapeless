package org.ensime.sexp.util

import org.parboiled.scala._
import org.parboiled.errors.{ ErrorUtils, ParsingException }

trait ThreadLocalSupport {
  protected def local[T](t: => T) = new ThreadLocal[T] {
    override def initialValue = t
  }
}

trait ParboiledParser[T] extends Parser with ThreadLocalSupport {

  override val buildParseTree = true

  // always re-use the parser
  // http://users.parboiled.org/Scala-performance-td4024217.html
  private lazy val Runner = local(ReportingParseRunner(Top))

  def parse(desc: String): T = {
    val parsingResult = Runner.get.run(desc)
    parsingResult.result.getOrElse {
      throw new ParsingException(
        "Invalid :\n" + ErrorUtils.printParseErrors(parsingResult)
      )
    }
  }

  protected implicit class RulePimp(val rule: Rule0) {
    def save: Rule1[String] = { rule ~> (_.toString) }
    def as[P](t: P): Rule1[P] = { rule ~> (_ => t) }
  }

  protected def Top: Rule1[T]
}
