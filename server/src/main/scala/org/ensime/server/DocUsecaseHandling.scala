package org.ensime.server

import java.io.{ File, IOException }
import java.util.jar.JarFile
import java.util.regex.Pattern

import org.apache.commons.lang.StringEscapeUtils

import org.ensime.api._
import org.ensime.core._

import scala.io.Source

// Scaladoc uses @usecase comment annotations to substitute kid-safe signatures
// in the docs.
// See: http://stackoverflow.com/questions/26132459/why-are-scaladoc-method-signatures-wrong
// Unfortunately, these usecase signatures are also used for the link anchors.
// As a result, it's impossible to generate the links without doing non-trivial
// analysis of the doc comments.
// Also see: https://issues.scala-lang.org/browse/SI-9168
//
// Instead of doing things the 'right' way, which would be hard, we use heuristics
// to determine if the link is likely to be a @usecase, and then go digging
// through the html content to find the anchor.
trait DocUsecaseHandling { self: DocServer =>

  val PrefixRegexp = """^([A-Za-z:_\+-]+).*""".r
  protected def maybeReplaceWithUsecase(jar: File, sig: DocSig): DocSig = {
    if (sig.fqn.scalaStdLib) {
      sig.member match {
        case Some(PrefixRegexp(prefix)) if UseCasePrefixes.contains(prefix) =>
          try {
            val jarFile = new JarFile(jar)
            val is = jarFile.getInputStream(jarFile.getEntry(scalaFqnToPath(sig.fqn)))
            val html = Source.fromInputStream(is).mkString
            val re = s"""<a id="(${Pattern.quote(prefix)}.+?)"""".r
            re.findFirstMatchIn(html).map {
              m => sig.copy(member = Some(StringEscapeUtils.unescapeHtml(m.group(1))))
            }.getOrElse(sig)
          } catch { case e: IOException => sig }
        case _ => sig
      }
    } else sig
  }

  private val UseCasePrefixes = Set(
    "+",
    "+",
    "++",
    "++:",
    "+:",
    "-",
    ":+",
    "::",
    ":::",
    "collect",
    "copyToArray",
    "diff",
    "filterMap",
    "flatMap",
    "flatten",
    "foreach",
    "getOrElse",
    "indexOf",
    "intersect",
    "lastIndexOf",
    "map",
    "mapConserve",
    "max",
    "maxBy",
    "min",
    "minBy",
    "padTo",
    "patch",
    "product",
    "reverseMap",
    "reverse_:::",
    "sameElements",
    "scan",
    "sum",
    "to",
    "toArray",
    "toMap",
    "union",
    "updated",
    "zip",
    "zipAll",
    "zipWithIndex"
  )

}
