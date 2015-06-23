package org.ensime.config

import org.scalatest._

import org.ensime.sexp._
import org.ensime.sexp.formats._

import scalariform.formatter.preferences._

class ScalariformFormatSpec extends FlatSpec {

  object ScalariformProtocol extends DefaultSexpProtocol with ScalariformFormat
  import ScalariformProtocol._

  val prefs = FormattingPreferences().
    setPreference(DoubleIndentClassDeclaration, true).
    setPreference(IndentSpaces, 13)

  "ScalariformFormat" should "parse some example config" in {
    val text = """(:doubleIndentClassDeclaration t
                     :indentSpaces 13)"""
    val recover = text.parseSexp.convertTo[FormattingPreferences]
    assert(recover.preferencesMap == prefs.preferencesMap)
  }

  it should "create valid output" in {
    assert(prefs.toSexp === SexpList(
      SexpSymbol(":doubleIndentClassDeclaration"), SexpSymbol("t"),
      SexpSymbol(":indentSpaces"), SexpNumber(13)
    ))
  }
}
