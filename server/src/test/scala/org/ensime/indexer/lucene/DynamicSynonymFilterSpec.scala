package org.ensime.indexer.lucene

import java.io.StringReader
import org.apache.lucene.analysis.core.KeywordTokenizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.scalatest.FunSpec
import org.scalatest.Matchers
import DynamicSynonymFilter._
import scala.collection.mutable

class DynamicSynonymFilterSpec extends FunSpec with Matchers {

  val cleese = Set(
    "resting", "stunned", "deceased", "passed on",
    "no more", "ceased to be", "expired and gone to meet his maker",
    "stiff", "bereft of life", "rests in peace", "pushing up the daisies",
    "metabolic processes are history", "off the twig", "kicked the bucket",
    "shuffled off his mortal coil",
    "run down the curtain and joined the bleedin choir invisible",
    "ex-parrot"
  )
  val engine = new SynonymEngine {
    def synonyms(term: String) =
      if (term != "dead") Set.empty
      else cleese
  }

  private def applyEngineToTerm(term: String, engine: SynonymEngine): List[String] = {
    val reader = new StringReader(term)
    val source = new KeywordTokenizer(reader)
    val filter = new DynamicSynonymFilter(source, engine)

    val words: mutable.ListBuffer[String] = mutable.ListBuffer()
    filter.reset()
    while (filter.incrementToken()) {
      words += source.getAttribute(classOf[CharTermAttribute]).toString
    }
    filter.close()
    words.toList.sorted
  }

  describe("DynamicSynonymFilter") {
    it("should not add synonyms where there are none") {
      val term = "Norwegian Blue"
      assert(applyEngineToTerm(term, engine) === List(term))
    }

    it("should report known synonyms") {
      val term = "dead"
      assert(applyEngineToTerm(term, engine) === (cleese + term).toList.sorted)
    }
  }
}
