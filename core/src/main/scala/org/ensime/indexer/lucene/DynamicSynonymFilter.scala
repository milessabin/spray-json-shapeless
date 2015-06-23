package org.ensime.indexer.lucene

import java.io.Reader

import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.{ Analyzer, TokenFilter, TokenStream }
import org.apache.lucene.analysis.core.KeywordTokenizer
import org.apache.lucene.analysis.tokenattributes.{ CharTermAttribute, PositionIncrementAttribute }
import org.apache.lucene.util.AttributeSource.State
import org.ensime.indexer.lucene.DynamicSynonymFilter._

import scala.collection.mutable

/**
 * `Analyzer` that does no additional (not even lowercasing) other than
 * the term itself and its synonyms.
 */
trait DynamicSynonymAnalyzer extends Analyzer with SynonymEngine {
  override final def createComponents(fieldName: String, reader: Reader) = {
    val source = new KeywordTokenizer(reader)
    val result = new DynamicSynonymFilter(source, this)
    new TokenStreamComponents(source, result)
  }
}

object DynamicSynonymFilter {
  trait SynonymEngine {
    /** @return the synonyms of `term` (`term` should not be in the list) */
    def synonyms(term: String): Set[String]
  }
}

/**
 * Splits tokens into synonyms at the same position, taking in a
 * simple map from a String to a list of its synonyms (which doesn't
 * need to contain the original token).
 *
 * This has been heavily influenced by SynonymFilter from "Lucene in
 * Action" and upgraded for Lucene 4 because bundled
 * ```org.apache.lucene.analysis.synonym.SynonymFilter``` requires the
 * mappings to be built up in advance.
 *
 * Apologies for all the mutable state: we're interacting with a
 * mutable Java API.
 */
class DynamicSynonymFilter(input: TokenStream, engine: SynonymEngine) extends TokenFilter(input) {
  private val termAtt = addAttribute(classOf[CharTermAttribute])
  private val posIncrAtt = addAttribute(classOf[PositionIncrementAttribute])

  private val stack: mutable.Stack[String] = mutable.Stack()
  private var current: State = _

  // return false when EOL
  override def incrementToken(): Boolean = {
    if (stack.nonEmpty) {
      val synonym = stack.pop()
      restoreState(current) // brings us back to the original token in case of multiple synonyms
      termAtt.setEmpty()
      termAtt.append(synonym)
      posIncrAtt.setPositionIncrement(0)
      return true
    }

    if (!input.incrementToken())
      return false

    val term = termAtt.toString
    val synonyms = engine.synonyms(term)
    if (synonyms.nonEmpty) {
      synonyms foreach { synonym =>
        if (!synonym.equals(term))
          stack.push(synonym)
      }
      current = captureState()
    }
    true
  }

  // Lucene being stupid higher up the hierarchy
  override def equals(other: Any): Boolean = other match {
    case that: DynamicSynonymFilter => this eq that
    case _ => false
  }
}
