package org.ensime.sexp.formats

import BigIntConvertor._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.collection.immutable.BitSet

class BigIntConvertorSpec extends FunSpec {
  private val examples = List(
    BitSet() -> BigInt(0),
    BitSet(0) -> BigInt(1),
    BitSet(1) -> BigInt(2),
    BitSet(64) -> BigInt("18446744073709551616"),
    BitSet(0, 64) -> BigInt("18446744073709551617"),
    BitSet(1, 64) -> BigInt("18446744073709551618")
  )

  it("should convert basic BigSet to BitInt") {
    examples foreach {
      case (bitset, bigint) => assert(fromBitSet(bitset) === bigint)
    }
  }

  it("should convert basic BigInt to BitSet") {
    examples foreach {
      case (bitset, bigint) => assert(toBitSet(bigint) === bitset)
    }
  }
}

class BigIntConvertorCheck extends FunSpec with GeneratorDrivenPropertyChecks {

  def positiveIntStream: Arbitrary[Stream[Int]] = Arbitrary {
    Gen.containerOf[Stream, Int](Gen.chooseNum(0, 2 * Short.MaxValue))
  }

  implicit def arbitraryBitSet: Arbitrary[BitSet] = Arbitrary {
    for (seq <- positiveIntStream.arbitrary) yield BitSet(seq: _*)
  }

  it("should round-trip BigInt <=> BitSet") {
    forAll { (bigint: BigInt) =>
      whenever(bigint >= 0) {
        // the exact rules for which negative numbers are allowed
        // seems to be quite complex, but certainly it is sometimes
        // valid.
        assert(fromBitSet(toBitSet(bigint)) === bigint)
      }
    }
  }

  it("should round-trip BitSet <=> BigInt") {
    forAll { (bitset: BitSet) =>
      assert(toBitSet(fromBitSet(bitset)) === bitset)
    }
  }
}
