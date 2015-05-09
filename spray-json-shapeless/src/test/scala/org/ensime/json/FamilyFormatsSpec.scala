package org.ensime.json

import org.scalatest._
import spray.json._
import shapeless._
import java.util.UUID

// Example domain models used in the tests. Note that the domain model
// and formatters are defined in sibling packages.
package examples {
  sealed trait SimpleTrait
  case class Foo(s: String) extends SimpleTrait
  case class Bar() extends SimpleTrait
  case object Baz extends SimpleTrait
  case class Faz(o: Option[String]) extends SimpleTrait

  sealed trait SubTrait extends SimpleTrait
  case object Fuzz extends SubTrait

  sealed trait Spiel
  case object Buzz extends Spiel

  case class Schpugel(v: String) // I asked my wife to make up a word
  case class Smim(v: String) // I should stop asking my wife to make up words

  sealed trait StringEnum {
    def label: String
  }
  case class Flooma(label: String) extends StringEnum // to be fair, that was mine
  case class Blam(label: String) extends StringEnum

  sealed trait Cloda
  case class Plooba(thing: String) extends Cloda // *sigh*

  object Quack
  case class Huey(duck: Quack.type, witch: Option[Quack.type])
  case class Dewey(duck: Quack.type, witch: Option[Quack.type])
  case class Louie(duck: Quack.type, witch: Option[Quack.type])

  // I love monkeys, you got a problem with that?
  sealed trait Primates
  sealed trait Strepsirrhini extends Primates
  sealed trait Haplorhini extends Primates
  sealed trait Tarsiiformes extends Haplorhini
  case object Tarsiidae extends Tarsiiformes
  sealed trait Simiiformes extends Haplorhini
  sealed trait Platyrrhini extends Simiiformes
  case object Callitrichidae extends Platyrrhini
  case object Cebidae extends Platyrrhini
  case object Aotidae extends Platyrrhini
  case object Pitheciidae extends Platyrrhini
  case object Atelidae extends Platyrrhini
  sealed trait Catarrhini extends Simiiformes
  sealed trait Cercopithecoidea extends Catarrhini
  case object Cercopithecidae extends Cercopithecoidea
  sealed trait Hominoidea extends Catarrhini
  case object Hylobatidae extends Hominoidea
  case class Hominidae(id: UUID) extends Hominoidea

}
object ExamplesFormats extends DefaultJsonProtocol with FamilyFormats {
  import examples._

  // wtf?? why is this needed, why does it even work? Miles??
  implicit val symbolFormat = SymbolJsonFormat

  ///////////////////////////////////////////////
  // Example of "explicit implicit" for performance
  implicit val SimpleTraitFormat: RootJsonFormat[SimpleTrait] = cachedImplicit

  ///////////////////////////////////////////////
  // user-defined hinting
  implicit object SubTraitHint extends FlatCoproductHint[SubTrait]("hint")
  implicit object SpielHint extends NestedCoproductHint[Spiel]

  ///////////////////////////////////////////////
  // user-defined field naming rules
  implicit object ClodaHint extends FlatCoproductHint[Cloda]("TYPE") {
    override def fieldName(orig: String): String = orig.toUpperCase
  }
  implicit object PloobaHint extends ProductHint[Plooba] {
    override def fieldName[K <: Symbol](k: K): String = k.name.toUpperCase
  }

  ///////////////////////////////////////////////
  // user-defined /missing value rules
  implicit object HueyHint extends ProductHint[Huey] {
    override def nulls = AlwaysJsNull
  }
  implicit object DeweyHint extends ProductHint[Dewey] {
    override def nulls = JsNullNotNone
  }
  implicit object LouieHint extends ProductHint[Louie] {
    override def nulls = NeverJsNull
  }
  implicit object QuackFormat extends JsonFormat[Quack.type] {
    // needed something that would serialise to JsNull for testing
    def read(j: JsValue): Quack.type = j match {
      case JsNull => Quack
      case other => deserializationError(s"unexpected $other")
    }
    def write(q: Quack.type): JsValue = JsNull
  }

  ///////////////////////////////////////////////
  // user-defined JsonFormat
  implicit object SchpugelFormat extends JsonFormat[Schpugel] {
    def read(j: JsValue): Schpugel = j match {
      case JsString(v) => Schpugel(v)
      case other => deserializationError(s"unexpected $other")
    }
    def write(s: Schpugel): JsValue = JsString(s.v)
  }

  ///////////////////////////////////////////////
  // user-defined RootJsonFormat
  implicit object SmimFormat extends RootJsonFormat[Smim] {
    def read(j: JsValue): Smim = j match {
      case JsObject(els) if els.contains("smim") =>
        els("smim") match {
          case JsString(v) => Smim(v)
          case other => deserializationError(s"unexpected $other")
        }
      case other => deserializationError(s"unexpected $other")
    }
    def write(s: Smim): JsValue = JsObject("smim" -> JsString(s.v))
  }

  ///////////////////////////////////////////////
  // non-trivial user-defined JsonFormat
  implicit def stringEnumFormat[T <: StringEnum](
    implicit
    g: Generic.Aux[T, String :: HNil],
    tpe: Typeable[T]
  ): JsonFormat[T] = new JsonFormat[T] {
    def read(json: JsValue): T = json match {
      case JsString(value) => g.from(value :: HNil)
      case _ => deserializationError("expected JsString, got " + json)
    }
    def write(obj: T): JsValue = JsString(obj.label)
  }
}

class FamilyFormatsSpec extends FlatSpec with Matchers
    with SprayJsonTestSupport {
  import examples._
  import ExamplesFormats._

  "FamilyFormats" should "support case objects" in {
    roundtrip(Baz, "{}")
  }

  it should "support symbols, provided by base spray-json" in {
    // any use of Symbol seems to need the hack above. Known caveat.
    roundtrip('foo, """"foo"""")
  }

  it should "support case classes" in {
    roundtrip(Foo("foo"), """{"s":"foo"}""")
    roundtrip(Bar(), "{}")
  }

  it should "support optional parameters on case classes" in {
    roundtrip(Faz(Some("meh")), """{"o":"meh"}""") // note uses optionFormat, not familyFormat
    roundtrip(Faz(None), "{}") // should be omitted, not "null"
  }

  it should "fail when missing required fields" in {
    intercept[DeserializationException] {
      """{}""".parseJson.convertTo[Foo]
    }
  }

  it should "support simple sealed families" in {
    roundtrip(Foo("foo"): SimpleTrait, """{"type":"Foo","s":"foo"}""")
    roundtrip(Bar(): SimpleTrait, """{"type":"Bar"}""")
    roundtrip(Baz: SimpleTrait, """{"type":"Baz"}""")
    roundtrip(Fuzz: SimpleTrait, """{"type":"Fuzz"}""")
  }

  it should "fail when missing required coproduct disambiguators" in {
    intercept[DeserializationException] {
      """{"s":"foo"}""".parseJson.convertTo[SimpleTrait]
    }
  }

  it should "support custom coproduct keys" in {
    roundtrip(Fuzz: SubTrait, """{"hint":"Fuzz"}""")
    roundtrip(Buzz: Spiel, """{"Buzz":{}}""")
  }

  it should "support custom coproduct field naming rules" in {
    roundtrip(Plooba("poo"): Cloda, """{"TYPE":"PLOOBA","THING":"poo"}""")
  }

  it should "support custom product field naming rules" in {
    roundtrip(Plooba("poo"), """{"THING":"poo"}""")
  }

  it should "support custom missing value rules" in {
    roundtrip(Huey(Quack, None), """{"duck":null,"witch":null}""")
    roundtrip(Dewey(Quack, None), """{"duck":null}""")
    roundtrip(Louie(Quack, None), """{}""")

    val json = """{"duck":null,"witch":null}""".parseJson
    json.convertTo[Huey] shouldBe Huey(Quack, None)
    json.convertTo[Dewey] shouldBe Dewey(Quack, Some(Quack))
    json.convertTo[Louie] shouldBe Louie(Quack, None)
  }

  it should "fail when missing required (null) values" in {
    val noduck = """{"witch":null}""".parseJson
    val nowitch = """{"duck":null}""".parseJson

    intercept[DeserializationException] {
      noduck.convertTo[Huey]
    }
    intercept[DeserializationException] {
      noduck.convertTo[Dewey]
    }
    noduck.convertTo[Louie] shouldBe Louie(Quack, None)

    intercept[DeserializationException] {
      nowitch.convertTo[Huey]
    }
    nowitch.convertTo[Dewey] shouldBe Dewey(Quack, None)
    nowitch.convertTo[Louie] shouldBe Louie(Quack, None)
  }

  it should "prefer user customisable JsonFormats" in {
    roundtrip(Schpugel("foo"), """"foo"""")
  }

  it should "prefer user customisable RootJsonFormats" in {
    roundtrip(Smim("foo"), """{"smim":"foo"}""")
  }

  it should "prefer non-trivial user customisable JsonFormats" in {
    roundtrip(Flooma("aha"), """"aha"""")

    // This might surprise you: the user's custom formatter for the
    // classes in this family is a `JsonFormat` (not a
    // `RootJsonFormat`), so when we serialise the trait, it is forced
    // to fall back to the derived product rule, not the custom one,
    // because it expects a `RootJsonFormat`. Funky.
    roundtrip(Flooma("aha"): StringEnum, """{"type":"Flooma","label":"aha"}""")
  }

  it should "fail to compile when a member of the family cannot be serialised" in {
    // this is an example of when this library can be very
    // frustrating. The compiler error when an implicit cannot be
    // created is always the least specific type. Here we're missing a
    // formatter for UUIDs but the compiler warns about Primates. If
    // we narrow it down to Hominidae it also errors... but finding
    // these problems is a human driven search game.

    shapeless.test.illTyped(
      """roundtrip(Hominidae(UUID.randomUUID): Primates)""",
      ".*could not find implicit value for evidence parameter of type spray.json.JsonFormat\\[org.ensime.json.examples.Primates\\].*"
    )

    shapeless.test.illTyped(
      """roundtrip(Hominidae(UUID.randomUUID))""",
      ".*could not find implicit value for evidence parameter of type spray.json.JsonFormat\\[org.ensime.json.examples.Hominidae\\].*"
    )
  }

  ///////////////////////////////////////////////
  // non-trivial AST (in separate file)
  it should "support an example AST" in {
    import ExampleAst._

    roundtrip(SpecialToken: TokenTree, """{"type":"SpecialToken"}""")

    val fieldTerm = FieldTerm("thing is ten", DatabaseField("THING"), "10")
    roundtrip(fieldTerm: TokenTree, """{"type":"FieldTerm","text":"thing is ten","field":{"column":"THING"},"value":"10"}""")

    val and = AndCondition(fieldTerm, fieldTerm, "wibble")
    roundtrip(and: TokenTree, """{"type":"AndCondition","left":{"type":"FieldTerm","text":"thing is ten","field":{"column":"THING"},"value":"10"},"right":{"type":"FieldTerm","text":"thing is ten","field":{"column":"THING"},"value":"10"},"text":"wibble"}""")
  }
}
