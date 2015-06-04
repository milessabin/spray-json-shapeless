package org.ensime.json

import org.slf4j.LoggerFactory
import scala.collection.immutable.ListMap

import spray.json._

import shapeless._, labelled.{ field, FieldType }, syntax.singleton._

/**
 * Automatically create product/coproduct marshallers (i.e. families
 * of sealed traits and case classes/objects) for spray-json.
 *
 * Shapeless allows us to view sealed traits as "co-products" (aka
 * `CList`s) and to view case classes / objects as "products" (aka
 * `HList`s).
 *
 * Here we write marshallers for `HList`s and `CList`s and a converter
 * to/from the generic form.
 *
 * =Customisation=
 *
 * Users may provide an implicit `CoproductHint[T]` for their sealed
 * traits, allowing the disambiguation scheme to be customised.
 * Some variants are provided to cater for common needs.
 *
 * Users may also provide an implicit `ProductHint[T]` for their case
 * classes, allowing wire format field names to be customised and to
 * specialise the handling of `JsNull` entries. By default, `None`
 * optional parameters are omitted and any formatter that outputs
 * `JsNull` will be respected.
 *
 * =Performance=
 *
 * TL;DR these things are bloody fast, don't even think about runtime
 * performance concerns unless you have hard proof that these
 * serialisers are a bottleneck.
 *
 * However, **compilation times** may be negatively impacted. Many of
 * the problems are fundamental performance issues in the Scala
 * compiler. In particular, compile times appear to be quadratic with
 * respect to the number of case classes/objects in a sealed family,
 * with compile times becoming unbearable for 30+ implementations of a
 * sealed trait. It may be wise to use encapsulation to reduce the
 * number of implementations of a sealed trait if this becomes a
 * problem. For more information see
 * https://github.com/milessabin/shapeless/issues/381
 *
 * Benchmarking has shown that these formats are within 5% of the
 * performance of hand-crafted spray-json family format serialisers.
 * The extra overhead can be explained by the conversion to/from
 * `LabelledGeneric`, and administration around custom type hinting.
 * These are all just churn of small objects - not computational - and
 * is nothing compared to the performance bottleneck introduced by
 * using an `Either` (which uses exception throwing for control flow),
 * e.g. https://github.com/spray/spray-json/issues/133 or how `Option`
 * fields used to be handled
 * https://github.com/spray/spray-json/pull/136
 *
 * The best performance is obtained by constructing an "explicit
 * implicit" (see the tests for examples) for anything that you
 * specifically wish to convert to / from (e.g. the top level family,
 * if it is on a popular endpoint) because it will reuse the
 * formatters at all stages in the hierarchy. However, the overhead
 * for not defining the `implicit val` is less that you might expect
 * (it only accounts for another 5%) whereas eagerly defining
 * `implicit val`s for everything in the hierarchy can
 * (counterintuitively) slow things down by 50%.
 *
 * Logging at the `trace` level is enabled to allow visibility of when
 * formatters are being instantiated.
 *
 * =Caveats=
 *
 * If shapeless fails to derive a family format for you, it won't tell
 * you what was missing in your tree. e.g. if you have a `UUID` in a
 * deeply nested case class in a `MyFamily` sealed trait, it will
 * simply say "can't find implicit for MyFamily" not "can't find
 * implicit for UUID". When that happens, you just have to work out
 * what is missing by trial and error. Sorry!
 *
 * Also, the Scala compiler has some funny [order dependency
 * rules](https://issues.scala-lang.org/browse/SI-7755) which will
 * sometimes make it look like you're missing an implicit for an
 * element. The best way to avoid this is:
 *
 * 1. define the protocol/formatters in sibling, or otherwise
 *    independent, non-cyclic, packages.
 *
 * 2. define all your custom rules in an `object` that extends
 *    `FamilyFormats` so that the implicit resolution priority rules
 *    work in your favour (see tests for an example of this style).
 *
 * Finally, if you have `scala.Symbol` in your family, you need to
 * provide an explicit `implicit val symbolFormat = SymbolJsonFormat`,
 * similarly for `JsObjectFormat` (and there may be others). Most
 * concerning, we have no idea why. Shapeless' derived orphans may be
 * significant in finding a solution.
 */
trait FamilyFormats extends LowPriorityFamilyFormats {
  this: StandardFormats =>

  // scala compiler doesn't like spray-json's use of a type alias in the sig
  override implicit def optionFormat[T: JsonFormat]: JsonFormat[Option[T]] = new OptionFormat[T]
}
object FamilyFormats extends DefaultJsonProtocol with FamilyFormats

/* low priority implicit scope so user-defined implicits take precedence */
private[json] trait LowPriorityFamilyFormats
    extends JsonFormatXor with JsonFormatHints {
  this: StandardFormats with FamilyFormats =>

  private[json] def log = LoggerFactory.getLogger(getClass)

  /**
   * a `JsonFormat[HList]` or `JsonFormat[CList]` would not retain the
   * type information for the full generic that it is serialising.
   * This allows us to pass the wrapped type, achieving: 1) custom
   * `CoproductHint`s on a per-trait level 2) configurable `null` behaviour
   * on a per product level 3) clearer error messages.
   *
   * This is intentionally not part of the `JsonFormat` hierarchy to
   * avoid ambiguous implicit errors.
   */
  abstract class WrappedRootJsonFormat[Wrapped, SubRepr](
      implicit
      tpe: Typeable[Wrapped]
  ) {
    final def read(j: JsValue): SubRepr = j match {
      case jso: JsObject => readJsObject(jso)
      case other => unexpectedJson[Wrapped](other)
    }
    def readJsObject(j: JsObject): SubRepr
    def write(v: SubRepr): JsObject
  }

  // save an object alloc every time and gives ordering guarantees
  private[this] val emptyJsObject = new JsObject(ListMap())

  // HNil is the empty HList
  implicit def hNilFormat[Wrapped](
    implicit
    t: Typeable[Wrapped]
  ): WrappedRootJsonFormat[Wrapped, HNil] = new WrappedRootJsonFormat[Wrapped, HNil] {
    def readJsObject(j: JsObject) = HNil // usually a populated JsObject, contents irrelevant
    def write(n: HNil) = emptyJsObject
  }

  // HList with a FieldType at the head
  implicit def hListFormat[Wrapped, Key <: Symbol, Value, Remaining <: HList](
    implicit
    t: Typeable[Wrapped],
    ph: ProductHint[Wrapped],
    key: Witness.Aux[Key],
    jfh: Lazy[JsonFormat[Value]], // svc doesn't need to be a RootJsonFormat
    jft: Lazy[WrappedRootJsonFormat[Wrapped, Remaining]]
  ): WrappedRootJsonFormat[Wrapped, FieldType[Key, Value] :: Remaining] =
    new WrappedRootJsonFormat[Wrapped, FieldType[Key, Value] :: Remaining] {
      private[this] val fieldName = ph.fieldName(key.value)

      private[this] def missingFieldError(j: JsObject): Nothing =
        deserError(s"missing $fieldName, found ${j.fields.keys.mkString(",")}")

      def readJsObject(j: JsObject) = {
        val resolved: Value = (j.fields.get(fieldName), jfh.value) match {
          // (None, _) means the value is missing in the wire format
          case (None, f) if ph.nulls == NeverJsNull =>
            f.read(JsNull)

          case (None, f) if ph.nulls == AlwaysJsNull =>
            missingFieldError(j)

          case (None, f: OptionFormat[_]) if ph.nulls == JsNullNotNone =>
            None.asInstanceOf[Value]

          case (Some(JsNull), f: OptionFormat[_]) if ph.nulls == JsNullNotNone =>
            f.readSome(JsNull)

          case (Some(value), f) =>
            f.read(value)

          case _ =>
            missingFieldError(j)
        }
        val remaining = jft.value.read(j)
        field[Key](resolved) :: remaining
      }

      def write(ft: FieldType[Key, Value] :: Remaining) = (jfh.value.write(ft.head), jfh.value) match {
        // (JsNull, _) means the underlying formatter serialises to JsNull
        case (JsNull, _) if ph.nulls == NeverJsNull =>
          jft.value.write(ft.tail)
        case (JsNull, _) if ph.nulls == JsNullNotNone & ft.head == None =>
          jft.value.write(ft.tail)
        case (value, _) =>
          jft.value.write(ft.tail) match {
            case JsObject(others) =>
              // when gathering results, we must remember that 'other'
              // is to the right of us and this seems to be the
              // easiest way to prepend to a ListMap
              JsObject(ListMap(fieldName -> value) ++: others)
            case other =>
              serError(s"expected JsObject, seen $other")
          }
      }
    }

  // CNil is the empty co-product. It's never called because it would
  // mean a non-existant sealed trait in our interpretation.
  implicit def cNilFormat[Wrapped](
    implicit
    t: Typeable[Wrapped]
  ): WrappedRootJsonFormat[Wrapped, CNil] = new WrappedRootJsonFormat[Wrapped, CNil] {
    def readJsObject(j: JsObject) = deserError(s"read should never be called for CNil, $j")
    def write(c: CNil) = serError("write should never be called for CNil")
  }

  // CList with a FieldType at the head
  implicit def cListFormat[Wrapped, Name <: Symbol, Instance, Remaining <: Coproduct](
    implicit
    tpe: Typeable[Wrapped],
    th: CoproductHint[Wrapped],
    key: Witness.Aux[Name],
    jfh: Lazy[RootJsonFormat[Instance]],
    jft: Lazy[WrappedRootJsonFormat[Wrapped, Remaining]]
  ): WrappedRootJsonFormat[Wrapped, FieldType[Name, Instance] :+: Remaining] =
    new WrappedRootJsonFormat[Wrapped, FieldType[Name, Instance] :+: Remaining] {
      def readJsObject(j: JsObject) = th.read(j, key.value) match {
        case Some(product) =>
          val recovered = jfh.value.read(product)
          Inl(field[Name](recovered))

        case None =>
          Inr(jft.value.read(j))
      }

      def write(lr: FieldType[Name, Instance] :+: Remaining) = lr match {
        case Inl(l) =>
          jfh.value.write(l) match {
            case j: JsObject => th.write(j, key.value)
            case other => serError(s"expected JsObject, got $other")
          }

        case Inr(r) =>
          jft.value.write(r)
      }
    }

  /**
   * Format for `LabelledGenerics` that uses the `HList` or `CList`
   * marshaller above.
   *
   * `Blah.Aux[T, Repr]` is a trick to work around scala compiler
   * constraints. We'd really like to have only one type parameter
   * (`T`) implicit list `g: LabelledGeneric[T], f:
   * Lazy[JsonFormat[T.Repr]]` but that's not possible.
   */
  implicit def familyFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    sg: Lazy[WrappedRootJsonFormat[T, Repr]],
    tpe: Typeable[T],
    th: CoproductHint[T],
    ph: ProductHint[T],
    not: NoExistingJsonFormat[T]
  ): RootJsonFormat[T] = new RootJsonFormat[T] {
    if (log.isTraceEnabled)
      log.trace(s"creating ${tpe.describe}")

    def read(j: JsValue): T = gen.from(sg.value.read(j))
    def write(t: T): JsObject = sg.value.write(gen.to(t))
  }
}

/**
 * a.k.a. the "ambiguous implicits" trick:
 * http://stackoverflow.com/questions/15962743
 *
 * If a `JsonFormat[A]` is already defined elsewhere, both of the
 * implicit defs here will trigger and result in an ambiguous
 * implicit compilation error.
 *
 * Requiring an implicit `NoExistingJsonFormat` on a method will
 * guarantee that it can only be called if there is no existing
 * `JsonFormat` (note that it will not match on existing
 * `RootJsonFormat`, but that's not needed).
 *
 * Typically called `Not`, I have renamed it `Xor` because it feels
 * more like an exclusive OR constraint to me.
 */
trait JsonFormatXor {
  trait Xor[T]
  implicit def alwaysDefined[T]: Xor[JsonFormat[T]] = new Xor[JsonFormat[T]] {}
  implicit def onlyIfJsonFormatDefined[T: JsonFormat]: Xor[JsonFormat[T]] = new Xor[JsonFormat[T]] {}
  type NoExistingJsonFormat[T] = Xor[JsonFormat[T]]
}

trait JsonFormatHints {
  trait CoproductHint[T] {
    /**
     * Given the `JsObject` for the sealed family, disambiguate and
     * extract the `JsObject` associated to the `Name` implementation
     * (if available) or otherwise return `None`.
     */
    def read[Name <: Symbol](j: JsObject, n: Name): Option[JsObject]

    /**
     * Given the `JsObject` for the contained product type of `Name`,
     * encode disambiguation information for later retrieval.
     */
    def write[Name <: Symbol](j: JsObject, n: Name): JsObject

    /**
     * Override to provide custom field naming.
     * Caching is recommended for performance.
     */
    protected def fieldName(orig: String): String = orig
  }

  /**
   * Product types are disambiguated by a `{"key":"value",...}`. Of
   * course, this will fail if the product type has a field with the
   * same name as the key. The default key is the word "type" which
   * is a keyword in Scala so unlikely to collide with too many case
   * classes.
   *
   * This variant is most common in JSON serialisation schemes and
   * well supported by other frameworks.
   */
  class FlatCoproductHint[T: Typeable](key: String) extends CoproductHint[T] {
    def read[Name <: Symbol](j: JsObject, n: Name): Option[JsObject] = {
      j.fields.get(key) match {
        case Some(JsString(hint)) if hint == fieldName(n.name) => Some(j)
        case Some(JsString(hint)) => None
        case _ =>
          deserError(s"missing $key, found ${j.fields.keys.mkString(",")}")
      }
    }

    // puts the typehint at the head of the field list
    def write[Name <: Symbol](j: JsObject, n: Name): JsObject = {
      // runtime error, would be nice if we could check this at compile time
      if (j.fields.contains(key))
        serError("typehint '$key' collides with existing field ${j.fields(key)}")
      JsObject(ListMap(key -> JsString(fieldName(n.name))) ++: j.fields)
    }
  }

  /**
   * Product types are disambiguated by an extra JSON map layer
   * containing a single key which is the name of the type of product
   * contained in the value. e.g. `{"MyType":{...}}`
   *
   * This variant may be more appropriate for non-polymorphic schemas
   * such as MongoDB and Mongoose (consider using the above format on
   * your endpoints, and this format when persisting).
   */
  class NestedCoproductHint[T: Typeable] extends CoproductHint[T] {
    def read[Name <: Symbol](j: JsObject, n: Name): Option[JsObject] =
      j.fields.get(fieldName(n.name)).map {
        case jso: JsObject => jso
        case other => unexpectedJson(other)
      }

    def write[Name <: Symbol](j: JsObject, n: Name): JsObject =
      JsObject(fieldName(n.name) -> j)
  }

  implicit def coproductHint[T: Typeable]: CoproductHint[T] = new FlatCoproductHint[T]("type")

  /**
   * Sometimes the wire format needs to match an existing format and
   * `JsNull` behaviour needs to be customised. This allows null
   * behaviour to be defined at the product level. Field level control
   * is only possible with a user-defined `RootJsonFormat`.
   */
  sealed trait JsNullBehaviour
  /** All values serialising to `JsNull` will be included in the wire format. Ambiguous. */
  case object AlwaysJsNull extends JsNullBehaviour
  /** Option values of `None` are omitted, but `Some` values of `JsNull` are retained. Default. */
  case object JsNullNotNone extends JsNullBehaviour
  /** No values serialising to `JsNull` will be included in the wire format. Ambiguous. */
  case object NeverJsNull extends JsNullBehaviour

  trait ProductHint[T] {
    def nulls: JsNullBehaviour = JsNullNotNone
    def fieldName[Key <: Symbol](key: Key): String = key.name
  }
  implicit def productHint[T: Typeable] = new ProductHint[T] {}

}
