# spray-json-shapeless

Automatically derive [spray-json](https://github.com/spray/spray-json) `JsonFormat`s, powered by [shapeless](https://github.com/milessabin/shapeless) (no macros were written during the creation of this library).

For documentation, [read the scaladocs](src/main/scala/fommil/sjs/FamilyFormats.scala) and for examples [read the test cases](src/test/scala/fommil/sjs/FamilyFormatsSpec.scala).

**Please read the documentation and examples thoroughly before raising a ticket.**

## TL;DR

```scala
libraryDependencies += "com.github.fommil" %% "spray-json-shapeless" % "1.0.0"
```

```scala
import spray.json._
import fommil.sjs._

// your ADT here
package adt {
  sealed trait SimpleTrait
  case class Foo(s: String) extends SimpleTrait
  case class Bar() extends SimpleTrait
  case object Baz extends SimpleTrait
  case class Faz(o: Option[String]) extends SimpleTrait
}
object MyFormats extends DefaultJsonProtocol with FamilyFormats {
  // gives a slight performance boost, but isn't necessary
  implicit val MyAdtFormats = shapeless.cachedImplicit[JsonFormat[SimpleTrait]]
}
import MyFormats._

Foo("foo").toJson              // """{"s":"foo"}"""
Faz(Some("meh")).toJson        // """{"o":"meh"}"""
Faz(None).toJson               // "{}"
Foo("foo"): SimpleTrait.toJson // """{"type":"Foo","s":"foo"}"""
Bar(): SimpleTrait.toJson      // """{"type":"Bar"}"""
Baz: SimpleTrait.toJson        // """{"type":"Baz"}"""
Fuzz: SimpleTrait.toJson       // """{"type":"Fuzz"}"""
```

## License

spray-json-shapeless is an Open Source project under the Apache License v2.

I would prefer to license under the LPGL because
[TypeSafe have set the precedent of closing sources](https://github.com/smootoo/freeslick#history)
and it is extremely concerning that this is happening within our
community. The best way to fight back is by writing software with a
license that guarantees your software's freedom.

I do not have a problem with anyone charging for software (I don't
mind if something is not free as in gratis) but I abhor software which
does not provide source code or allow me to change (i.e. fix it when
it breaks, which it does almost always): free as in libre.

However, both of the primary upstream projects, spray-json and shapeless, are
published under Apache v2, so it is more appropriate to publish this project
on the same terms.
