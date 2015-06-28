# spray-json-shapeless

Automatically derive [spray-json](https://github.com/spray/spray-json) `JsonFormat`s, powered by [shapeless](https://github.com/milessabin/shapeless) (no macros were written during the creation of this library).

For documentation, [read the scaladocs](src/main/scala/fommil/sjs/FamilyFormats.scala) and for examples [read the test cases](src/test/scala/fommil/sjs/FamilyFormatsSpec.scala).

**Please read the documentation and examples thoroughly before raising a ticket.**

## TL;DR

```scala
libraryDependencies += "com.github.fommil" %% "spray-json-shapeless" % "1.0.0-SNAPSHOT"
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

Note that this library is LGPL. All contributions are on this basis.

I chose to license under the LPGL because
[TypeSafe have set the precedent of closing sources](https://github.com/smootoo/freeslick#history)
and it is extremely concerning that this is happening within our
community. The best way to fight back is by writing software with a
license that guarantees your software's freedom.

I do not have a problem with anyone charging for software (I don't
mind if something is not free as in gratis) but I abhor software which
does not provide source code or allow me to change (i.e. fix it when
it breaks, which it does almost always): free as in libre.

If you *really* need an alternative license to get past your legal
department, I will consider one-off licensing deals if you can prove
to me that your legal department will not accept LGPL, that you have
tried to get them to accept it, you donate money to the ongoing
maintenance costs of [ENSIME](https://github.com/ensime) and agree to
submit any changes that you (or your colleagues) make back to this
project under the LGPL.
