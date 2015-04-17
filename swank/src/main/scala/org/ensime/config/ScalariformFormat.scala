package org.ensime.config

import org.ensime.sexp._
import org.ensime.sexp.formats._
import scalariform.formatter.preferences._

trait ScalariformFormat {
  this: BasicFormats =>

  // TODO sexp formatting should live in server.protocol

  implicit object FormattingPreferencesFormat extends SexpFormat[FormattingPreferences] {

    private def key(d: PreferenceDescriptor[_]) = SexpSymbol(":" + d.key)

    private def deser(
      descriptor: PreferenceDescriptor[_],
      data: Map[SexpSymbol, Sexp]
    ): Option[Any] =
      data.get(key(descriptor)).map { sexp =>
        descriptor.preferenceType match {
          case BooleanPreference => sexp.convertTo[Boolean]
          case IntegerPreference(_, _) => sexp.convertTo[Int]
        }
      }

    def read(s: Sexp): FormattingPreferences = s match {
      case SexpNil =>
        FormattingPreferences()

      case SexpData(data) =>
        val custom = for {
          descriptor <- AllPreferences.preferences
          value <- deser(descriptor, data)
        } yield (descriptor, value)
        new FormattingPreferences(custom.toMap)

      case x => deserializationError(x)
    }

    private def ser(
      descriptor: PreferenceDescriptor[_],
      value: Any
    ): Sexp = descriptor.preferenceType match {
      case BooleanPreference => value.asInstanceOf[Boolean].toSexp
      case IntegerPreference(_, _) => value.asInstanceOf[Int].toSexp
    }

    def write(f: FormattingPreferences) = {
      val data = f.preferencesMap.map {
        case (d, v) => key(d) -> ser(d, v)
      }
      SexpData(data.toList)
    }
  }
}
