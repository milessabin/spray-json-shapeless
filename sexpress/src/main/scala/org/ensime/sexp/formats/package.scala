package org.ensime.sexp

package object formats {
  def deserializationError(got: Sexp) =
    throw new DeserializationException(s"Unable to parse $got")

  def serializationError(msg: String) = throw new SerializationException(msg)
}

