package org.ensime.sexp.formats

import scala.util.Try

import org.ensime.sexp._

/**
 * Utility methods for creating custom `SexpFormat`s.
 */
object SexpFormatUtils {
  /**
   * Lazy wrapper around serialization. Useful when you want to
   * serialize (mutually) recursive structures.
   */
  def lazyFormat[T](format: => SexpFormat[T]) = new SexpFormat[T] {
    lazy val delegate = format
    def write(x: T) = delegate.write(x)
    def read(value: Sexp) = delegate.read(value)
  }

  /**
   * Wraps an existing `SexpReader` with `Exception` protection.
   */
  def safeReader[A: SexpReader] = new SexpReader[Try[A]] {
    def read(value: Sexp) = Try(value.convertTo[A])
  }

  /**
   * Turns an `SexpWriter` into a `SexpFormat` that throws an
   * `UnsupportedOperationException` for reads.
   */
  def lift[T](writer: SexpWriter[T]) = new SexpFormat[T] {
    def write(obj: T): Sexp = writer.write(obj)
    def read(value: Sexp) =
      throw new UnsupportedOperationException("SexpReader implementation missing")
  }

  /**
   * Turns an `SexpReader` into a `SexpFormat` that throws an
   * `UnsupportedOperationException` for writes.
   */
  def lift[T](reader: SexpReader[T]) = new SexpFormat[T] {
    def write(obj: T): Sexp =
      throw new UnsupportedOperationException(s"SexpWriter implementation missing")
    def read(value: Sexp) = reader.read(value)
  }
}
