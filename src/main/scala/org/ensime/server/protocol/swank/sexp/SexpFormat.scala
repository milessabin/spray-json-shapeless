package org.ensime.server.protocol.swank.sexp

import annotation.implicitNotFound

/** Provides the S-Exp deserialization for type T. */
@implicitNotFound(msg = "Cannot find SexpReader or SexpFormat type class for ${T}")
trait SexpReader[T] {
  def read(value: Sexp): T
}

object SexpReader {
  implicit def func2Reader[T](f: Sexp => T): SexpReader[T] = new SexpReader[T] {
    def read(sexp: Sexp) = f(sexp)
  }
}

/** Provides the S-Exp serialization for type T. */
@implicitNotFound(msg = "Cannot find SexpWriter or SexpFormat type class for ${T}")
trait SexpWriter[T] {
  def write(obj: T): Sexp
}

object SexpWriter {
  implicit def func2Writer[T](f: T => Sexp): SexpWriter[T] = new SexpWriter[T] {
    def write(obj: T) = f(obj)
  }
}

/** Provides the S-Exp deserialization and serialization for type T. */
trait SexpFormat[T] extends SexpReader[T] with SexpWriter[T]

object SexpFormat {
  /**
   * Some implicit implementations of an `SexpFormat` return a class
   * that is refined by a type parameter or similar. Such
   * implementations are very inefficient because the class is created
   * every single time its use is invoked, resulting in excessive
   * memory churn. To workaround the problem, this may be used to assign
   * the implementation to an `implicit val` which is re-used.
   *
   * In addition, limitations in the current release of Shapeless
   * (2.0.0) mean that nested case classes are not allowed unless the
   * nested types have a pre-computed `implicit val` as provided by
   * this.
   *
   * See https://github.com/typelevel/scala/issues/83 for more.
   */
  def apply[T](implicit f: SexpFormat[T]) = f
}
