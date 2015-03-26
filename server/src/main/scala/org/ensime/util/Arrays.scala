package org.ensime.util

import scala.reflect.ClassTag

object Arrays {

  def splice[T: ClassTag](a: Array[T], start: Int, end: Int, b: Array[T]): Array[T] = {
    val c = new Array[T](a.length + (b.length - (end - start)))
    System.arraycopy(a, 0, c, 0, start)
    System.arraycopy(b, 0, c, start, b.length)
    System.arraycopy(a, end + 1, c, start + b.length, a.length - end - 1)
    c
  }
}
