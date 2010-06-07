package com.ensime.config
import java.io.File
import java.util.concurrent.Callable

trait GlobalLock{
  def apply[T](lockFile:File, run:Callable[T]):T;
}
