package org.ensime.util

sealed abstract class RefactorLocation(val symbol: Symbol)

object RefactorLocation {
  case object QualifiedName extends RefactorLocation('qualifiedName)
  case object File extends RefactorLocation('file)
  case object NewName extends RefactorLocation('newName)
  case object Name extends RefactorLocation('name)
  case object Start extends RefactorLocation('start)
  case object End extends RefactorLocation('end)
  case object MethodName extends RefactorLocation('methodName)
}
