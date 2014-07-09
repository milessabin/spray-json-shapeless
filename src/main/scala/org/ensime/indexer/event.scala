package org.ensime.indexer

sealed trait IndexEvent
case class ClassEvent(name: String, location: String, flags: Int) extends IndexEvent
case class MethodEvent(className: String, name: String, location: String, flags: Int) extends IndexEvent
case object StopEvent extends IndexEvent

