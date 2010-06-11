/* sbt -- Simple Build Tool
* Copyright 2008, 2009, 2010  Mark Harrah
*/
package com.ensime.config

import java.io.File

sealed trait IvyConfiguration extends NotNull{
  type This <: IvyConfiguration
  def baseDirectory: File
  def lock: Option[GlobalLock]
  def log: IvyLogger
  def file: File
  def cacheBaseDirectory: File
}

class MvnFileConfiguration(
  val baseDirectory: File, 
  val lock: Option[GlobalLock], 
  val log: IvyLogger, 
  val file:File,
  val cacheBaseDirectory:File
) extends IvyConfiguration{}


class IvyFileConfiguration(
  val baseDirectory: File, 
  val lock: Option[GlobalLock], 
  val log: IvyLogger, 
  val file:File,
  val cacheBaseDirectory:File
) extends IvyConfiguration{}








