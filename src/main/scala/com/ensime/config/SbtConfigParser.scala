/*
* Based on code from the sbt project:
* Copyright 2009, 2010  Mark Harrah
*/
package com.ensime.config


import java.lang.Character.isWhitespace
import java.io.{BufferedReader, File, FileInputStream, InputStreamReader, Reader, StringReader}
import java.net.{MalformedURLException, URL}
import java.util.regex.Pattern
import scala.collection.immutable.ListMap

object SbtConfigParser extends NotNull
{
  def apply(file: File): SbtConfig = Using(new InputStreamReader(new FileInputStream(file), "UTF-8"))(apply)
  def apply(s: String): SbtConfig = Using(new StringReader(s))(apply)
  def apply(reader: Reader): SbtConfig = Using(new BufferedReader(reader))(apply)

  type Prop = (String, String)

  private def apply(in:BufferedReader):SbtConfig = {
    def readLine(accum:List[Option[Prop]], index:Int):List[Option[Prop]] = {
      val line = in.readLine()
      if(line eq null) accum.reverse else readLine(parseLine(line, index) ::: accum, index+1)
    }
    val m = readLine(Nil, 0).flatten.toMap
    val buildScalaVersion = m.get("build.scala.versions").getOrElse("2.8.0")
    new SbtConfig(buildScalaVersion)
  }

  def parseProp(str: String):Option[Prop] = str.split("=",2) match {
    case Array(name,value) => Some(name.trim, value.trim)
    case x => None
  }

  def parseLine(content: String, line: Int) = {
    val trimmed = content.trim
    val offset = content.length - trimmed.length

    if(trimmed.isEmpty) {
      Nil
    }
    else {
      val processed =
      trimmed.charAt(0) match
      {
	case '#' => None
	case _ => parseProp(trimmed)
      }
      processed :: Nil
    }
  }
}
