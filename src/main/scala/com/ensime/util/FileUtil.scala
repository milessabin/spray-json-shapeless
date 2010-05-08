package com.ensime.util

import scala.collection.Seq
import java.io.File

// This routine stolen from http://rosettacode.org/wiki/Walk_a_directory/Recursively#Scala


/** A wrapper around file, allowing iteration either on direct children 
or on directory tree */
class RichFile(file: File) {
  
  def children = new Iterable[File] {
    override def iterator = (
      if (file.isDirectory) file.listFiles.iterator else Iterator.empty)
  }
  
  def andTree : Iterable[File] = (
    Seq(file) ++ children.flatMap(child => new RichFile(child).andTree))

}

/** implicitely enrich java.io.File with methods of RichFile */
object RichFile {
  implicit def toRichFile(file: File) = new RichFile(file)
}



