package org.ensime.util

/*
* https://issues.scala-lang.org/browse/SI-4601
*/

object Symbols {
  val Method = 'method
  val Trait = 'trait
  val Interface = 'interface
  val Object = 'object
  val Class = 'class
  val Field = 'field
  val Nil = 'nil

  val Rename = 'rename
  val ExtractMethod = 'extractMethod
  val ExtractLocal = 'extractLocal
  val InlineLocal = 'inlineLocal
  val OrganizeImports = 'organizeImports
  val AddImport = 'addImport

  val QualifiedName = 'qualifiedName
  val File = 'file
  val NewName = 'newName
  val Name = 'name
  val Start = 'start
  val End = 'end
  val MethodName = 'methodName
}

