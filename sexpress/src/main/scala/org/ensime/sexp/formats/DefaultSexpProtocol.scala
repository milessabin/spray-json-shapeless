package org.ensime.sexp.formats

trait DefaultSexpProtocol
  extends BasicFormats
  with StandardFormats
  with CollectionFormats
  with ProductFormats

object DefaultSexpProtocol extends DefaultSexpProtocol
