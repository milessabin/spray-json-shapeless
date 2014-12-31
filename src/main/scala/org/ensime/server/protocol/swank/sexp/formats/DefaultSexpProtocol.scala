package org.ensime.server.protocol.swank.sexp.formats

trait DefaultSexpProtocol
  extends BasicFormats
  with StandardFormats
  with CollectionFormats
  with ProductFormats

object DefaultSexpProtocol extends DefaultSexpProtocol
