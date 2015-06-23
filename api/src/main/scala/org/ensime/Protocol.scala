package org.ensime

import java.io._
import api._

/**
 * Provides a blocking I/O API for reading RpcRequest messages and
 * writing out RpcResponse, RpcErrors and EnsimeEvents.
 */
trait Protocol {
  def read(input: InputStream): RpcRequestEnvelope
  def write(resp: RpcResponse, output: OutputStream): Unit
  def write(event: EnsimeEvent, output: OutputStream): Unit
  def write(error: RpcError, output: OutputStream): Unit
}
