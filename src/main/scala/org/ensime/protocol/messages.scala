package org.ensime.protocol

case class RPCRequest(data: RPCCall, callId: Int)

sealed trait RPCCall

case class ExecUndoRequest(id: Int) extends RPCCall

