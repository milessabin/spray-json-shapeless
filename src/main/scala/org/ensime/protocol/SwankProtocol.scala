/**
 *  Copyright (c) 2010, Aemon Cannon
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *      * Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      * Neither the name of ENSIME nor the
 *        names of its contributors may be used to endorse or promote products
 *        derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED. IN NO EVENT SHALL Aemon Cannon BE LIABLE FOR ANY
 *  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 *  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 *  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.ensime.protocol

import java.io._
import org.ensime.config.{ ProjectConfig, ReplConfig }
import org.ensime.indexer.MethodBytecode
import org.ensime.model._
import org.ensime.server._
import org.ensime.util._
import org.ensime.util.SExp._
import scala.actors._
import scala.tools.nsc.util.{ Position, RangePosition }
import scala.util.parsing.input

object SwankProtocol extends SwankProtocol {}

trait SwankProtocol extends Protocol {

  class ConnectionInfo {
    val pid = None
    val serverName: String = "ENSIME-ReferenceServer"
    val protocolVersion: String = "0.8.6"
  }

  /**
   * Protocol Version: 0.8.6
   *
   * Protocol Change Log:
   *   0.8.6
   *     Add support for ranges to type-at-point, inspect-type-at-point,
   *       type-by-name-at-point
   *   0.8.5
   *     DebugLocation of type 'field now gets field name from :field, not from :name
   *     The debug-to-string call now requires thread id
   *   0.8.4
   *     Add local-name to SymbolInfo
   *   0.8.3
   *     Add debug-to-string call.
   *     Refactor debug-value-for-name to debug-locate-name + debug-value
   *     Adds typecheck-files
   *     Unifies debug locations under DebugLocation
   *   0.8.2
   *     Debug attachment to remote VM
   *     CompletionInfo type-sig now has structure see CompletionSignature
   *   0.8.1
   *     Add patch-source.
   *     Completions now takes a 'reload' argument.
   *   0.8
   *     Add RPC calls for debugging
   *     Protocol is now explicitely UTF-8
   *   0.7.4
   *     Add optional 'owner-type-id' key to SymbolInfo
   *     Add optional 'case-sens' option to swank:completions call
   *   0.7.3
   *     Add optional 'to-insert' key to CompletionInfo
   *     Add optional a max results argument to swank:completions call
   *   0.7.2
   *     Get rid of scope and type completion in favor of unified
   *     swank:completions call.
   *     Wrap completion result in CompletionInfoList.
   *   0.7.1
   *     Remove superfluous status values for events such as :compiler-ready,
   *     :clear-scala-notes, etc.
   *   0.7
   *     Rename swank:perform-refactor to swank:prepare-refactor.
   *     Include status flag in return of swank:exec-refactor.
   */


  import SwankProtocol._
  import ProtocolConst._

  private var outPeer: Actor = null;
  private var rpcTarget: RPCTarget = null;

  def peer = outPeer

  def setOutputActor(peer: Actor) { outPeer = peer }

  def setRPCTarget(target: RPCTarget) { this.rpcTarget = target }

  // Handle reading / writing of messages

  def writeMessage(value: WireFormat, out: OutputStream) {
    val dataString: String = value.toWireString
    val data: Array[Byte] = dataString.getBytes("UTF-8")
    val header: Array[Byte] = String.format(
      "%06x", int2Integer(data.length)).getBytes("UTF-8")
    println("Writing: " + dataString)
    out.write(header)
    out.write(data)
    out.flush()
  }

  private def fillArray(in: java.io.Reader, a: Array[Char]) {
    var n = 0
    var l = a.length
    var charsRead = 0
    while (n < l) {
      charsRead = in.read(a, n, l - n)
      if (charsRead == -1) {
        throw new EOFException("End of file reached in socket reader.");
      } else {
        n += charsRead
      }
    }
  }

  private val headerBuf = new Array[Char](6);

  def readMessage(in: java.io.InputStream): WireFormat = {
    val reader = new InputStreamReader(in, "UTF-8")
    fillArray(reader, headerBuf)
    val msglen = Integer.valueOf(new String(headerBuf), 16).intValue()
    if (msglen > 0) {
      //TODO allocating a new array each time is inefficient!
      val buf: Array[Char] = new Array[Char](msglen)
      fillArray(reader, buf)
      SExp.read(new input.CharArrayReader(buf))
    } else {
      throw new IllegalStateException("Empty message read from socket!")
    }
  }

  def handleIncomingMessage(msg: Any) {
    msg match {
      case sexp: SExp => handleMessageForm(sexp)
      case _ => System.err.println("WTF: Unexpected message: " + msg)
    }
  }

  private def handleMessageForm(sexp: SExp) {
    sexp match {
      case SExpList(KeywordAtom(":swank-rpc") :: form :: IntAtom(callId) :: rest) => {
        handleEmacsRex(form, callId)
      }
      case _ => {
        sendProtocolError(ErrUnrecognizedForm, Some(sexp.toReadableString))
      }
    }
  }

  private def handleEmacsRex(form: SExp, callId: Int) {
    form match {
      case SExpList(SymbolAtom(name) :: rest) => {
        try {
          handleRPCRequest(name, form, callId)
        } catch {
          case e: Throwable =>
            {
              e.printStackTrace(System.err)
              sendRPCError(ErrExceptionInRPC, Some(e.getMessage), callId)
            }
        }
      }
      case _ => {
        sendRPCError(
          ErrMalformedRPC,
          Some("Expecting leading symbol in: " + form),
          callId)
      }
    }
  }



  ////////////////////
  // Datastructures //
  ////////////////////

  /**
   * Doc DataStructure:
   *   Position
   * Summary:
   *   A source position.
   * Structure:
   *   (
   *     :file   //String:A filename
   *     :offset  //Int:The zero-indexed character offset of this position.
   *   )
   */

  /**
   * Doc DataStructure:
   *   RangePosition
   * Summary:
   *   A source position that describes a range of characters in a file.
   * Structure:
   *   (
   *     :file   //String:A filename
   *     :start  //Int:The character offset of the start of the range.
   *     :end    //Int:The character offset of the end of the range.
   *   )
   */

  /**
   * Doc DataStructure:
   *   Change
   * Summary:
   *   Describes a change to a source code file.
   * Structure:
   *   (
   *   :file //String:Filename of source  to be changed
   *   :text //String:Text to be inserted
   *   :from //Int:Character offset of start of text to replace.
   *   :to //Int:Character offset of end of text to replace.
   *   )
   */

  /**
   * Doc DataStructure:
   *   SymbolSearchResult
   * Summary:
   *   Describes a symbol found in a search operation.
   * Structure:
   *   (
   *   :name //String:Qualified name of symbol.
   *   :local-name //String:Unqualified name of symbol
   *   :decl-as //Symbol:What kind of symbol this is.
   *   :owner-name //String:If symbol is a method, gives the qualified owner type.
   *   :pos //Position:Where is this symbol declared?
   *   )
   */

  /**
   * Doc DataStructure:
   *   ParamSectionInfo
   * Summary:
   *   Description of one of a method's parameter sections.
   * Structure:
   *   (
   *   :params //List of (String TypeInfo) pairs:Describes params in section
   *   :is-implicit //Bool:Is this an implicit parameter section.
   *   )
   */

  /**
   * Doc DataStructure:
   *   CallCompletionInfo
   * Summary:
   *   Description of a Scala method's type
   * Structure:
   *   (
   *   :result-type //TypeInfo
   *   :param-sections //List of ParamSectionInfo:
   *   )
   */

  /**
   * Doc DataStructure:
   *   TypeMemberInfo
   * Summary:
   *   Description of a type member
   * Structure:
   *   (
   *   :name //String:The name of this member.
   *   :type-sig //String:The type signature of this member
   *   :type-id //Int:The type id for this member's type.
   *   :is-callable //Bool:Is this a function or method type.
   *   )
   */

  /**
   * Doc DataStructure:
   *   TypeInfo
   * Summary:
   *   Description of a Scala type.
   * Structure:
   *   (
   *   :name //String:The short name of this type.
   *   :type-id //Int:Type Id of this type (for fast lookups)
   *   :full-name //String:The qualified name of this type
   *   :decl-as //Symbol:What kind of type is this? (class,trait,object, etc)
   *   :type-args //List of TypeInfo:Type args this type has been applied to.
   *   :members //List of TypeMemberInfo
   *   :arrow-type //Bool:Is this a function or method type?
   *   :result-type //TypeInfo:
   *   :param-sections //List of ParamSectionInfo:
   *   :pos //Position:Position where this type was declared
   *   :outer-type-id //Int:If this is a nested type, type id of owning type
   *   )
   */

  /**
   * Doc DataStructure:
   *   InterfaceInfo
   * Summary:
   *   Describes an inteface that a type supports
   * Structure:
   *   (
   *   :type //TypeInfo:The type of the interface.
   *   :via-view //Bool:Is this type suppoted via an implicit conversion?
   *   )
   */

  /**
   * Doc DataStructure:
   *   TypeInspectInfo
   * Summary:
   *   Detailed description of a Scala type.
   * Structure:
   *   (
   *   :type //TypeInfo
   *   :companion-id //Int:Type Id of this type's companion type.
   *   :interfaces //List of InterfaceInfo:Interfaces this type supports
   *   )
   */

  /**
   * Doc DataStructure:
   *   SymbolInfo
   * Summary:
   *   Description of a Scala symbol.
   * Structure:
   *   (
   *   :name //String:Name of this symbol.
   *   :local-name //String:Unqualified name of this symbol.
   *   :type //TypeInfo:The type of this symbol.
   *   :decl-pos //Position:Source location of this symbol's declaration.
   *   :is-callable //Bool:Is this symbol a method or function?
   *   :owner-type-id //Int: (optional) Type id of owner type.
   *   )
   */

  /**
   * Doc DataStructure:
   *   CompletionSignature
   * Summary:
   *   An abbreviated signature for a type member
   * Structure:
   *   (
   *   //List of List of Pairs of String: Parameter sections
   *   //String: Result type
   *   )
   */

  /**
   * Doc DataStructure:
   *   CompletionInfo
   * Summary:
   *   An abbreviated symbol description.
   * Structure:
   *   (
   *   :name //String:Name of this symbol.
   *   :type-sig //A CompletionSignature
   *   :type-id //Int:A type id.
   *   :is-callable //Bool:Is this symbol a method or function?
   *   :to-insert //String|Nil:The representation that should be
   *     written to the buffer.
   *   )
   */

  /**
   * Doc DataStructure:
   *   CompletionInfoList
   * Summary:
   *   An annotated collection of CompletionInfo structures.
   * Structure:
   *   (
   *   :prefix //String:The common prefix for all selections,
   *     modulo case.
   *   :completions //List of CompletionInfo:In order of descending
   *     relevance.
   *   )
   */

  /**
   * Doc DataStructure:
   *   PackageInfo
   * Summary:
   *   A description of a package and all its members.
   * Structure:
   *   (
   *   :name //String:Name of this package.
   *   :full-name //String:Qualified name of this package.
   *   :members //List of PackageInfo | TypeInfo:The members of this package.
   *   :info-type //Symbol:Literally 'package
   *   )
   */

  /**
   * Doc DataStructure:
   *   SymbolDesignations
   * Summary:
   *   Describe the symbol classes in a given textual range.
   * Structure:
   *   (
   *   :file //String:Filename of file to be annotated.
   *   :syms //List of (Symbol Integer Integer):Denoting the symbol class and start and end of the range where it applies.
   *   )
   */

  /**
   * Doc DataStructure:
   *   PackageMemberInfoLight
   * Summary:
   *   An abbreviated package member description.
   * Structure:
   *   (
   *   :name //String:Name of this symbol.
   *   )
   */

  /**
   * Doc DataStructure:
   *   RefactorFailure
   * Summary:
   *   Notification that a refactoring has failed in some way.
   * Structure:
   *   (
   *   :procedure-id //Int:The id for this refactoring.
   *   :message //String:A text description of the error.
   *   :status //Symbol:'failure.
   *   )
   */

  /**
   * Doc DataStructure:
   *   RefactorEffect
   * Summary:
   *   A description of the effects a proposed refactoring would have.
   * Structure:
   *   (
   *   :procedure-id //Int:The id for this refactoring.
   *   :refactor-type //Symbol:The type of refactoring.
   *   :status //Symbol:'success
   *   :changes //List of Change:The textual effects.
   *   )
   */

  /**
   * Doc DataStructure:
   *   RefactorResult
   * Summary:
   *   A description of the effects a refactoring has effected.
   * Structure:
   *   (
   *   :procedure-id //Int:The id for this refactoring.
   *   :refactor-type //Symbol:The type of refactoring.
   *   :touched //List of String:Names of files touched by the refactoring.
   *   :status //Symbol:'success.
   *   )
   */

  /**
   * Doc DataStructure:
   *   Note
   * Summary:
   *   Describes a note generated by the compiler.
   * Structure:
   *   (
   *   :severity //Symbol: One of 'error, 'warn, 'info.
   *   :msg //String: Text of the compiler message.
   *   :beg //Int: Zero-based offset of beginning of region
   *   :end //Int: Zero-based offset of end of region
   *   :line //Int: Line number of region
   *   :col //Int: Column offset of region
   *   :file //String: Filename of source file
   *   )
   */

  /**
   * Doc DataStructure:
   *   Notes
   * Summary:
   *   Describes a set of notes generated by the compiler.
   * Structure:
   *   (
   *   :is-full //Bool: Is the note the result of a full compilation?
   *   :notes //List of Note: The descriptions of the notes themselves.
   *   )
   */

  /**
   * Doc DataStructure:
   *   FilePatch
   * Summary:
   *   Describes a patch to be applied to a single file.
   * Structure:
   *   (
   *   // '+' is an insert of text before the existing text starting at i
   *   // '-' is a deletion of text in interval [i,j)
   *   // '*' is a replacement of text in interval [i,j)
   *   [("+" i "some text") | ("-" i j) | ("*" i j "some text")]*
   *   )
   */

  /**
   * Doc DataStructure:
   *   DebugLocation
   * Summary:
   *   A unique location in the VM under debug. Note: this datastructure
   *   is a union of several location types.
   * Structure:
   *   (
   *     :type   //Symbol:One of 'reference, 'field, 'element, 'slot
   *
   *     [ // if type is 'reference
   *     :object-id  //String:The unique id of the object.
   *     ]
   *
   *     [ // if type is 'field
   *     :object-id  //String:The unique id of the object.
   *     :field  //String:Name of the field of the object.
   *     ]
   *
   *     [ // if type is 'element
   *     :object-id  //String:The unique id of the array.
   *     :index  //Int:A zero-indexed offset within array.
   *     ]
   *
   *     [ // if type is 'slot
   *     :thread-id  //String:The unique id of the thread.
   *     :frame  //Int:Select the zero-indexed frame in the thread's call stack.
   *     :offset  //Int:A zero-indexed offset within frame.
   *     ]
   *
   *
   *   )
   */

  private def handleRPCRequest(callType: String, form: SExp, callId: Int) {

    println("\nHandling RPC: " + form.toReadableString)

    def oops = sendRPCError(ErrMalformedRPC,
      Some("Malformed " + callType + " call: " + form), callId)

    callType match {

      ///////////////
      // RPC Calls //
      ///////////////

      /**
       * Doc RPC:
       *   swank:connection-info
       * Summary:
       *   Request connection information.
       * Arguments:
       *   None
       * Return:
       *   (
       *   :pid  //Int:The integer process id of the server (or nil if unnavailable)
       *   :implementation
       *     (
       *     :name  //String:An identifying name for this server implementation.
       *     )
       *   :version //String:The version of the protocol this server supports.
       *   )
       * Example call:
       *   (:swank-rpc (swank:connection-info) 42)
       * Example return:
       *   (:return (:ok (:pid nil :implementation (:name "ENSIME - Reference Server")
       *   :version "0.7")) 42)
       */
      case "swank:connection-info" => {
        sendRPCReturn(toWF(new ConnectionInfo()), callId)
      }

      /**
       * Doc RPC:
       *   swank:init-project
       * Summary:
       *   Initialize the server with a project configuration. The
       *   server returns it's own knowledge about the project, including
       *   source roots which can be used by clients to determine whether
       *   a given source file belongs to this project.
       * Arguments:
       *   A complete ENSIME configuration property list. See manual.
       * Return:
       *   (
       *   :project-name //String:The name of the project.
       *   :source-roots //List of Strings:The source code directory roots..
       *   )
       * Example call:
       *   (:swank-rpc (swank:init-project (:use-sbt t :compiler-args
       *   (-Ywarn-dead-code -Ywarn-catches -Xstrict-warnings)
       *   :root-dir /Users/aemon/projects/ensime/)) 42)
       * Example return:
       *   (:return (:ok (:project-name "ensime" :source-roots
       *   ("/Users/aemon/projects/ensime/src/main/scala"
       *   "/Users/aemon/projects/ensime/src/test/scala"
       *   "/Users/aemon/projects/ensime/src/main/java"))) 42)
       */
      case "swank:init-project" => {
        form match {
          case SExpList(head :: (conf: SExpList) :: body) => {
            ProjectConfig.fromSExp(conf) match {
              case Right(config) => rpcTarget.rpcInitProject(config, callId)
              case Left(t) => sendRPCError(ErrExceptionInRPC,
                Some(t.toString()),
                callId)
            }
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:peek-undo
       * Summary:
       *   The intention of this call is to preview the effect of an undo
       *   before executing it.
       * Arguments:
       *   None
       * Return:
       *   (
       *   :id //Int:Id of this undo
       *   :changes //List of Changes:Describes changes this undo would effect.
       *   :summary //String:Summary of action this undo would revert.
       *   )
       * Example call:
       *   (:swank-rpc (swank:peek-undo) 42)
       * Example return:
       *   (:return (:ok (:id 1 :changes ((:file
       *   "/ensime/src/main/scala/org/ensime/server/RPCTarget.scala"
       *   :text "rpcInitProject" :from 2280 :to 2284))
       *   :summary "Refactoring of type: 'rename") 42)
       */
      case "swank:peek-undo" => {
        rpcTarget.rpcPeekUndo(callId)
      }

      /**
       * Doc RPC:
       *   swank:exec-undo
       * Summary:
       *   Execute a specific, server-side undo operation.
       * Arguments:
       *   An integer undo id. See swank:peek-undo for how to learn this id.
       * Return:
       *   (
       *   :id //Int:Id of this undo
       *   :touched-files //List of Strings:Filenames of touched files,
       *   )
       * Example call:
       *   (:swank-rpc (swank:exec-undo 1) 42)
       * Example return:
       *   (:return (:ok (:id 1 :touched-files
       *   ("/src/main/scala/org/ensime/server/RPCTarget.scala"))) 42)
       */
      case "swank:exec-undo" => {
        form match {
          case SExpList(head :: (IntAtom(id)) :: body) => {
            rpcTarget.rpcExecUndo(id, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:repl-config
       * Summary:
       *   Get information necessary to launch a scala repl for this project.
       * Arguments:
       *   None
       * Return:
       *   (
       *   :classpath //String:Classpath string formatted for passing to Scala.
       *   )
       * Example call:
       *   (:swank-rpc (swank:repl-config) 42)
       * Example return:
       *   (:return (:ok (:classpath "lib1.jar:lib2.jar:lib3.jar")) 42)
       */
      case "swank:repl-config" => {
        rpcTarget.rpcReplConfig(callId)
      }

      /**
       * Doc RPC:
       *   swank:builder-init
       * Summary:
       *   Initialize the incremental builder and kick off a full rebuild.
       * Arguments:
       *   None
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:builder-init) 42)
       * Example return:
       *   (:return (:ok ()) 42)
       */
      case "swank:builder-init" => {
        rpcTarget.rpcBuilderInit(callId)
      }

      /**
       * Doc RPC:
       *   swank:builder-update-files
       * Summary:
       *   Signal to the incremental builder that the given files
       *   have changed and must be rebuilt. Triggers rebuild.
       * Arguments:
       *   List of Strings:Filenames, absolute or relative to the project root.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:builder-update-files
       *   ("/ensime/src/main/scala/org/ensime/server/Analyzer.scala")) 42)
       * Example return:
       *   (:return (:ok ()) 42)
       */
      case "swank:builder-update-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderUpdateFiles(files, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:builder-add-files
       * Summary:
       *   Signal to the incremental builder that the given files
       *   should be added to the build. Triggers rebuild.
       * Arguments:
       *   List of Strings:Filenames, absolute or relative to the project root.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:builder-add-files
       *   ("/ensime/src/main/scala/org/ensime/server/Analyzer.scala")) 42)
       * Example return:
       *   (:return (:ok ()) 42)
       */
      case "swank:builder-add-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderAddFiles(files, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:builder-remove-files
       * Summary:
       *   Signal to the incremental builder that the given files
       *   should be removed from the build. Triggers rebuild.
       * Arguments:
       *   List of Strings:Filenames, absolute or relative to the project root.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:builder-remove-files
       *   ("/ensime/src/main/scala/org/ensime/server/Analyzer.scala")) 42)
       * Example return:
       *   (:return (:ok ()) 42)
       */
      case "swank:builder-remove-files" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcBuilderRemoveFiles(files, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:remove-file
       * Summary:
       *   Remove a file from consideration by the ENSIME analyzer.
       * Arguments:
       *   String:A filename, absolute or relative to the project.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:remove-file "Analyzer.scala") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:remove-file" => {
        form match {
          case SExpList(head :: StringAtom(file) :: body) => {
            rpcTarget.rpcRemoveFile(file, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:typecheck-file
       * Summary:
       *   Request immediate load and check the given source file.
       * Arguments:
       *   String:A filename, absolute or relative to the project.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:typecheck-file "Analyzer.scala") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:typecheck-file" => {
        form match {
          case SExpList(head :: StringAtom(file) :: body) => {
            rpcTarget.rpcTypecheckFiles(List(file), callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:typecheck-files
       * Summary:
       *   Request immediate load and check the given source files.
       * Arguments:
       *   List of String:Filenames, absolute or relative to the project.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:typecheck-files ("Analyzer.scala")) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:typecheck-files" => {
        form match {
          case SExpList(head :: SExpList(strings) :: body) => {
	    val filenames = strings.collect{case StringAtom(s) => s}.toList
            rpcTarget.rpcTypecheckFiles(filenames, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:patch-source
       * Summary:
       *   Request immediate load and check the given source file.
       * Arguments:
       *   String:A filename
       *   A FilePatch
       * Return:
       *   None
       * Example call:
       *   (swank:patch-source "Analyzer.scala" (("+" 6461 "Inc")
       *     ("-" 7127 7128)))
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:patch-source" => {
        form match {
          case SExpList(head :: StringAtom(file) ::
            SExpList(editSexps) :: body) => {
            val edits = editSexps.map {
              case SExpList(StringAtom("+") :: IntAtom(i) ::
                StringAtom(text) :: _) => PatchInsert(i, text)
              case SExpList(StringAtom("*") :: IntAtom(i) :: IntAtom(j) ::
                StringAtom(text) :: _) => PatchReplace(i, j, text)
              case SExpList(StringAtom("-") :: IntAtom(i) :: IntAtom(j) :: _) =>
                PatchDelete(i, j)
              case _ => PatchInsert(0, "")
            }.toList
            rpcTarget.rpcPatchSource(file, edits, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:typecheck-all
       * Summary:
       *   Request immediate load and typecheck of all known sources.
       * Arguments:
       *   None
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:typecheck-all) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:typecheck-all" => {
        rpcTarget.rpcTypecheckAll(callId)
      }

      /**
       * Doc RPC:
       *   swank:format-source
       * Summary:
       *   Run the source formatter the given source files. Writes
       *   the formatted sources to the disk. Note: the client is
       *   responsible for reloading the files from disk to display
       *   to user.
       * Arguments:
       *   List of String:Filenames, absolute or relative to the project.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:format-source ("/ensime/src/Test.scala")) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:format-source" => {
        form match {
          case SExpList(head :: SExpList(filenames) :: body) => {
            val files = filenames.map(_.toString)
            rpcTarget.rpcFormatFiles(files, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:public-symbol-search
       * Summary:
       *   Search top-level symbols (types and methods) for names that
       *   contain ALL the given search keywords.
       * Arguments:
       *   List of Strings:Keywords that will be ANDed to form the query.
       *   Int:Maximum number of results to return.
       * Return:
       *   List of SymbolSearchResults
       * Example call:
       *   (:swank-rpc (swank:public-symbol-search ("java" "io" "File") 50) 42)
       * Example return:
       *   (:return (:ok ((:name "java.io.File" :local-name "File" :decl-as class
       *   :pos (:file "/Classes/classes.jar" :offset -1))) 42)
       */
      case "swank:public-symbol-search" => {
        form match {
          case SExpList(head :: SExpList(keywords) :: IntAtom(maxResults) :: body) => {
            rpcTarget.rpcPublicSymbolSearch(
              keywords.map(_.toString).toList, maxResults, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:import-suggestions
       * Summary:
       *   Search top-level types for qualified names similar to the given
       *   type names. This call can service requests for many typenames at
       *   once, but this isn't currently used in ENSIME.
       * Arguments:
       *   String:Source filename, absolute or relative to the project.
       *   Int:Character offset within that file where type name is referenced.
       *   List of String:Type names (possibly partial) for which to suggest.
       *   Int:The maximum number of results to return.
       * Return:
       *   List of Lists of SymbolSearchResults:Each list corresponds to one of the
       *     type name arguments.
       * Example call:
       *   (:swank-rpc (swank:import-suggestions
       *   "/ensime/src/main/scala/org/ensime/server/Analyzer.scala"
       *   2300 (Actor) 10) 42)
       * Example return:
       *   (:return (:ok (((:name "scala.actors.Actor" :local-name "Actor"
       *   :decl-as trait :pos (:file "/lib/scala-library.jar" :offset -1)))))
       *   42)
       */
      case "swank:import-suggestions" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) ::
            SExpList(names) :: IntAtom(maxResults) :: body) => {
            rpcTarget.rpcImportSuggestions(file, point,
              names.map(_.toString).toList, maxResults, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:completions
       * Summary:
       *   Find viable completions at given point.
       * Arguments:
       *   String:Source filename, absolute or relative to the project.
       *   Int:Character offset within that file.
       *   Int:Max number of completions to return. Value of zero denotes
       *     no limit.
       *   Bool:If non-nil, only return prefixes that match the case of the
       *     prefix.
       *   Bool:If non-nil, reload source from disk before computing completions.
       * Return:
       *   CompletionInfoList: The list of completions
       * Example call:
       *   (:swank-rpc (swank:completions
       *   "/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala
       *   22626 0 t) 42)
       * Example return:
       *   (:return (:ok (:prefix "form" :completions
       *   ((:name "form" :type-sig "SExp" :type-id 10)
       *   (:name "format" :type-sig "(String, <repeated>[Any]) => String"
       *   :type-id 11 :is-callable t))) 42))
       */
      case "swank:completions" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) ::
            IntAtom(maxResults) :: BooleanAtom(caseSens) ::
            BooleanAtom(reload) :: body) => {
            rpcTarget.rpcCompletionsAtPoint(
              file, point, maxResults, caseSens, reload, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:package-member-completion
       * Summary:
       *   Find possible completions for a given package path.
       * Arguments:
       *   String:A package path: such as "org.ensime" or "com".
       *   String:The prefix of the package member name we are looking for.
       * Return:
       *   List of PackageMemberInfoLight: List of possible completions.
       * Example call:
       *   (:swank-rpc (swank:package-member-completion org.ensime.server Server)
       *   42)
       * Example return:
       *   (:return (:ok ((:name "Server$") (:name "Server"))) 42)
       */
      case "swank:package-member-completion" => {
        form match {
          case SExpList(head :: StringAtom(path) :: StringAtom(prefix) :: body) => {
            rpcTarget.rpcPackageMemberCompletion(path, prefix, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:call-completion
       * Summary:
       *   Lookup the type information of a specific method or function
       *   type. This is used by ENSIME to retrieve detailed parameter
       *   and return type information after the user has selected a
       *   method or function completion.
       * Arguments:
       *   Int:A type id, as returned by swank:scope-completion or
       *     swank:type-completion.
       * Return:
       *   A CallCompletionInfo
       * Example call:
       *   (:swank-rpc (swank:call-completion 1)) 42)
       * Example return:
       *   (:return (:ok (:result-type (:name "Unit" :type-id 7 :full-name
       *   "scala.Unit" :decl-as class) :param-sections ((:params (("id"
       *   (:name "Int" :type-id 74 :full-name "scala.Int" :decl-as class))
       *   ("callId" (:name "Int" :type-id 74 :full-name "scala.Int"
       *   :decl-as class))))))) 42)
       */
      case "swank:call-completion" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcCallCompletion(id, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:uses-of-symbol-at-point
       * Summary:
       *   Request all source locations where indicated symbol is used in
       *   this project.
       * Arguments:
       *   String:A Scala source filename, absolute or relative to the project.
       *   Int:Character offset of the desired symbol within that file.
       * Return:
       *   List of RangePosition:Locations where the symbol is reference.
       * Example call:
       *   (:swank-rpc (swank:uses-of-symbol-at-point "Test.scala" 11334) 42)
       * Example return:
       *   (:return (:ok ((:file "RichPresentationCompiler.scala" :offset 11442
       *   :start 11428 :end 11849) (:file "RichPresentationCompiler.scala"
       *   :offset 11319 :start 11319 :end 11339))) 42)
       */
      case "swank:uses-of-symbol-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcUsesOfSymAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:type-by-id
       * Summary:
       *   Request description of the type with given type id.
       * Arguments:
       *   Int:A type id.
       * Return:
       *   A TypeIfo
       * Example call:
       *   (:swank-rpc (swank:type-by-id 1381) 42)
       * Example return:
       *   (:return (:ok (:name "Option" :type-id 1381 :full-name "scala.Option"
       *   :decl-as class :type-args ((:name "Int" :type-id 1129 :full-name "scala.Int"
       *   :decl-as class)))) 42)
       */
      case "swank:type-by-id" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcTypeById(id, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:type-by-name
       * Summary:
       *   Lookup a type description by name.
       * Arguments:
       *   String:The fully qualified name of a type.
       * Return:
       *   A TypeIfo
       * Example call:
       *   (:swank-rpc (swank:type-by-name "java.lang.String") 42)
       * Example return:
       *   (:return (:ok (:name "String" :type-id 1188 :full-name
       *   "java.lang.String" :decl-as class)) 42)
       */
      case "swank:type-by-name" => {
        form match {
          case SExpList(head :: StringAtom(name) :: body) => {
            rpcTarget.rpcTypeByName(name, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:type-by-name-at-point
       * Summary:
       *   Lookup a type by name, in a specific source context.
       * Arguments:
       *   String:The local or qualified name of the type.
       *   String:A source filename.
       *   Int or (Int, Int):A character offset (or range) in the file.a
       * Return:
       *   A TypeInfo
       * Example call:
       *   (:swank-rpc (swank:type-by-name-at-point "String"
       *   "SwankProtocol.scala" 31680) 42)
       * Example return:
       *   (:return (:ok (:name "String" :type-id 1188 :full-name
       *   "java.lang.String" :decl-as class)) 42)
       */
      case "swank:type-by-name-at-point" => {
        form match {
          case SExpList(head :: StringAtom(name) :: StringAtom(file) ::
            OffsetRangeExtractor(range) :: body) => {
            rpcTarget.rpcTypeByNameAtPoint(name, file, range, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:type-at-point
       * Summary:
       *   Lookup type of thing at given position.
       * Arguments:
       *   String:A source filename.
       *   Int or (Int, Int):A character offset (or range) in the file.
       * Return:
       *   A TypeInfo
       * Example call:
       *   (:swank-rpc (swank:type-at-point "SwankProtocol.scala"
       *    32736) 42)
       * Example return:
       *   (:return (:ok (:name "String" :type-id 1188 :full-name
       *   "java.lang.String" :decl-as class)) 42)
       */
      case "swank:type-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) ::
              OffsetRangeExtractor(range) :: body) => {
            rpcTarget.rpcTypeAtPoint(file, range, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:inspect-type-at-point
       * Summary:
       *   Lookup detailed type of thing at given position.
       * Arguments:
       *   String:A source filename.
       *   Int or (Int, Int):A character offset (or range) in the file.
       * Return:
       *   A TypeInspectInfo
       * Example call:
       *   (:swank-rpc (swank:inspect-type-at-point "SwankProtocol.scala"
       *    32736) 42)
       * Example return:
       *   (:return (:ok (:type (:name "SExpList$" :type-id 1469 :full-name
       *   "org.ensime.util.SExpList$" :decl-as object :pos
       *   (:file "SExp.scala" :offset 1877))......)) 42)
       */
      case "swank:inspect-type-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) ::
              OffsetRangeExtractor(range) :: body) => {
            rpcTarget.rpcInspectTypeAtPoint(file, range, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:inspect-type-by-id
       * Summary:
       *   Lookup detailed type description by id
       * Arguments:
       *   Int:A type id.
       * Return:
       *   A TypeInspectInfo
       * Example call:
       *   (:swank-rpc (swank:inspect-type-by-id 232) 42)
       * Example return:
       *   (:return (:ok (:type (:name "SExpList$" :type-id 1469 :full-name
       *   "org.ensime.util.SExpList$" :decl-as object :pos
       *   (:file "SExp.scala" :offset 1877))......)) 42)
       */
      case "swank:inspect-type-by-id" => {
        form match {
          case SExpList(head :: IntAtom(id) :: body) => {
            rpcTarget.rpcInspectTypeById(id, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:symbol-at-point
       * Summary:
       *   Get a description of the symbol at given location.
       * Arguments:
       *   String:A source filename.
       *   Int:A character offset in the file.
       * Return:
       *   A SymbolInfo
       * Example call:
       *   (:swank-rpc (swank:symbol-at-point "SwankProtocol.scala" 36483) 42)
       * Example return:
       *   (:return (:ok (:name "file" :type (:name "String" :type-id 25
       *   :full-name "java.lang.String" :decl-as class) :decl-pos
       *   (:file "SwankProtocol.scala" :offset 36404))) 42)
       */
      case "swank:symbol-at-point" => {
        form match {
          case SExpList(head :: StringAtom(file) :: IntAtom(point) :: body) => {
            rpcTarget.rpcSymbolAtPoint(file, point, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:inspect-package-by-path
       * Summary:
       *   Get a detailed description of the given package.
       * Arguments:
       *   String:A qualified package name.
       * Return:
       *   A PackageInfo
       * Example call:
       *   (:swank-rpc (swank:inspect-package-by-path "org.ensime.util" 36483) 42)
       * Example return:
       *   (:return (:ok (:name "util" :info-type package :full-name "org.ensime.util"
       *   :members ((:name "BooleanAtom" :type-id 278 :full-name
       *   "org.ensime.util.BooleanAtom" :decl-as class :pos
       *   (:file "SExp.scala" :offset 2848)).....))) 42)
       */
      case "swank:inspect-package-by-path" => {
        form match {
          case SExpList(head :: StringAtom(path) :: body) => {
            rpcTarget.rpcInspectPackageByPath(path, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:prepare-refactor
       * Summary:
       *   Initiate a refactoring. The server will respond with a summary
       *   of what the refactoring *would* do, were it executed.This call
       *   does not effect any changes unless the 4th argument is nil.
       * Arguments:
       *   Int:A procedure id for this refactoring, uniquely generated by client.
       *   Symbol:The manner of refacoring we want to prepare. Currently, one of
       *     rename, extractMethod, extractLocal, organizeImports, or addImport.
       *   An association list of params of the form (sym1 val1 sym2 val2).
       *     Contents of the params varies with the refactoring type:
       *     rename: (newName String file String start Int end Int)
       *     extractMethod: (methodName String file String start Int end Int)
       *     extractLocal: (name String file String start Int end Int)
       *     inlineLocal: (file String start Int end Int)
       *     organizeImports: (file String)
       *     addImport: (qualifiedName String file String start Int end Int)
       *   Bool:Should the refactoring require confirmation? If nil, the refactoring
       *     will be executed immediately.
       * Return:
       *   RefactorEffect | RefactorFailure
       * Example call:
       *   (:swank-rpc (swank:prepare-refactor 6 rename (file "SwankProtocol.scala"
       *   start 39504 end 39508 newName "dude") t) 42)
       * Example return:
       *   (:return (:ok (:procedure-id 6 :refactor-type rename :status success
       *   :changes ((:file "SwankProtocol.scala" :text "dude" :from 39504 :to 39508))
       *   )) 42)
       */
      case "swank:prepare-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: SymbolAtom(tpe) ::
            (params: SExp) :: BooleanAtom(interactive) :: body) => {
            rpcTarget.rpcPrepareRefactor(Symbol(tpe), procId,
              listOrEmpty(params).toSymbolMap, interactive, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:exec-refactor
       * Summary:
       *   Execute a refactoring, usually after user confirmation.
       * Arguments:
       *   Int:A procedure id for this refactoring, uniquely generated by client.
       *   Symbol:The manner of refacoring we want to prepare. Currently, one of
       *     rename, extractMethod, extractLocal, organizeImports, or addImport.
       * Return:
       *   RefactorResult | RefactorFailure
       * Example call:
       *   (:swank-rpc (swank:exec-refactor 7 rename) 42)
       * Example return:
       *   (:return (:ok (:procedure-id 7 :refactor-type rename
       *   :touched-files ("SwankProtocol.scala"))) 42)
       */
      case "swank:exec-refactor" => {
        form match {
          case SExpList(dude :: IntAtom(procId) :: SymbolAtom(tpe) :: body) => {
            rpcTarget.rpcExecRefactor(Symbol(tpe), procId, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:cancel-refactor
       * Summary:
       *   Cancel a refactor that's been performed but not
       *   executed.
       * Arguments:
       *   Int:Procedure Id of the refactoring.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:cancel-refactor 1) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:cancel-refactor" => {
        form match {
          case SExpList(head :: IntAtom(procId) :: body) => {
            rpcTarget.rpcCancelRefactor(procId, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:symbol-designations
       * Summary:
       *   Request the semantic classes of symbols in the given
       *   range. These classes are intended to be used for
       *   semantic highlighting.
       * Arguments:
       *   String:A source filename.
       *   Int:The character offset of the start of the input range.
       *   Int:The character offset of the end of the input range.
       *   List of Symbol:The symbol classes in which we are interested.
       *     Available classes are: var,val,varField,valField,functionCall,
       *     operator,param,class,trait,object.
       * Return:
       *   SymbolDesignations
       * Example call:
       *   (:swank-rpc (swank:symbol-designations "SwankProtocol.scala" 0 46857
       *   (var val varField valField)) 42)
       * Example return:
       *   (:return (:ok (:file "SwankProtocol.scala" :syms
       *   ((varField 33625 33634) (val 33657 33661) (val 33663 33668)
       *   (varField 34369 34378) (val 34398 34400)))) 42)
       */
      case "swank:symbol-designations" => {
        form match {
          case SExpList(head :: StringAtom(filename) :: IntAtom(start) ::
            IntAtom(end) :: (reqTypes: SExp) :: body) => {
            val requestedTypes: List[Symbol] = listOrEmpty(reqTypes).map(
              tpe => Symbol(tpe.toString)).toList
            rpcTarget.rpcSymbolDesignations(filename, start,
              end, requestedTypes, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:expand-selection
       * Summary:
       *   Given a start and end point in a file, expand the
       *   selection so that it spans the smallest syntactic
       *   scope that contains start and end.
       * Arguments:
       *   String:A source filename.
       *   Int:The character offset of the start of the input range.
       *   Int:The character offset of the end of the input range.
       * Return:
       *   A RangePosition:The expanded range.
       * Example call:
       *   (:swank-rpc (swank:expand-selection "Model.scala" 4648 4721) 42)
       * Example return:
       *   (:return (:ok (:file "Model.scala" :start 4374 :end 14085)) 42)
       */
      case "swank:expand-selection" => {
        form match {
          case SExpList(head :: StringAtom(filename) :: IntAtom(start) ::
            IntAtom(end) :: body) => {
            rpcTarget.rpcExpandSelection(filename, start, end, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:method-bytecode
       * Summary:
       *   Get bytecode for method at file and line.
       * Arguments:
       *   String:The file in which the method is defined.
       *   Int:A line within the method's code.
       * Return:
       *   A MethodBytecode
       * Example call:
       *   (:swank-rpc (swank:method-bytecode "hello.scala" 12) 42)
       * Example return:
       *   (:return
       *   (:ok (
       *   :class-name "SomeClassName"
       *   :name "SomeMethodName"
       *   :signature ??
       *   :bytcode ("opName" "opDescription" ...
       *   )
       *   42)
       */
      case "swank:method-bytecode" => {
        form match {
          case SExpList(head :: StringAtom(filename) ::
            IntAtom(line) :: body) => {
            rpcTarget.rpcMethodBytecode(filename, line, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-active-vm
       * Summary:
       *   Is a there an active vm? if so return a description.
       * Arguments:
       *   None
       * Return:
       *   Nil | A short description of the current vm.
       * Example call:
       *   (:swank-rpc (swank:debug-active-vm) 42)
       * Example return:
       *   (:return (:ok nil) 42)
       */
      case "swank:debug-active-vm" => {
        form match {
          case SExpList(head :: body) => {
            rpcTarget.rpcDebugActiveVM(callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-start
       * Summary:
       *   Start a new debug session.
       * Arguments:
       *   String:The commandline to pass to the debugger. Of the form:
       *     "package.ClassName arg1 arg2....."
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-start "org.hello.HelloWorld arg") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-start" => {
        form match {
          case SExpList(head :: StringAtom(commandLine) :: body) => {
            rpcTarget.rpcDebugStartVM(commandLine, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-attach
       * Summary:
       *   Start a new debug session on a target vm.
       * Arguments:
       *   String: The hostname of the vm
       *   String: The debug port of the vm
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-attach "localhost" "9000") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-attach" => {
        form match {
          case SExpList(head :: StringAtom(hostname) :: StringAtom(port) :: body) => {
            rpcTarget.rpcDebugAttachVM(hostname, port, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-stop
       * Summary:
       *   Stop the debug session
       * Arguments:
       *   None
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-stop) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-stop" => {
        form match {
          case SExpList(head :: body) => {
            rpcTarget.rpcDebugStopVM(callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-set-break
       * Summary:
       *   Add a breakpoint
       * Arguments:
       *   String:The file in which to set the breakpoint.
       *   Int:The breakpoint line.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-set-break "hello.scala" 12) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-set-break" => {
        form match {
          case SExpList(head :: StringAtom(filename) ::
            IntAtom(line) :: body) => {
            rpcTarget.rpcDebugBreak(filename, line, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-clear-break
       * Summary:
       *   Clear a breakpoint
       * Arguments:
       *   String:The file from which to clear the breakpoint.
       *   Int:The breakpoint line.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-clear "hello.scala" 12) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-clear-break" => {
        form match {
          case SExpList(head :: StringAtom(filename) ::
            IntAtom(line) :: body) => {
            rpcTarget.rpcDebugClearBreak(filename, line, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-clear-all-breaks
       * Summary:
       *   Clear all breakpoints
       * Arguments:
       *   None
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-clear-all-breaks) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-clear-all-breaks" => {
        form match {
          case SExpList(head :: body) => {
            rpcTarget.rpcDebugClearAllBreaks(callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-list-breakpoints
       * Summary:
       *   Get a list of all breakpoints set so far.
       * Arguments:
       *   None
       * Return:
       *   List of Position:A list of positions
       * Example call:
       *   (:swank-rpc (swank:debug-list-breakpoints) 42)
       * Example return:
       *   (:return (:ok (:file "hello.scala" :line 1)
       *   (:file "hello.scala" :line 23)) 42)
       */
      case "swank:debug-list-breakpoints" => {
        form match {
          case SExpList(head :: body) => {
            rpcTarget.rpcDebugListBreaks(callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-run
       * Summary:
       *   Resume execution of the VM.
       * Arguments:
       *   None
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-run) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-run" => {
        form match {
          case SExpList(head :: body) => {
            rpcTarget.rpcDebugRun(callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-continue
       * Summary:
       *   Resume execution of the VM.
       * Arguments:
       *   String:The thread-id to continue.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-continue "1") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-continue" => {
        form match {
          case SExpList(head :: StringAtom(threadId) :: body) => {
            rpcTarget.rpcDebugContinue(threadId.toLong, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-step
       * Summary:
       *   Step the given thread to the next line. Step into
       *     function calls.
       * Arguments:
       *   String:The thread-id to step.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-step "982398123") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-step" => {
        form match {
          case SExpList(head :: StringAtom(threadId) :: body) => {
            rpcTarget.rpcDebugStep(threadId.toLong, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-next
       * Summary:
       *   Step the given thread to the next line. Do not
       *     step into function calls.
       * Arguments:
       *   String:The thread-id to step.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-next "982398123") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-next" => {
        form match {
          case SExpList(head :: StringAtom(threadId) :: body) => {
            rpcTarget.rpcDebugNext(threadId.toLong, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-step-out
       * Summary:
       *   Step the given thread to the next line. Step out of
       *     the current function to the calling frame if necessary.
       * Arguments:
       *   String:The thread-id to step.
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:debug-step-out "982398123") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-step-out" => {
        form match {
          case SExpList(head :: StringAtom(threadId) :: body) => {
            rpcTarget.rpcDebugStepOut(threadId.toLong, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-locate-name
       * Summary:
       *   Get the binding location for the given name at this point
       *     in the program's execution.
       * Arguments:
       *   String: The thread-id in which to search.
       *   String: The name to search for.
       * Return:
       *   A DebugLocation
       * Example call:
       *   (:swank-rpc (swank:debug-locate-name "thread-2" "apple") 42)
       * Example return:
       *   (:return (:ok (:slot :thread-id "thread-2" :frame 2 :offset 0)) 42)
       */
      case "swank:debug-locate-name" => {
        form match {
          case SExpList(head :: StringAtom(threadId) :: StringAtom(name) :: body) => {
            rpcTarget.rpcDebugLocateName(threadId.toLong, name, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-value
       * Summary:
       *   Get the value at the given location.
       * Arguments:
       *   DebugLocation: The location from which to load the value.
       * Return:
       *   A DebugValue
       * Example call:
       *   (:swank-rpc (swank:debug-value (:type element
       *    :object-id "23" :index 2)) 42)
       * Example return:
       *   (:return (:ok (:val-type prim :summary "23"
       *    :type-name "Integer")) 42)
       */
      case "swank:debug-value" => {
        form match {
          case SExpList(head :: DebugLocationExtractor(loc) :: body) => {
            rpcTarget.rpcDebugValue(loc, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:debug-to-string
       * Summary:
       *   Returns the result of calling toString on the value at the
       *   given location
       * Arguments:
       *   String: The thread-id in which to call toString.
       *   DebugLocation: The location from which to load the value.
       * Return:
       *   A DebugValue
       * Example call:
       *   (:swank-rpc (swank:debug-to-string "thread-2"
       *    (:type element :object-id "23" :index 2)) 42)
       * Example return:
       *   (:return (:ok "A little lamb") 42)
       */
      case "swank:debug-to-string" => {
        form match {
          case SExpList(head :: StringAtom(threadId) :: DebugLocationExtractor(loc) :: body) => {
            rpcTarget.rpcDebugToString(threadId.toLong, loc, callId)
          }
          case _ => oops
        }
      }


      /**
       * Doc RPC:
       *   swank:debug-set-value
       * Summary:
       *   Set the value at the given location.
       * Arguments:
       *   DebugLocation: Location to set value.
       *   String: A string encoded value.
       * Return:
       *   Boolean: t on success, nil otherwise
       * Example call:
       *   (:swank-rpc (swank:debug-set-stack-var (:type element
       *    :object-id "23" :index 2) "1") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:debug-set-value" => {
        form match {
          case SExpList(head :: DebugLocationExtractor(loc) ::
            StringAtom(newValue) :: body) => {
            rpcTarget.rpcDebugSetValue(loc, newValue, callId)
          }
          case _ => oops
        }
      }


      /**
       * Doc RPC:
       *   swank:debug-backtrace
       * Summary:
       *   Get a detailed backtrace for the given thread
       * Arguments:
       *   String: The unique id of the thread.
       *   Int: The index of the first frame to list. The 0th frame is the
       *     currently executing frame.
       *   Int: The number of frames to return. -1 denotes _all_ frames.
       * Return:
       *   A DebugBacktrace
       * Example call:
       *   (:swank-rpc (swank:debug-backtrace "23" 0 2) 42)
       * Example return:
       *   (:return (:ok (:frames () :thread-id "23" :thread-name "main")) 42)
       */
      case "swank:debug-backtrace" => {
        form match {
          case SExpList(head :: StringAtom(threadId) ::
            IntAtom(index) :: IntAtom(count) :: body) => {
            rpcTarget.rpcDebugBacktrace(threadId.toLong, index, count, callId)
          }
          case _ => oops
        }
      }

      /**
       * Doc RPC:
       *   swank:shutdown-server
       * Summary:
       *   Politely ask the server to shutdown.
       * Arguments:
       *   None
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:shutdown-server) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case "swank:shutdown-server" => {
        rpcTarget.rpcShutdownServer(callId)
      }

      case other => {
        sendRPCError(
          ErrUnrecognizedRPC,
          Some("Unknown :swank-rpc call: " + other),
          callId)
      }
    }
  }

  def listOrEmpty(list: SExp): SExpList = {
    list match {
      case l: SExpList => l
      case _ => SExpList(List())
    }
  }

  def sendRPCAckOK(callId: Int) {
    sendRPCReturn(true, callId)
  }

  def sendRPCReturn(value: WireFormat, callId: Int) {
    value match {
      case sexp: SExp =>
        {
          sendMessage(SExp(
            key(":return"),
            SExp(key(":ok"), sexp),
            callId))
        }
      case _ => throw new IllegalStateException("Not a SExp: " + value)
    }
  }

  def sendEvent(value: WireFormat) {
    value match {
      case sexp: SExp => { sendMessage(sexp) }
      case _ => throw new IllegalStateException("Not a SExp: " + value)
    }
  }

  def sendRPCError(code: Int, detail: Option[String], callId: Int) {
    sendMessage(SExp(
      key(":return"),
      SExp(key(":abort"),
        code,
        detail.map(strToSExp).getOrElse(NilAtom())),
      callId))
  }

  def sendProtocolError(code: Int, detail: Option[String]) {
    sendMessage(
      SExp(
        key(":reader-error"),
        code,
        detail.map(strToSExp).getOrElse(NilAtom())))
  }

  object SExpConversion {

    implicit def posToSExp(pos: Position): SExp = {
      if (pos.isDefined) {
        SExp.propList((":file", pos.source.path), (":offset", pos.point))
      } else {
        'nil
      }
    }

    implicit def posToSExp(pos: RangePosition): SExp = {
      if (pos.isDefined) {
        SExp.propList(
          (":file", pos.source.path),
          (":offset", pos.point),
          (":start", pos.start),
          (":end", pos.end))
      } else {
        'nil
      }
    }

  }

  import SExpConversion._

  object OffsetRangeExtractor {
    def unapply(sexp: SExp): Option[OffsetRange] = sexp match {
      case IntAtom(a) => Some(OffsetRange(a, a))
      case SExpList(IntAtom(a)::IntAtom(b)::Nil) => Some(OffsetRange(a, b))
      case _ => None
    }
  }

  object DebugLocationExtractor {
    def unapply(sexp: SExpList): Option[DebugLocation] = {
      val m = sexp.toKeywordMap()
      m.get(key(":type")).flatMap {
	case SymbolAtom("reference") => {
	  for(StringAtom(id) <- m.get(key(":object-id"))) yield {
	    DebugObjectReference(id.toLong)
	  }
	}
	case SymbolAtom("field") => {
	  for(StringAtom(id) <- m.get(key(":object-id"));
	    StringAtom(field) <- m.get(key(":field"))) yield {
	    DebugObjectField(id.toLong, field)
	  }
	}
	case SymbolAtom("element") => {
	  for(StringAtom(id) <- m.get(key(":object-id"));
	    IntAtom(index) <- m.get(key(":index"))) yield {
	    DebugArrayElement(id.toLong, index)
	  }
	}
	case SymbolAtom("slot") => {
	  for(StringAtom(id) <- m.get(key(":thread-id"));
	    IntAtom(frame) <- m.get(key(":frame"));
	    IntAtom(offset) <- m.get(key(":offset"))) yield {
	    DebugStackSlot(id.toLong, frame, offset)
	  }
	}
	case _ => None
      }
    }
  }

  def toWF(obj: DebugLocation): SExp = {
    obj match {
      case obj: DebugObjectReference => toWF(obj)
      case obj: DebugArrayElement => toWF(obj)
      case obj: DebugObjectField => toWF(obj)
      case obj: DebugStackSlot => toWF(obj)
    }
  }
  def toWF(obj: DebugObjectReference): SExp = {
    SExp(
      key(":type"), 'reference,
      key(":object-id"), obj.objectId.toString)
  }
  def toWF(obj: DebugArrayElement): SExp = {
    SExp(
      key(":type"), 'element,
      key(":object-id"), obj.objectId.toString,
      key(":index"), obj.index)
  }
  def toWF(obj: DebugObjectField): SExp = {
    SExp(
      key(":type"), 'field,
      key(":object-id"), obj.objectId.toString,
      key(":field"), obj.name)
  }
  def toWF(obj: DebugStackSlot): SExp = {
    SExp(
      key(":type"), 'slot,
      key(":thread-id"), obj.threadId.toString,
      key(":frame"), obj.frame,
      key(":offset"), obj.offset)
  }

  def toWF(obj: DebugValue): SExp = {
    obj match {
      case obj: DebugPrimitiveValue => toWF(obj)
      case obj: DebugObjectInstance => toWF(obj)
      case obj: DebugArrayInstance => toWF(obj)
      case obj: DebugStringInstance => toWF(obj)
      case obj: DebugNullValue => toWF(obj)
    }
  }
  def toWF(obj: DebugNullValue): SExp = {
    SExp(
      key(":val-type"), 'null,
      key(":type-name"), obj.typeName)
  }
  def toWF(obj: DebugPrimitiveValue): SExp = {
    SExp(
      key(":val-type"), 'prim,
      key(":summary"), obj.summary,
      key(":type-name"), obj.typeName)
  }
  def toWF(obj: DebugClassField): SExp = {
    SExp(
      key(":index"), obj.index,
      key(":name"), obj.name,
      key(":summary"), obj.summary,
      key(":type-name"), obj.typeName)
  }
  def toWF(obj: DebugObjectInstance): SExp = {
    SExp(
      key(":val-type"), 'obj,
      key(":fields"), SExpList(obj.fields.map(toWF)),
      key(":type-name"), obj.typeName,
      key(":object-id"), obj.objectId.toString)
  }
  def toWF(obj: DebugStringInstance): SExp = {
    SExp(
      key(":val-type"), 'str,
      key(":summary"), obj.summary,
      key(":fields"), SExpList(obj.fields.map(toWF)),
      key(":type-name"), obj.typeName,
      key(":object-id"), obj.objectId.toString)
  }
  def toWF(obj: DebugArrayInstance): SExp = {
    SExp(
      key(":val-type"), 'arr,
      key(":length"), obj.length,
      key(":type-name"), obj.typeName,
      key(":element-type-name"), obj.elementTypeName,
      key(":object-id"), obj.objectId.toString)
  }

  def toWF(obj: DebugStackLocal): SExp = {
    SExp(
      key(":index"), obj.index,
      key(":name"), obj.name,
      key(":summary"), obj.summary,
      key(":type-name"), obj.typeName)
  }

  def toWF(obj: DebugStackFrame): SExp = {
    SExp(
      key(":index"), obj.index,
      key(":locals"), SExpList(obj.locals.map(toWF)),
      key(":num-args"), obj.numArguments,
      key(":class-name"), obj.className,
      key(":method-name"), obj.methodName,
      key(":pc-location"), toWF(obj.pcLocation),
      key(":this-object-id"), obj.thisObjectId.toString)
  }

  def toWF(obj: DebugBacktrace): SExp = {
    SExp(
      key(":frames"), SExpList(obj.frames.map(toWF)),
      key(":thread-id"), obj.threadId.toString,
      key(":thread-name"), obj.threadName)
  }

  def toWF(pos: SourcePosition): SExp = {
    SExp(
      key(":file"), pos.file.getAbsolutePath(),
      key(":line"), pos.line)
  }

  def toWF(info: ConnectionInfo): SExp = {
    SExp(
      key(":pid"), 'nil,
      key(":implementation"),
      SExp(key(":name"), info.serverName),
      key(":version"), info.protocolVersion)
  }

  def toWF(evt: SendBackgroundMessageEvent): SExp = {
    SExp(key(":background-message"), evt.code,
      evt.detail.map(strToSExp).getOrElse(NilAtom()))
  }

  /**
   * Doc Event:
   *   :compiler-ready
   * Summary:
   *   Signal that the compiler has finished its initial compilation and the server
   *   is ready to accept RPC calls.
   * Structure:
   *   (:compiler-ready)
   */
  def toWF(evt: AnalyzerReadyEvent): SExp = {
    SExp(key(":compiler-ready"))
  }

  /**
   * Doc Event:
   *   :full-typecheck-finished
   * Summary:
   *   Signal that the compiler has finished compilation of the entire project.
   * Structure:
   *   (:full-typecheck-finished)
   */
  def toWF(evt: FullTypeCheckCompleteEvent): SExp = {
    SExp(key(":full-typecheck-finished"))
  }

  /**
   * Doc Event:
   *   :indexer-ready
   * Summary:
   *   Signal that the indexer has finished indexing the classpath.
   * Structure:
   *   (:indexer-ready)
   */
  def toWF(evt: IndexerReadyEvent): SExp = {
    SExp(key(":indexer-ready"))
  }

  /**
   * Doc Event:
   *   :scala-notes
   * Summary:
   *   Notify client when Scala compiler generates errors,warnings or other notes.
   * Structure:
   *   (:scala-notes
   *   notes //List of Note
   *   )
   */
  //-----------------------
  /**
   * Doc Event:
   *   :java-notes
   * Summary:
   *   Notify client when Java compiler generates errors,warnings or other notes.
   * Structure:
   *   (:java-notes
   *   notes //List of Note
   *   )
   */
  def toWF(evt: NewNotesEvent): SExp = {
    if (evt.lang == 'scala) SExp(key(":scala-notes"), toWF(evt.notelist))
    else SExp(key(":java-notes"), toWF(evt.notelist))
  }

  /**
   * Doc Event:
   *   :clear-all-scala-notes
   * Summary:
   *   Notify client when Scala notes have become invalidated. Editor should consider
   *   all Scala related notes to be stale at this point.
   * Structure:
   *   (:clear-all-scala-notes)
   */
  //-----------------------
  /**
   * Doc Event:
   *   :clear-all-java-notes
   * Summary:
   *   Notify client when Java notes have become invalidated. Editor should consider
   *   all Java related notes to be stale at this point.
   * Structure:
   *   (:clear-all-java-notes)
   */
  def toWF(evt: ClearAllNotesEvent): SExp = {
    if (evt.lang == 'scala) SExp(key(":clear-all-scala-notes"))
    else SExp(key(":clear-all-java-notes"))
  }

  def toWF(evt: DebugEvent): SExp = {
    evt match {
      /**
       * Doc Event:
       *   :debug-event (:type output)
       * Summary:
       *   Communicates stdout/stderr of debugged VM to client.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: output
       *      :body //String: A chunk of output text
       *   ))
       */
      case DebugOutputEvent(out: String) => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'output,
            key(":body"), out))
      }

      /**
       * Doc Event:
       *   :debug-event (:type step)
       * Summary:
       *   Signals that the debugged VM has stepped to a new location and is now
       *     paused awaiting control.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: step
       *      :thread-id //String: The unique thread id of the paused thread.
       *      :thread-name //String: The informal name of the paused thread.
       *      :file //String: The source file the VM stepped into.
       *      :line //Int: The source line the VM stepped to.
       *   ))
       */
      case DebugStepEvent(threadId, threadName, pos) => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'step,
            key(":thread-id"), threadId.toString,
            key(":thread-name"), threadName,
            key(":file"), pos.file.getAbsolutePath,
            key(":line"), pos.line))
      }

      /**
       * Doc Event:
       *   :debug-event (:type breakpoint)
       * Summary:
       *   Signals that the debugged VM has stopped at a breakpoint.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: breakpoint
       *      :thread-id //String: The unique thread id of the paused thread.
       *      :thread-name //String: The informal name of the paused thread.
       *      :file //String: The source file the VM stepped into.
       *      :line //Int: The source line the VM stepped to.
       *   ))
       */
      case DebugBreakEvent(threadId, threadName, pos) => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'breakpoint,
            key(":thread-id"), threadId.toString,
            key(":thread-name"), threadName,
            key(":file"), pos.file.getAbsolutePath,
            key(":line"), pos.line))
      }

      /**
       * Doc Event:
       *   :debug-event (:type death)
       * Summary:
       *   Signals that the debugged VM has exited.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: death
       *   ))
       */
      case DebugVMDeathEvent() => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'death))
      }

      /**
       * Doc Event:
       *   :debug-event (:type start)
       * Summary:
       *   Signals that the debugged VM has started.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: start
       *   ))
       */
      case DebugVMStartEvent() => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'start))
      }

      /**
       * Doc Event:
       *   :debug-event (:type disconnect)
       * Summary:
       *   Signals that the debugger has disconnected form the debugged VM.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: disconnect
       *   ))
       */
      case DebugVMDisconnectEvent() => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'disconnect))
      }

      /**
       * Doc Event:
       *   :debug-event (:type exception)
       * Summary:
       *   Signals that the debugged VM has thrown an exception and is now paused
       *     waiting for control.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: exception
       *      :exception //String: The unique object id of the exception.
       *      :thread-id //String: The unique thread id of the paused thread.
       *      :thread-name //String: The informal name of the paused thread.
       *      :file //String: The source file where the exception was caught,
       *         or nil if no location is known.
       *      :line //Int: The source line where the exception was thrown,
       *         or nil if no location is known.
       *   ))
       */
      case DebugExceptionEvent(excId, threadId, threadName, maybePos) => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'exception,
            key(":exception"), excId.toString,
            key(":thread-id"), threadId.toString,
            key(":thread-name"), threadName,
            key(":file"), maybePos.map { p =>
              StringAtom(p.file.getAbsolutePath)
            }.getOrElse('nil),
            key(":line"), maybePos.map { p =>
              IntAtom(p.line)
            }.getOrElse('nil)))
      }

      /**
       * Doc Event:
       *   :debug-event (:type threadStart)
       * Summary:
       *   Signals that a new thread has started.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: threadStart
       *      :thread-id //String: The unique thread id of the new thread.
       *   ))
       */
      case DebugThreadStartEvent(threadId: Long) => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'threadStart,
            key(":thread-id"), threadId.toString))
      }

      /**
       * Doc Event:
       *   :debug-event (:type threadDeath)
       * Summary:
       *   Signals that a new thread has died.
       * Structure:
       *   (:debug-event
       *     (:type //Symbol: threadDeath
       *      :thread-id //String: The unique thread id of the new thread.
       *   ))
       */
      case DebugThreadDeathEvent(threadId: Long) => {
        SExp(key(":debug-event"),
          SExp(key(":type"), 'threadDeath,
            key(":thread-id"), threadId.toString))
      }
      case _ => SExp(key(":debug-event"))
    }

  }

  def toWF(bp: Breakpoint): SExp = {
    SExp(
      key(":file"), bp.pos.file.getAbsolutePath,
      key(":line"), bp.pos.line)
  }

  def toWF(bps: BreakpointList): SExp = {
    SExp(
      key(":active"), SExpList(bps.active.map { toWF(_) }),
      key(":pending"), SExpList(bps.pending.map { toWF(_) }))
  }

  def toWF(config: ProjectConfig): SExp = {
    SExp(
      key(":project-name"), config.name.map(StringAtom).getOrElse('nil),
      key(":source-roots"), SExp(
        (config.sourceRoots ++ config.referenceSourceRoots).map {
          f => StringAtom(f.getPath)
        }))
  }

  def toWF(config: ReplConfig): SExp = {
    SExp.propList((":classpath", strToSExp(config.classpath)))
  }

  def toWF(value: Boolean): SExp = {
    if (value) TruthAtom()
    else NilAtom()
  }

  def toWF(value: Null): SExp = {
    NilAtom()
  }

  def toWF(value: String): SExp = {
    StringAtom(value)
  }

  def toWF(note: Note): SExp = {
    SExp(
      key(":severity"), note.friendlySeverity,
      key(":msg"), note.msg,
      key(":beg"), note.beg,
      key(":end"), note.end,
      key(":line"), note.line,
      key(":col"), note.col,
      key(":file"), note.file)
  }

  def toWF(notelist: NoteList): SExp = {
    val NoteList(isFull, notes) = notelist
    SExp(
      key(":is-full"),
      toWF(isFull),
      key(":notes"),
      SExpList(notes.map(toWF)))
  }

  def toWF(values: Iterable[WireFormat]): SExp = {
    SExpList(values.map(ea => ea.asInstanceOf[SExp]))
  }

  def toWF(value: CompletionSignature): SExp = {
    SExp(
      SExp(value.sections.map { section =>
        SExpList(section.map { param =>
          SExp(param._1, param._2)
        })
      }),
      value.result)
  }

  def toWF(value: CompletionInfo): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type-sig", toWF(value.tpeSig)),
      (":type-id", value.tpeId),
      (":is-callable", value.isCallable),
      (":to-insert", value.toInsert.map(strToSExp).getOrElse('nil)))
  }

  def toWF(value: CompletionInfoList): SExp = {
    SExp.propList(
      (":prefix", value.prefix),
      (":completions", SExpList(value.completions.map(toWF))))
  }

  def toWF(value: PackageMemberInfoLight): SExp = {
    SExp(key(":name"), value.name)
  }

  def toWF(value: SymbolInfo): SExp = {
    SExp.propList(
      (":name", value.name),
      (":local-name", value.localName),
      (":type", toWF(value.tpe)),
      (":decl-pos", value.declPos),
      (":is-callable", value.isCallable),
      (":owner-type-id", value.ownerTypeId.map(intToSExp).getOrElse('nil)))
  }

  def toWF(value: FileRange): SExp = {
    SExp.propList(
      (":file", value.file),
      (":start", value.start),
      (":end", value.end))
  }

  def toWF(value: Position): SExp = {
    posToSExp(value)
  }

  def toWF(value: RangePosition): SExp = {
    posToSExp(value)
  }

  def toWF(value: NamedTypeMemberInfoLight): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type-sig", value.tpeSig),
      (":type-id", value.tpeId),
      (":is-callable", value.isCallable))
  }

  def toWF(value: NamedTypeMemberInfo): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type", toWF(value.tpe)),
      (":pos", value.pos),
      (":decl-as", value.declaredAs))
  }

  def toWF(value: EntityInfo): SExp = {
    value match {
      case value: PackageInfo => toWF(value)
      case value: TypeInfo => toWF(value)
      case value: NamedTypeMemberInfo => toWF(value)
      case value: NamedTypeMemberInfoLight => toWF(value)
      case value => throw new IllegalStateException("Unknown EntityInfo: " + value)
    }
  }

  def toWF(value: TypeInfo): SExp = {
    value match {
      case value: ArrowTypeInfo =>
        {
          SExp.propList(
            (":name", value.name),
            (":type-id", value.id),
            (":arrow-type", true),
            (":result-type", toWF(value.resultType)),
            (":param-sections", SExp(value.paramSections.map(toWF))))
        }
      case value: TypeInfo =>
        {
          SExp.propList((":name", value.name),
            (":type-id", value.id),
            (":full-name", value.fullName),
            (":decl-as", value.declaredAs),
            (":type-args", SExp(value.args.map(toWF))),
            (":members", SExp(value.members.map(toWF))),
            (":pos", value.pos),
            (":outer-type-id", value.outerTypeId.map(intToSExp).getOrElse('nil)))
        }
      case value => throw new IllegalStateException("Unknown TypeInfo: " + value)
    }
  }

  def toWF(value: PackageInfo): SExp = {
    SExp.propList((":name", value.name),
      (":info-type", 'package),
      (":full-name", value.fullname),
      (":members", SExpList(value.members.map(toWF))))
  }

  def toWF(value: CallCompletionInfo): SExp = {
    SExp.propList(
      (":result-type", toWF(value.resultType)),
      (":param-sections", SExp(value.paramSections.map(toWF))))
  }

  def toWF(value: ParamSectionInfo): SExp = {
    SExp.propList(
      (":params", SExp(value.params.map {
        case (nm, tp) => SExp(nm, toWF(tp))
      })),
      (":is-implicit", value.isImplicit))

  }

  def toWF(value: InterfaceInfo): SExp = {
    SExp.propList(
      (":type", toWF(value.tpe)),
      (":via-view", value.viaView.map(strToSExp).getOrElse('nil)))
  }

  def toWF(value: TypeInspectInfo): SExp = {
    SExp.propList(
      (":type", toWF(value.tpe)),
      (":info-type", 'typeInspect),
      (":companion-id", value.companionId match {
        case Some(id) => id
        case None => 'nil
      }), (":interfaces", SExp(value.supers.map(toWF))))
  }

  def toWF(value: RefactorFailure): SExp = {
    SExp.propList(
      (":procedure-id", value.procedureId),
      (":status", 'failure),
      (":reason", value.message))
  }

  def toWF(value: RefactorEffect): SExp = {
    SExp.propList(
      (":procedure-id", value.procedureId),
      (":refactor-type", value.refactorType),
      (":status", 'success),
      (":changes", SExpList(value.changes.map(changeToWF))))
  }

  def toWF(value: RefactorResult): SExp = {
    SExp.propList(
      (":procedure-id", value.procedureId),
      (":refactor-type", value.refactorType),
      (":status", 'success),
      (":touched-files", SExpList(value.touched.map(f => strToSExp(f.getAbsolutePath)))))
  }

  def toWF(value: SymbolSearchResults): SExp = {
    SExpList(value.syms.map(toWF))
  }

  def toWF(value: ImportSuggestions): SExp = {
    SExpList(value.symLists.map { l => SExpList(l.map(toWF)) })
  }

  private def toWF(pos: Option[(String, Int)]): SExp = {
    pos match {
      case Some((f, o)) => SExp.propList((":file", f), (":offset", o))
      case _ => 'nil
    }
  }

  def toWF(value: SymbolSearchResult): SExp = {
    value match {
      case value: TypeSearchResult => {
        SExp.propList(
          (":name", value.name),
          (":local-name", value.localName),
          (":decl-as", value.declaredAs),
          (":pos", toWF(value.pos)))
      }
      case value: MethodSearchResult => {
        SExp.propList(
          (":name", value.name),
          (":local-name", value.localName),
          (":decl-as", value.declaredAs),
          (":pos", toWF(value.pos)),
          (":owner-name", value.owner))
      }
      case value => throw new IllegalStateException(
        "Unknown SymbolSearchResult: " + value)
    }

  }

  def toWF(value: Undo): SExp = {
    SExp.propList(
      (":id", value.id),
      (":changes", SExpList(value.changes.map(changeToWF))),
      (":summary", value.summary))
  }

  def toWF(value: UndoResult): SExp = {
    SExp.propList(
      (":id", value.id),
      (":touched-files", SExpList(value.touched.map(f => strToSExp(f.getAbsolutePath)))))
  }

  def toWF(value: SymbolDesignations): SExp = {
    SExp.propList(
      (":file", value.file),
      (":syms",
        SExpList(value.syms.map { s =>
          SExpList(List(s.symType, s.start, s.end))
        })))
  }

  def toWF(vmStatus: DebugVmStatus): SExp = {
    vmStatus match {
      case DebugVmSuccess() => SExp(
        key(":status"), ("success"))
      case DebugVmError(code, details) => SExp(
        key(":status"), ("error"),
        key(":error-code"), (code),
        key(":details"), (details))
    }
  }

  def toWF(method: MethodBytecode): SExp = {
    SExp.propList(
      (":class-name", method.className),
      (":name", method.methodName),
      (":signature", method.methodSignature.map(strToSExp).getOrElse('nil)),
      (":bytecode", SExpList(method.byteCode.map { op =>
        SExp(op.op, op.description)
      })))
  }

  private def changeToWF(ch: FileEdit): SExp = {
    SExp.propList(
      (":file", ch.file.getCanonicalPath()),
      (":text", ch.text),
      (":from", ch.from),
      (":to", ch.to))
  }

}
