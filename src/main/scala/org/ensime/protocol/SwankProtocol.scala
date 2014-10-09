package org.ensime.protocol

import java.io._

import akka.actor.ActorRef
import org.ensime.model._
import org.ensime.server._
import org.ensime.util.SExp._
import org.ensime.util._
import org.slf4j.LoggerFactory

import scala.util.parsing.input

class SwankProtocol extends Protocol {
  import SwankProtocol._

  /**
   * Protocol Version: 0.8.9
   *
   * Protocol Change Log:
   *   0.8.9
   *     Remove Incremental builder - removed
   *       swank:builder-init
   *       swank:builder-update-files
   *       swank:builder-add-files
   *       swank:builder-remove-files
   *   0.8.8
   *     Add optional :archive member to Position and RangePosition
   *   0.8.7
   *     Add optional file contents parameter to typecheck-file
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
   *     Protocol is now explicitly UTF-8
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

  import org.ensime.protocol.ProtocolConst._

  val log = LoggerFactory.getLogger(this.getClass)
  override val conversions: ProtocolConversions = new SwankProtocolConversions

  private var outPeer: ActorRef = null
  private var rpcTarget: RPCTarget = null

  def peer = outPeer

  def setOutputActor(peer: ActorRef) { outPeer = peer }

  def setRPCTarget(target: RPCTarget) { this.rpcTarget = target }

  // Handle reading / writing of messages

  def writeMessage(value: WireFormat, out: OutputStream): Unit = {
    val dataString: String = value.toWireString
    val data: Array[Byte] = dataString.getBytes("UTF-8")
    val header: Array[Byte] = "%06x".format(data.length).getBytes("UTF-8")
    log.info("Writing: " + dataString)
    out.write(header)
    out.write(data)
    out.flush()
  }

  private def fillArray(in: java.io.Reader, a: Array[Char]): Unit = {
    var n = 0
    val l = a.length
    var charsRead = 0
    while (n < l) {
      charsRead = in.read(a, n, l - n)
      if (charsRead == -1) {
        throw new EOFException("End of file reached in socket reader.")
      } else {
        n += charsRead
      }
    }
  }

  private val headerBuf = new Array[Char](6)

  def readMessage(reader: java.io.InputStreamReader): WireFormat = {
    fillArray(reader, headerBuf)
    val msglen = Integer.valueOf(new String(headerBuf), 16).intValue()
    if (msglen > 0) {
      //TODO allocating a new array each time is inefficient!
      val buf: Array[Char] = new Array[Char](msglen)
      fillArray(reader, buf)
      SExpParser.read(new input.CharArrayReader(buf))
    } else {
      throw new IllegalStateException("Empty message read from socket!")
    }
  }

  def handleIncomingMessage(msg: Any): Unit = {
    msg match {
      case sexp: SExp => handleMessageForm(sexp)
      case _ => log.error("Unexpected message type: " + msg)
    }
  }

  private def handleMessageForm(sexp: SExp): Unit = {
    val msgStr = sexp.toString
    val displayStr = if (msgStr.length > 500)
      msgStr.take(500) + "..."
    else
      msgStr

    log.info("Received msg: " + displayStr)
    sexp match {
      case SExpList(KeywordAtom(":swank-rpc") :: form :: IntAtom(callId) :: Nil) =>
        handleEmacsRex(form, callId)
      case _ =>
        sendProtocolError(ErrUnrecognizedForm, sexp.toString)
    }
  }

  private def handleEmacsRex(form: SExp, callId: Int): Unit = {
    form match {
      case l @ SExpList(SymbolAtom(name) :: rest) =>
        try {
          handleRPCRequest(name, l.items.drop(1), l, callId)
        } catch {
          case e: Throwable =>
            log.error("Exception whilst handling rpc " + e.getMessage, e)
            sendRPCError(ErrExceptionInRPC, e.getMessage, callId)
        }
      case _ =>
        sendRPCError(ErrMalformedRPC, "Expecting leading symbol in: " + form, callId)
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
   *     :file    //String:A filename. If :archive is set, :file is the entry within the archive
   *     :archive //String(optional): If set, a jar or zip archive that contains :file
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
   *     :file   //String:A filename. If :archive is set, :file is the entry within the archive
   *     :archive //String(optional): If set, a jar or zip archive that contains :file
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

  private def handleRPCRequest(callType: String, args: List[SExp], fullForm: SExp, callId: Int): Unit = {

    def oops(): Unit = sendRPCError(ErrMalformedRPC, "Malformed " + callType + " call: " + fullForm, callId)

    (callType, args) match {
      //    callType match {

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
      case ("swank:connection-info", Nil) =>
        rpcTarget.rpcConnectionInfo(callId)

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
       *   ("-Ywarn-dead-code" "-Ywarn-catches" "-Xstrict-warnings")
       *   :root-dir "/Users/aemon/projects/ensime/")) 42)
       * Example return:
       *   (:return (:ok (:project-name "ensime" :source-roots
       *   ("/Users/aemon/projects/ensime/src/main/scala"
       *   "/Users/aemon/projects/ensime/src/test/scala"
       *   "/Users/aemon/projects/ensime/src/main/java"))) 42)
       */
      case ("swank:init-project", (conf: SExpList) :: Nil) =>
        rpcTarget.rcpInitProject(conf, callId)

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
      case ("swank:peek-undo", Nil) =>
        rpcTarget.rpcPeekUndo(callId)

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
      case ("swank:exec-undo", IntAtom(id) :: Nil) =>
        rpcTarget.rpcExecUndo(id, callId)

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
      case ("swank:repl-config", Nil) =>
        rpcTarget.rpcReplConfig(callId)

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
      case ("swank:remove-file", StringAtom(file) :: Nil) =>
        rpcTarget.rpcRemoveFile(file, callId)

      /**
       * Doc RPC:
       *   swank:typecheck-file
       * Summary:
       *   Request immediate load and check the given source file.
       * Arguments:
       *   String:A filename, absolute or relative to the project.
       *   String(optional): if set, it is substituted for the file's contents
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:typecheck-file "Analyzer.scala") 42)
       *   (:swank-rpc (swank:typecheck-file "Analyzer.scala" "FILE CONTENTS") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case ("swank:typecheck-file", StringAtom(file) :: StringAtom(contents) :: Nil) =>
        rpcTarget.rpcTypecheckFiles(List(SourceFileInfo(new File(file), Some(contents))), callId)
      case ("swank:typecheck-file", StringAtom(file) :: Nil) =>
        rpcTarget.rpcTypecheckFiles(List(SourceFileInfo(file)), callId)

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
       *   (:swank-rpc (swank:typecheck-files ("Analyzer.scala" "Foo.scala")) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case ("swank:typecheck-files", StringListExtractor(names) :: Nil) =>
        rpcTarget.rpcTypecheckFiles(names.map(SourceFileInfo(_)), callId)

      /**
       * Doc RPC:
       *   swank:patch-source
       * Summary:
       *   Patch the source with the given changes.
       * Arguments:
       *   String:A filename
       *   A FilePatch
       * Return:
       *   None
       * Example call:
       *   (swank:patch-source "Analyzer.scala" (("+" 6461 "Inc") ("-" 7127 7128) ("*" 7200 7300 "Bob")) )
       * Example return:
       *   (:return (:ok t) 42)
       */
      case ("swank:patch-source", StringAtom(file) :: PatchListExtractor(edits) :: Nil) =>
        rpcTarget.rpcPatchSource(file, edits, callId)

      /**
       * Doc RPC:
       *   swank:unload-all
       * Summary:
       *   Remove all sources from the presentation compiler. Reset the project's symbols
       *    to the ones defiled in .class files.
       * Arguments:
       *   None
       * Return:
       *   None
       * Example call:
       *   (:swank-rpc (swank:unload-all) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case ("swank:unload-all", Nil) =>
        rpcTarget.rpcUnloadAll(callId)

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
      case ("swank:typecheck-all", Nil) =>
        rpcTarget.rpcTypecheckAll(callId)

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
      case ("swank:format-source", StringListExtractor(filenames) :: Nil) =>
        rpcTarget.rpcFormatFiles(filenames, callId)

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
      case ("swank:public-symbol-search", StringListExtractor(keywords) :: IntAtom(maxResults) :: Nil) =>
        rpcTarget.rpcPublicSymbolSearch(keywords, maxResults, callId)

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
       *   2300 ("Actor") 10) 42)
       * Example return:
       *   (:return (:ok (((:name "akka.actor.Actor" :local-name "Actor"
       *   :decl-as trait :pos (:file "/lib/scala-library.jar" :offset -1)))))
       *   42)
       */
      case ("swank:import-suggestions", StringAtom(file) :: IntAtom(point) :: StringListExtractor(names) :: IntAtom(maxResults) :: Nil) =>
        rpcTarget.rpcImportSuggestions(file, point, names, maxResults, callId)

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
       *   "/ensime/src/main/scala/org/ensime/protocol/SwankProtocol.scala"
       *   22626 0 t t) 42)
       * Example return:
       *   (:return (:ok (:prefix "form" :completions
       *   ((:name "form" :type-sig "SExp" :type-id 10)
       *   (:name "format" :type-sig "(String, <repeated>[Any]) => String"
       *   :type-id 11 :is-callable t))) 42))
       */
      case ("swank:completions", StringAtom(file) :: IntAtom(point) :: IntAtom(maxResults) ::
        BooleanAtom(caseSens) :: BooleanAtom(reload) :: Nil) =>
        rpcTarget.rpcCompletionsAtPoint(file, point, maxResults, caseSens, reload, callId)

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
       *   (:swank-rpc (swank:package-member-completion "org.ensime.server" "Server") 42)
       * Example return:
       *   (:return (:ok ((:name "Server$") (:name "Server"))) 42)
       */
      case ("swank:package-member-completion", StringAtom(path) :: StringAtom(prefix) :: Nil) =>
        rpcTarget.rpcPackageMemberCompletion(path, prefix, callId)

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
       *   (:swank-rpc (swank:call-completion 1) 42)
       * Example return:
       *   (:return (:ok (:result-type (:name "Unit" :type-id 7 :full-name
       *   "scala.Unit" :decl-as class) :param-sections ((:params (("id"
       *   (:name "Int" :type-id 74 :full-name "scala.Int" :decl-as class))
       *   ("callId" (:name "Int" :type-id 74 :full-name "scala.Int"
       *   :decl-as class))))))) 42)
       */
      case ("swank:call-completion", IntAtom(id) :: Nil) =>
        rpcTarget.rpcCallCompletion(id, callId)

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
      case ("swank:uses-of-symbol-at-point", StringAtom(file) :: IntAtom(point) :: Nil) =>
        rpcTarget.rpcUsesOfSymAtPoint(file, point, callId)

      /**
       * Doc RPC:
       *   swank:member-by-name
       * Summary:
       *   Request description of a member of a given type
       * Arguments:
       *   String: Type full-name as return by swank:typeInfo
       *   String: Member name
       *   Boolean: true if the member is a trait or class, flase otherwise
       * Return:
       *   A SymbolInfo
       * Example call:
       *   (:swank-rpc (swank:member-by-name "com.example.Class", "x", nil) 42)
       * Example return:
       *   (:return (:ok (:name "x" :local-name "x" :type (:name "String" :type-id 25
       *   :full-name "java.lang.String" :decl-as class) :decl-pos
       *   (:file "SwankProtocol.scala" :offset 36404))) 42)
       */
      case ("swank:member-by-name", StringAtom(typeFullName) :: StringAtom(memberName) :: BooleanAtom(memberIsType) :: Nil) =>
        rpcTarget.rpcMemberByName(typeFullName, memberName, memberIsType, callId)

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
      case ("swank:type-by-id", IntAtom(id) :: Nil) =>
        rpcTarget.rpcTypeById(id, callId)

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
      case ("swank:type-by-name", StringAtom(name) :: Nil) =>
        rpcTarget.rpcTypeByName(name, callId)

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
       *   (:swank-rpc (swank:type-by-name-at-point "String" "SwankProtocol.scala" 31680) 42)
       *   (:swank-rpc (swank:type-by-name-at-point "String" "SwankProtocol.scala" (31680 31691)) 42)
       * Example return:
       *   (:return (:ok (:name "String" :type-id 1188 :full-name
       *   "java.lang.String" :decl-as class)) 42)
       */
      case ("swank:type-by-name-at-point", StringAtom(name) :: StringAtom(file) :: OffsetRangeExtractor(range) :: Nil) =>
        rpcTarget.rpcTypeByNameAtPoint(name, file, range, callId)

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
       *   (:swank-rpc (swank:type-at-point "SwankProtocol.scala" 32736) 42)
       * Example return:
       *   (:return (:ok (:name "String" :type-id 1188 :full-name
       *   "java.lang.String" :decl-as class)) 42)
       */
      case ("swank:type-at-point", StringAtom(file) :: OffsetRangeExtractor(range) :: Nil) =>
        rpcTarget.rpcTypeAtPoint(file, range, callId)

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
       *   (:swank-rpc (swank:inspect-type-at-point "SwankProtocol.scala" 32736) 42)
       *   (:swank-rpc (swank:inspect-type-at-point "SwankProtocol.scala" (32736 32740)) 42)
       * Example return:
       *   (:return (:ok (:type (:name "SExpList$" :type-id 1469 :full-name
       *   "org.ensime.util.SExpList$" :decl-as object :pos
       *   (:file "SExp.scala" :offset 1877))......)) 42)
       */
      case ("swank:inspect-type-at-point", StringAtom(file) :: OffsetRangeExtractor(range) :: Nil) =>
        rpcTarget.rpcInspectTypeAtPoint(file, range, callId)

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
      case ("swank:inspect-type-by-id", IntAtom(id) :: Nil) =>
        rpcTarget.rpcInspectTypeById(id, callId)

      /**
       * Doc RPC:
       *   swank:inspect-type-by-name
       * Summary:
       *   Lookup detailed type description by its full name
       * Arguments:
       *   String: Type full-name as return by swank:typeInfo
       * Return:
       *   A TypeInspectInfo
       * Example call:
       *   (:swank-rpc (swank:inspect-type-by-name "abc.d") 42)
       * Example return:
       *   (:return (:ok (:type (:name "SExpList$" :type-id 1469 :full-name
       *   "abc.d" :decl-as class :pos
       *   (:file "SExp.scala" :offset 1877))......)) 42)
       */
      case ("swank:inspect-type-by-name", StringAtom(name) :: Nil) =>
        rpcTarget.rpcInspectTypeByName(name, callId)

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
      case ("swank:symbol-at-point", StringAtom(file) :: IntAtom(point) :: Nil) =>
        rpcTarget.rpcSymbolAtPoint(file, point, callId)

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
       *   (:swank-rpc (swank:inspect-package-by-path "org.ensime.util") 42)
       * Example return:
       *   (:return (:ok (:name "util" :info-type package :full-name "org.ensime.util"
       *   :members ((:name "BooleanAtom" :type-id 278 :full-name
       *   "org.ensime.util.BooleanAtom" :decl-as class :pos
       *   (:file "SExp.scala" :offset 2848)).....))) 42)
       */
      case ("swank:inspect-package-by-path", StringAtom(path) :: Nil) =>
        rpcTarget.rpcInspectPackageByPath(path, callId)

      /**
       * Doc RPC:
       *   swank:prepare-refactor
       * Summary:
       *   Initiate a refactoring. The server will respond with a summary
       *   of what the refactoring *would* do, were it executed.This call
       *   does not effect any changes unless the 4th argument is nil.
       * Arguments:
       *   Int:A procedure id for this refactoring, uniquely generated by client.
       *   Symbol:The manner of refactoring we want to prepare. Currently, one of
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
      case ("swank:prepare-refactor", IntAtom(procId) :: SymbolAtom(tpe) :: SymbolMapExtractor(params) :: BooleanAtom(interactive) :: Nil) =>
        parseRefactor(tpe, params) match {
          case Right(refactor) =>
            rpcTarget.rpcPrepareRefactor(procId, refactor, interactive, callId)
          case Left(msg) =>
            sendRPCError(ErrMalformedRPC, "Malformed prepare-refactor - " + msg + ": " + fullForm, callId)
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
      case ("swank:exec-refactor", IntAtom(procId) :: SymbolAtom(tpe) :: Nil) =>
        rpcTarget.rpcExecRefactor(procId, Symbol(tpe), callId)

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
      case ("swank:cancel-refactor", IntAtom(procId) :: Nil) =>
        rpcTarget.rpcCancelRefactor(procId, callId)

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
      case ("swank:symbol-designations", StringAtom(filename) :: IntAtom(start) :: IntAtom(end) :: SymbolListExtractor(requestedTypes) :: Nil) =>
        rpcTarget.rpcSymbolDesignations(filename, start, end, requestedTypes, callId)

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
      case ("swank:expand-selection", StringAtom(filename) :: IntAtom(start) :: IntAtom(end) :: Nil) =>
        rpcTarget.rpcExpandSelection(filename, start, end, callId)

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
      case ("swank:method-bytecode", StringAtom(filename) :: IntAtom(line) :: Nil) =>
        rpcTarget.rpcMethodBytecode(filename, line, callId)

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
      case ("swank:debug-active-vm", Nil) =>
        rpcTarget.rpcDebugActiveVM(callId)

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
      case ("swank:debug-start", StringAtom(commandLine) :: Nil) =>
        rpcTarget.rpcDebugStartVM(commandLine, callId)

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
      case ("swank:debug-attach", StringAtom(hostname) :: StringAtom(port) :: Nil) =>
        rpcTarget.rpcDebugAttachVM(hostname, port, callId)

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
      case ("swank:debug-stop", Nil) =>
        rpcTarget.rpcDebugStopVM(callId)

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
      case ("swank:debug-set-break", StringAtom(filename) :: IntAtom(line) :: Nil) =>
        rpcTarget.rpcDebugBreak(filename, line, callId)

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
       *   (:swank-rpc (swank:debug-clear-break "hello.scala" 12) 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case ("swank:debug-clear-break", StringAtom(filename) :: IntAtom(line) :: Nil) =>
        rpcTarget.rpcDebugClearBreak(filename, line, callId)

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
      case ("swank:debug-clear-all-breaks", Nil) =>
        rpcTarget.rpcDebugClearAllBreaks(callId)

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
      case ("swank:debug-list-breakpoints", Nil) =>
        rpcTarget.rpcDebugListBreaks(callId)

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
      case ("swank:debug-run", Nil) =>
        rpcTarget.rpcDebugRun(callId)

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
      case ("swank:debug-continue", StringAtom(threadId) :: Nil) =>
        rpcTarget.rpcDebugContinue(threadId.toLong, callId)

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
      case ("swank:debug-step", StringAtom(threadId) :: Nil) =>
        rpcTarget.rpcDebugStep(threadId.toLong, callId)

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
      case ("swank:debug-next", StringAtom(threadId) :: Nil) =>
        rpcTarget.rpcDebugNext(threadId.toLong, callId)

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
      case ("swank:debug-step-out", StringAtom(threadId) :: Nil) =>
        rpcTarget.rpcDebugStepOut(threadId.toLong, callId)

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
       *   (:swank-rpc (swank:debug-locate-name "7" "apple") 42)
       * Example return:
       *   (:return (:ok (:slot :thread-id "7" :frame 2 :offset 0)) 42)
       */
      case ("swank:debug-locate-name", StringAtom(threadId) :: StringAtom(name) :: Nil) =>
        rpcTarget.rpcDebugLocateName(threadId.toLong, name, callId)

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
      case ("swank:debug-value", DebugLocationExtractor(loc) :: Nil) =>
        rpcTarget.rpcDebugValue(loc, callId)

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
      case ("swank:debug-to-string", StringAtom(threadId) :: DebugLocationExtractor(loc) :: Nil) =>
        rpcTarget.rpcDebugToString(threadId.toLong, loc, callId)

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
       *   (:swank-rpc (swank:debug-set-value (:type element
       *    :object-id "23" :index 2) "1") 42)
       * Example return:
       *   (:return (:ok t) 42)
       */
      case ("swank:debug-set-value", DebugLocationExtractor(loc) :: StringAtom(newValue) :: Nil) =>
        rpcTarget.rpcDebugSetValue(loc, newValue, callId)

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
      case ("swank:debug-backtrace", StringAtom(threadId) :: IntAtom(index) :: IntAtom(count) :: Nil) =>
        rpcTarget.rpcDebugBacktrace(threadId.toLong, index, count, callId)

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
      case ("swank:shutdown-server", Nil) =>
        rpcTarget.rpcShutdownServer(callId)

      case other =>
        oops()
    }
  }

  def sendRPCAckOK(callId: Int): Unit = {
    sendRPCReturn(true, callId)
  }

  def sendRPCReturn(value: WireFormat, callId: Int): Unit = {
    value match {
      case sexp: SExp =>
        sendMessage(SExp(key(":return"), SExp(key(":ok"), sexp), callId))
      case _ => throw new IllegalStateException("Not a SExp: " + value)
    }
  }

  override def sendEvent(event: SwankEvent): Unit = {
    sendMessage(conversions.toWF(event))
  }

  def sendRPCError(code: Int, detail: String, callId: Int): Unit = {
    sendMessage(SExp(key(":return"), SExp(key(":abort"), code, detail), callId))
  }

  def sendProtocolError(code: Int, detail: String): Unit = {
    sendMessage(SExp(key(":reader-error"), code, detail))
  }

  object SymbolMapExtractor {
    def unapply(l: SExpList): Option[Map[Symbol, Any]] = l.toSymbolMap
  }

  object StringListExtractor {
    def unapply(l: SExpList): Option[List[String]] = l.toStringList
  }
}

object SwankProtocol {
  object OffsetRangeExtractor {
    def unapply(sexp: SExp): Option[OffsetRange] = sexp match {
      case IntAtom(a) => Some(OffsetRange(a, a))
      case SExpList(IntAtom(a) :: IntAtom(b) :: Nil) => Some(OffsetRange(a, b))
      case _ => None
    }
  }

  object PatchListExtractor {
    def unapply(sexpList: SExpList): Option[List[PatchOp]] = {
      val edits = sexpList.map {
        case SExpList(StringAtom("+") :: IntAtom(i) :: StringAtom(text) :: Nil) =>
          PatchInsert(i, text)
        case SExpList(StringAtom("*") :: IntAtom(i) :: IntAtom(j) :: StringAtom(text) :: Nil) =>
          PatchReplace(i, j, text)
        case SExpList(StringAtom("-") :: IntAtom(i) :: IntAtom(j) :: Nil) =>
          PatchDelete(i, j)
        // short circuit on unknown elements - return is a bit evil
        case _ => return None
      }
      Some(edits.toList)
    }
  }

  object SymbolListExtractor {
    def unapply(l: SExpList): Option[List[Symbol]] = {
      Some(l.items.map {
        case SymbolAtom(value) => Symbol(value)
        // this is a bit evil
        case _ => return None
      })
    }
  }

  object DebugLocationExtractor {
    def unapply(sexp: SExpList): Option[DebugLocation] = {
      val m = sexp.toKeywordMap
      m.get(key(":type")).flatMap {
        case SymbolAtom("reference") =>
          for (StringAtom(id) <- m.get(key(":object-id"))) yield {
            DebugObjectReference(id.toLong)
          }
        case SymbolAtom("field") =>
          for (
            StringAtom(id) <- m.get(key(":object-id"));
            StringAtom(field) <- m.get(key(":field"))
          ) yield {
            DebugObjectField(id.toLong, field)
          }
        case SymbolAtom("element") =>
          for (
            StringAtom(id) <- m.get(key(":object-id"));
            IntAtom(index) <- m.get(key(":index"))
          ) yield {
            DebugArrayElement(id.toLong, index)
          }
        case SymbolAtom("slot") =>
          for (
            StringAtom(id) <- m.get(key(":thread-id"));
            IntAtom(frame) <- m.get(key(":frame"));
            IntAtom(offset) <- m.get(key(":offset"))
          ) yield {
            DebugStackSlot(id.toLong, frame, offset)
          }
        case _ => None
      }
    }
  }

  import org.ensime.util.{ Symbols => S }

  def parseRefactor(tpe: String, params: Map[Symbol, Any]): Either[String, RefactorDesc] = {
    // a bit ugly, but we cant match the map - so match a sorted list, so expected tokens have to be in lexographic order
    val orderedParams = params.toList.sortBy(_._1.name)
    (tpe, orderedParams) match {
      case ("rename", (S.End, end: Int) :: (S.File, file: String) :: (S.NewName, newName: String) :: (S.Start, start: Int) :: Nil) =>
        Right(RenameRefactorDesc(newName, file, start, end))
      case ("extractMethod", (S.End, end: Int) :: (S.File, file: String) :: (S.MethodName, methodName: String) :: (S.Start, start: Int) :: Nil) =>
        Right(ExtractMethodRefactorDesc(methodName, file, start, end))
      case ("extractLocal", (S.End, end: Int) :: (S.File, file: String) :: (S.Name, name: String) :: (S.Start, start: Int) :: Nil) =>
        Right(ExtractLocalRefactorDesc(name, file, start, end))
      case ("inlineLocal", (S.End, end: Int) :: (S.File, file: String) :: (S.Start, start: Int) :: Nil) =>
        Right(InlineLocalRefactorDesc(file, start, end))
      case ("organiseImports", (S.File, file: String) :: Nil) =>
        Right(OrganiseImportsRefactorDesc(file))
      case ("organizeImports", (S.File, file: String) :: Nil) =>
        Right(OrganiseImportsRefactorDesc(file))
      case ("addImport", (S.End, end: Int) :: (S.File, file: String) :: (S.QualifiedName, qualifiedName: String) :: (S.Start, start: Int) :: Nil) =>
        Right(AddImportRefactorDesc(qualifiedName, file, start, end))
      case _ =>
        Left("Incorrect arguments or unknown refactor type: " + tpe)
    }
  }
}
