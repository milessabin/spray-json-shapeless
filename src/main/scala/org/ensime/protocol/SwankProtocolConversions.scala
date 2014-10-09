package org.ensime.protocol

import java.io.File
import org.ensime.config._
import org.ensime.model._
import org.ensime.server._
import org.ensime.util._
import org.ensime.util.SExp._

import scala.reflect.internal.util.RangePosition

// bit of a rubbish class
class ReplConfig(val classpath: Set[File])

class SwankProtocolConversions extends ProtocolConversions {

  override def toWF(obj: DebugLocation): SExp = {
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

  override def toWF(obj: DebugValue): SExp = {
    obj match {
      case obj: DebugPrimitiveValue => toWF(obj)
      case obj: DebugObjectInstance => toWF(obj)
      case obj: DebugArrayInstance => toWF(obj)
      case obj: DebugStringInstance => toWF(obj)
      case obj: DebugNullValue => toWF(obj)
    }
  }

  override def toWF(obj: DebugNullValue): SExp = {
    SExp(
      key(":val-type"), 'null,
      key(":type-name"), obj.typeName)
  }

  override def toWF(obj: DebugPrimitiveValue): SExp = {
    SExp(
      key(":val-type"), 'prim,
      key(":summary"), obj.summary,
      key(":type-name"), obj.typeName)
  }

  override def toWF(obj: DebugClassField): SExp = {
    SExp(
      key(":index"), obj.index,
      key(":name"), obj.name,
      key(":summary"), obj.summary,
      key(":type-name"), obj.typeName)
  }

  override def toWF(obj: DebugObjectInstance): SExp = {
    SExp(
      key(":val-type"), 'obj,
      key(":fields"), SExpList(obj.fields.map(toWF)),
      key(":type-name"), obj.typeName,
      key(":object-id"), obj.objectId.toString)
  }

  override def toWF(obj: DebugStringInstance): SExp = {
    SExp(
      key(":val-type"), 'str,
      key(":summary"), obj.summary,
      key(":fields"), SExpList(obj.fields.map(toWF)),
      key(":type-name"), obj.typeName,
      key(":object-id"), obj.objectId.toString)
  }

  override def toWF(obj: DebugArrayInstance): SExp = {
    SExp(
      key(":val-type"), 'arr,
      key(":length"), obj.length,
      key(":type-name"), obj.typeName,
      key(":element-type-name"), obj.elementTypeName,
      key(":object-id"), obj.objectId.toString)
  }

  override def toWF(obj: DebugStackLocal): SExp = {
    SExp(
      key(":index"), obj.index,
      key(":name"), obj.name,
      key(":summary"), obj.summary,
      key(":type-name"), obj.typeName)
  }

  override def toWF(obj: DebugStackFrame): SExp = {
    SExp(
      key(":index"), obj.index,
      key(":locals"), SExpList(obj.locals.map(toWF)),
      key(":num-args"), obj.numArguments,
      key(":class-name"), obj.className,
      key(":method-name"), obj.methodName,
      key(":pc-location"), toWF(obj.pcLocation),
      key(":this-object-id"), obj.thisObjectId.toString)
  }

  override def toWF(obj: DebugBacktrace): SExp = {
    SExp(
      key(":frames"), SExpList(obj.frames.map(toWF)),
      key(":thread-id"), obj.threadId.toString,
      key(":thread-name"), obj.threadName)
  }

  override def toWF(pos: SourcePosition): SExp = pos match {
    case e: EmptySourcePosition => TruthAtom
    case l: LineSourcePosition => SExp(
      key(":file"), l.file.getAbsolutePath,
      key(":line"), l.line)
    case o: OffsetSourcePosition => SExp(
      key(":file"), o.file.getAbsolutePath,
      key(":offset"), o.offset)
  }

  override def toWF(info: ConnectionInfo): SExp = {
    SExp(
      key(":pid"), 'nil,
      key(":implementation"),
      SExp(key(":name"), info.serverName),
      key(":version"), info.protocolVersion)
  }

  def toWF(evt: SwankEvent): SExp = {
    evt match {
      case g: GeneralSwankEvent =>
        toWF(g)
      case d: DebugEvent =>
        toWF(d)
    }
  }

  def toWF(evt: GeneralSwankEvent): SExp = {
    evt match {
      /**
       * Doc Event:
       *   :compiler-ready
       * Summary:
       *   Signal that the compiler has finished its initial compilation and the server
       *   is ready to accept RPC calls.
       * Structure:
       *   (:compiler-ready)
       */
      case AnalyzerReadyEvent =>
        SExp(key(":compiler-ready"))
      /**
       * Doc Event:
       *   :full-typecheck-finished
       * Summary:
       *   Signal that the compiler has finished compilation of the entire project.
       * Structure:
       *   (:full-typecheck-finished)
       */
      case FullTypeCheckCompleteEvent =>
        SExp(key(":full-typecheck-finished"))
      /**
       * * Doc Event:
       *   :indexer-ready
       * Summary:
       *   Signal that the indexer has finished indexing the classpath.
       * Structure:
       *   (:indexer-ready)
       */
      case IndexerReadyEvent =>
        SExp(key(":indexer-ready"))
      /**
       * Doc Event:
       *   :compiler-restarted
       * Summary:
       *   Signal that the compiler was restarted. :type-id values received earlier
       *    are now invalid.
       * Structure:
       *   (:compiler-restarted)
       */
      case CompilerRestartedEvent =>
        SExp(key(":compiler-restarted"))
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
      case NewScalaNotesEvent(noteList) =>
        SExp(key(":scala-notes"), toWF(noteList))
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
      case NewJavaNotesEvent(noteList) =>
        SExp(key(":java-notes"), toWF(noteList))
      /**
       *  Doc Event:
       *   :clear-all-scala-notes
       * Summary:
       *   Notify client when Scala notes have become invalidated. Editor should consider
       *   all Scala related notes to be stale at this point.
       * Structure:
       *   (:clear-all-scala-notes)
       */
      case ClearAllScalaNotesEvent =>
        SExp(key(":clear-all-scala-notes"))
      /**
       * Doc Event:
       *   :clear-all-java-notes
       * Summary:
       *   Notify client when Java notes have become invalidated. Editor should consider
       *   all Java related notes to be stale at this point.
       * Structure:
       *   (:clear-all-java-notes)
       */
      case ClearAllJavaNotesEvent =>
        SExp(key(":clear-all-java-notes"))
      /**
       * Doc Event:
       *   :background-message
       * Summary:
       *   A background notification from the server for the client.
       * Structure:
       *   (:background-message
       *      code // Int
       *      message // String message or nil
       *   )
       */
      case SendBackgroundMessageEvent(code, detail) =>
        SExp(key(":background-message"), code, detail.map(strToSExp).getOrElse(NilAtom))
    }
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
      case DebugOutputEvent(out: String) =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'output,
            key(":body"), out))

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
      case DebugStepEvent(threadId, threadName, pos) =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'step,
            key(":thread-id"), threadId.toString,
            key(":thread-name"), threadName,
            key(":file"), pos.file.getAbsolutePath,
            key(":line"), pos.line))

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
      case DebugBreakEvent(threadId, threadName, pos) =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'breakpoint,
            key(":thread-id"), threadId.toString,
            key(":thread-name"), threadName,
            key(":file"), pos.file.getAbsolutePath,
            key(":line"), pos.line))

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
      case DebugVMDeathEvent() =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'death))

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
      case DebugVMStartEvent() =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'start))

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
      case DebugVMDisconnectEvent() =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'disconnect))

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
      case DebugExceptionEvent(excId, threadId, threadName, maybePos) =>
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
      case DebugThreadStartEvent(threadId: Long) =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'threadStart,
            key(":thread-id"), threadId.toString))

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
      case DebugThreadDeathEvent(threadId: Long) =>
        SExp(key(":debug-event"),
          SExp(key(":type"), 'threadDeath,
            key(":thread-id"), threadId.toString))
    }
  }

  def toWF(bp: Breakpoint): SExp = {
    SExp(
      key(":file"), bp.pos.file.getAbsolutePath,
      key(":line"), bp.pos.line)
  }

  override def toWF(bps: BreakpointList): SExp = {
    SExp(
      key(":active"), SExpList(bps.active.map { toWF }),
      key(":pending"), SExpList(bps.pending.map { toWF }))
  }

  override def toWF(config: EnsimeConfig): SExp = SExp(
    key(":project-name"), StringAtom(config.name),
    key(":source-roots"), SExp(
      config.modules.values.flatMap {
        _.sourceRoots.map { r => StringAtom(r.getAbsolutePath) }
      }
    )
  )

  override def toWF(config: ReplConfig): SExp = {
    SExp.propList((":classpath", strToSExp(config.classpath.mkString("\"", File.pathSeparator, "\""))))
  }

  override def toWF(value: Boolean): SExp = {
    if (value) TruthAtom
    else NilAtom
  }

  override val wfNull: SExp = NilAtom

  override def toWF(value: String): SExp = {
    StringAtom(value)
  }

  override def toWF(note: Note): SExp = {
    SExp(
      key(":severity"), note.friendlySeverity,
      key(":msg"), note.msg,
      key(":beg"), note.beg,
      key(":end"), note.end,
      key(":line"), note.line,
      key(":col"), note.col,
      key(":file"), note.file)
  }

  override def toWF(notelist: NoteList): SExp = {
    val NoteList(isFull, notes) = notelist
    SExp(
      key(":is-full"),
      toWF(isFull),
      key(":notes"),
      SExpList(notes.map(toWF).toList))
  }

  override def toWF(values: Iterable[WireFormat]): SExp = {
    SExpList(values.map(ea => ea.asInstanceOf[SExp]).toList)
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

  override def toWF(value: CompletionInfo): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type-sig", toWF(value.tpeSig)),
      (":type-id", value.tpeId),
      (":is-callable", value.isCallable),
      (":to-insert", value.toInsert.map(strToSExp).getOrElse('nil)))
  }

  override def toWF(value: CompletionInfoList): SExp = {
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
      // not entirely clear why "decl-pos" instead of "pos"
      (":decl-pos", value.declPos.map(toWF).getOrElse('nil)),
      (":is-callable", value.isCallable),
      (":owner-type-id", value.ownerTypeId.map(intToSExp).getOrElse('nil)))
  }

  def toWF(value: FileRange): SExp = {
    SExp.propList(
      (":file", value.file),
      (":start", value.start),
      (":end", value.end))
  }

  override def toWF(value: NamedTypeMemberInfo): SExp = {
    SExp.propList(
      (":name", value.name),
      (":type", toWF(value.tpe)),
      (":pos", value.pos.map(toWF).getOrElse('nil)),
      (":decl-as", value.declaredAs))
  }

  override def toWF(value: EntityInfo): SExp = {
    value match {
      case value: PackageInfo => toWF(value)
      case value: TypeInfo => toWF(value)
      case value: NamedTypeMemberInfo => toWF(value)
      case unknownValue => throw new IllegalStateException("Unknown EntityInfo: " + unknownValue)
    }
  }

  def toWF(value: TypeInfo): SExp = {
    value match {
      case value: ArrowTypeInfo =>
        SExp.propList(
          (":name", value.name),
          (":type-id", value.id),
          (":arrow-type", true),
          (":result-type", toWF(value.resultType)),
          (":param-sections", SExp(value.paramSections.map(toWF))))
      case value: TypeInfo =>
        SExp.propList((":name", value.name),
          (":type-id", value.id),
          (":full-name", value.fullName),
          (":decl-as", value.declaredAs),
          (":type-args", SExp(value.args.map(toWF))),
          (":members", SExp(value.members.map(toWF))),
          (":pos", value.pos.map(toWF).getOrElse('nil)),
          (":outer-type-id", value.outerTypeId.map(intToSExp).getOrElse('nil)))
      case unknownValue => throw new IllegalStateException("Unknown TypeInfo: " + unknownValue)
    }
  }

  def toWF(value: PackageInfo): SExp = {
    SExp.propList((":name", value.name),
      (":info-type", 'package),
      (":full-name", value.fullname),
      (":members", SExpList(value.members.map(toWF).toList)))
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
      }),
      (":interfaces", SExp(value.supers.map(toWF))))
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
      (":changes", SExpList(value.changes.map(changeToWF).toList)))
  }

  def toWF(value: RefactorResult): SExp = {
    SExp.propList(
      (":procedure-id", value.procedureId),
      (":refactor-type", value.refactorType),
      (":status", 'success),
      (":touched-files", SExpList(value.touched.map(f => strToSExp(f.getAbsolutePath)).toList)))
  }

  def toWF(value: SymbolSearchResults): SExp = {
    SExpList(value.syms.map(toWF).toList)
  }

  def toWF(value: ImportSuggestions): SExp = {
    SExpList(value.symLists.map { l => SExpList(l.map(toWF).toList) }.toList)
  }

  // FIXME: client expects offset, but we typically have line
  //        infer for now and look at changing the protocol
  def toWF(value: SymbolSearchResult): SExp = {
    value match {
      case value: TypeSearchResult =>
        SExp.propList(
          (":name", value.name),
          (":local-name", value.localName),
          (":decl-as", value.declaredAs),
          (":pos", value.pos.map(toWF).getOrElse('nil)))
      case value: MethodSearchResult =>
        SExp.propList(
          (":name", value.name),
          (":local-name", value.localName),
          (":decl-as", value.declaredAs),
          (":pos", value.pos.map(toWF).getOrElse('nil)),
          (":owner-name", value.owner))
    }
  }

  def toWF(p: RangePosition): SExp = {
    // assumes a real file. see discussion around SourcePosition
    SExp.propList(
      (":file", p.source.path),
      (":offset", p.point),
      (":start", p.start),
      (":end", p.end))
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
      (":touched-files", SExpList(value.touched.map(f => strToSExp(f.getAbsolutePath)).toList)))
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
      case DebugVmSuccess => SExp(
        key(":status"), "success")
      case DebugVmError(code, details) => SExp(
        key(":status"), "error",
        key(":error-code"), code,
        key(":details"), details)
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
      (":file", ch.file.getCanonicalPath),
      (":text", ch.text),
      (":from", ch.from),
      (":to", ch.to))
  }

}
