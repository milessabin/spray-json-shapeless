package org.ensime.model

import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.apache.commons.vfs2.FileObject
import scala.collection.mutable
import scala.reflect.internal.util.{ NoPosition, Position }

import org.ensime.config._
import org.ensime.server._
import org.ensime.indexer.DatabaseService._

import org.ensime.util.RichFile._

abstract class EntityInfo(val name: String, val members: Iterable[EntityInfo]) {}

sealed trait PosNeeded
case object PosNeededNo extends PosNeeded
case object PosNeededAvail extends PosNeeded
case object PosNeededYes extends PosNeeded

sealed trait SourcePosition
case class EmptySourcePosition() extends SourcePosition
case class OffsetSourcePosition(file: File, offset: Int) extends SourcePosition
case class LineSourcePosition(file: File, line: Int) extends SourcePosition

case class SourceFileInfo(file: File, contents: Option[String])
object SourceFileInfo {
  def apply(file: String) = new SourceFileInfo(new File(file), None)
  def apply(file: File) = new SourceFileInfo(file, None)
}

class PackageInfo(
  override val name: String,
  val fullname: String,
  override val members: Iterable[EntityInfo]) extends EntityInfo(name, members)

trait SymbolSearchResult {
  val name: String
  val localName: String
  val declaredAs: scala.Symbol
  val pos: Option[SourcePosition]
}

case class TypeSearchResult(
  name: String,
  localName: String,
  declaredAs: scala.Symbol,
  pos: Option[SourcePosition]) extends SymbolSearchResult

case class MethodSearchResult(
  name: String,
  localName: String,
  declaredAs: scala.Symbol,
  pos: Option[SourcePosition],
  owner: String) extends SymbolSearchResult

case class ImportSuggestions(symLists: Iterable[Iterable[SymbolSearchResult]])
case class SymbolSearchResults(syms: Iterable[SymbolSearchResult])

case class SymbolDesignations(
  file: String,
  syms: List[SymbolDesignation])

case class SymbolDesignation(
  start: Int,
  end: Int,
  symType: scala.Symbol)

class SymbolInfo(
  val name: String,
  val localName: String,
  val declPos: Option[SourcePosition],
  val tpe: TypeInfo,
  val isCallable: Boolean,
  val ownerTypeId: Option[Int])

case class Op(
  op: String,
  description: String)

case class MethodBytecode(
  className: String,
  methodName: String,
  methodSignature: Option[String],
  byteCode: List[Op],
  startLine: Int,
  endLine: Int)

case class CompletionSignature(
  sections: List[List[(String, String)]],
  result: String)

case class CompletionInfo(
  name: String,
  tpeSig: CompletionSignature,
  tpeId: Int,
  isCallable: Boolean,
  relevance: Int,
  toInsert: Option[String])

case class CompletionInfoList(
  prefix: String,
  completions: List[CompletionInfo])

sealed trait PatchOp {
  val start: Int
}

case class PatchInsert(
  start: Int,
  text: String) extends PatchOp

case class PatchDelete(
  start: Int,
  end: Int) extends PatchOp

case class PatchReplace(
  start: Int,
  end: Int,
  text: String) extends PatchOp

case class Breakpoint(pos: LineSourcePosition)
case class BreakpointList(active: List[Breakpoint], pending: List[Breakpoint])

case class OffsetRange(from: Int, to: Int)

object OffsetRange {
  def apply(fromTo: Int): OffsetRange = new OffsetRange(fromTo, fromTo)
}

sealed trait DebugLocation

case class DebugObjectReference(objectId: Long) extends DebugLocation

case class DebugStackSlot(threadId: Long, frame: Int, offset: Int) extends DebugLocation

case class DebugArrayElement(objectId: Long, index: Int) extends DebugLocation

case class DebugObjectField(objectId: Long, name: String) extends DebugLocation

sealed trait DebugValue {
  def typeName: String
}

case class DebugNullValue(
  typeName: String) extends DebugValue

case class DebugPrimitiveValue(
  summary: String,
  typeName: String) extends DebugValue

case class DebugClassField(
  index: Int,
  name: String,
  typeName: String,
  summary: String)

case class DebugObjectInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: Long) extends DebugValue

case class DebugStringInstance(
  summary: String,
  fields: List[DebugClassField],
  typeName: String,
  objectId: Long) extends DebugValue

case class DebugArrayInstance(
  length: Int,
  typeName: String,
  elementTypeName: String,
  objectId: Long) extends DebugValue

case class DebugStackLocal(
  index: Int,
  name: String,
  typeName: String,
  summary: String)

case class DebugStackFrame(
  index: Int,
  locals: List[DebugStackLocal],
  numArguments: Int,
  className: String,
  methodName: String,
  pcLocation: LineSourcePosition,
  thisObjectId: Long)

case class DebugBacktrace(
  frames: List[DebugStackFrame],
  threadId: Long,
  threadName: String)

class NamedTypeMemberInfo(override val name: String,
  val tpe: TypeInfo,
  val pos: Option[SourcePosition],
  val declaredAs: scala.Symbol) extends EntityInfo(name, List.empty)

class PackageMemberInfoLight(val name: String)

class TypeInfo(
  name: String,
  val id: Int,
  val declaredAs: scala.Symbol,
  val fullName: String,
  val args: Iterable[TypeInfo],
  members: Iterable[EntityInfo],
  val pos: Option[SourcePosition],
  val outerTypeId: Option[Int]) extends EntityInfo(name, members)

class ArrowTypeInfo(
  override val name: String,
  override val id: Int,
  val resultType: TypeInfo,
  val paramSections: Iterable[ParamSectionInfo]) extends TypeInfo(name, id, 'nil, name, List.empty, List.empty, None, None)

class CallCompletionInfo(
  val resultType: TypeInfo,
  val paramSections: Iterable[ParamSectionInfo])

class ParamSectionInfo(
  val params: Iterable[(String, TypeInfo)],
  val isImplicit: Boolean)

class InterfaceInfo(val tpe: TypeInfo, val viaView: Option[String])

class TypeInspectInfo(val tpe: TypeInfo, val companionId: Option[Int], val supers: Iterable[InterfaceInfo])

trait ModelBuilders { self: RichPresentationCompiler =>

  import rootMirror.RootPackage

  private val typeCache = new mutable.HashMap[Int, Type]
  private val typeCacheReverse = new mutable.HashMap[Type, Int]

  def clearTypeCache() {
    typeCache.clear()
    typeCacheReverse.clear()
  }
  def typeById(id: Int): Option[Type] = {
    typeCache.get(id)
  }
  def cacheType(tpe: Type): Int = {
    if (typeCacheReverse.contains(tpe)) {
      typeCacheReverse(tpe)
    } else {
      val id = typeCache.size + 1
      typeCache(id) = tpe
      typeCacheReverse(tpe) = id
      id
    }
  }

  def locateSymbolPos(sym: Symbol, needPos: PosNeeded): Option[SourcePosition] =
    if (sym == NoSymbol || needPos == PosNeededNo) None
    else if (sym.pos != NoPosition) {
      if (needPos eq PosNeededYes)
        OffsetSourcePosition.fromPosition(sym.pos)
      else Some(EmptySourcePosition())
    } else {
      // we might need this for some Java fqns but we need some evidence
      // val name = genASM.jsymbol(sym).fullName
      val name = sym.fullName
      val hit = search.findUnique(name)
      logger.debug(s"search: $name = $hit")
      if (needPos eq PosNeededYes) {
        hit.flatMap(LineSourcePosition.fromFqnSymbol(_)(config))
      } else {
        if (hit.isEmpty) None else Some(EmptySourcePosition())
      }
    }

  // When inspecting a type, transform a raw list of TypeMembers to a sorted
  // list of InterfaceInfo objects, each with its own list of sorted member infos.
  def prepareSortedInterfaceInfo(members: Iterable[Member], parents: Iterable[Type]): Iterable[InterfaceInfo] = {
    // ...filtering out non-visible and non-type members
    val visMembers: Iterable[TypeMember] = members.flatMap {
      case m @ TypeMember(sym, tpe, true, _, _) => List(m)
      case _ => List.empty
    }

    val parentMap = parents.map(_.typeSymbol -> List[TypeMember]()).toMap
    val membersMap = visMembers.groupBy {
      case TypeMember(sym, _, _, _, _) => sym.owner
    }
    // Create a list of pairs [(typeSym, membersOfSym)]
    val membersByOwner = (parentMap ++ membersMap).toList.sortWith {
      // Sort the pairs on the subtype relation
      case ((s1, _), (s2, _)) => s1.tpe <:< s2.tpe
    }

    membersByOwner.map {
      case (ownerSym, members) =>

        // If all the members in this interface were
        // provided by the same view, remember that
        // view for later display to user.
        val byView = members.groupBy(_.viaView)
        val viaView = if (byView.size == 1) {
          byView.keys.headOption.filter(_ != NoSymbol)
        } else { None }

        // Do one top level sort by name on members, before
        // subdividing into kinds of members.
        val sortedMembers = members.toList.sortWith { (a, b) =>
          a.sym.nameString <= b.sym.nameString
        }

        // Convert type members into NamedTypeMemberInfos
        // and divid into different kinds..

        val nestedTypes = new mutable.ArrayBuffer[NamedTypeMemberInfo]()
        val constructors = new mutable.ArrayBuffer[NamedTypeMemberInfo]()
        val fields = new mutable.ArrayBuffer[NamedTypeMemberInfo]()
        val methods = new mutable.ArrayBuffer[NamedTypeMemberInfo]()

        for (tm <- sortedMembers) {
          val info = NamedTypeMemberInfo(tm)
          val decl = info.declaredAs
          if (decl == 'method) {
            if (info.name == "this") {
              constructors += info
            } else {
              methods += info
            }
          } else if (decl == 'field) {
            fields += info
          } else if (decl == 'class || decl == 'trait ||
            decl == 'interface || decl == 'object) {
            nestedTypes += info
          }
        }

        val sortedInfos = nestedTypes ++ fields ++ constructors ++ methods

        new InterfaceInfo(TypeInfo(ownerSym.tpe, PosNeededAvail, sortedInfos),
          viaView.map(_.name.toString))
    }
  }

  object PackageInfo {
    def root: PackageInfo = fromSymbol(RootPackage)

    def fromPath(path: String): PackageInfo = {
      val pack = packageSymFromPath(path)
      pack match {
        case Some(packSym) => fromSymbol(packSym)
        case None => nullInfo
      }
    }

    def nullInfo = {
      new PackageInfo("NA", "NA", List.empty)
    }

    def fromSymbol(sym: Symbol): PackageInfo = {
      if (sym.isRoot || sym.isRootPackage) {
        new PackageInfo(
          "root",
          "_root_",
          packageMembers(sym).flatMap(packageMemberInfoFromSym))
      } else {
        new PackageInfo(
          sym.name.toString,
          sym.fullName,
          packageMembers(sym).flatMap(packageMemberInfoFromSym))
      }
    }

    def packageMemberInfoFromSym(sym: Symbol): Option[EntityInfo] = {
      try {
        if (sym == RootPackage) {
          Some(root)
        } else if (sym.hasPackageFlag) {
          Some(fromSymbol(sym))
        } else if (!sym.nameString.contains("$") && (sym != NoSymbol) && (sym.tpe != NoType)) {
          if (sym.isClass || sym.isTrait || sym.isModule ||
            sym.isModuleClass || sym.isPackageClass) {
            Some(TypeInfo(sym.tpe, PosNeededAvail))
          } else {
            None
          }
        } else {
          None
        }
      } catch {
        case e: Throwable => None
      }
    }
  }

  object TypeInfo {

    // use needPos=PosNeededYes sparingly as it potentially causes lots of I/O
    def apply(typ: Type, needPos: PosNeeded = PosNeededNo, members: Iterable[EntityInfo] = List.empty): TypeInfo = {
      val tpe = typ match {
        // TODO: Instead of throwing away this information, would be better to
        // alert the user that the type is existentially quantified.
        case et: ExistentialType => et.underlying
        case t => t
      }
      tpe match {
        case tpe: MethodType => ArrowTypeInfo(tpe)
        case tpe: PolyType => ArrowTypeInfo(tpe)
        case tpe: Type =>
          val args = tpe.typeArgs.map(TypeInfo(_))
          val typeSym = tpe.typeSymbol
          val sym = if (typeSym.isModuleClass)
            typeSym.sourceModule else typeSym
          val symPos = locateSymbolPos(sym, needPos)
          val outerTypeId = outerClass(typeSym).map(s => cacheType(s.tpe))
          new TypeInfo(
            typeShortName(tpe),
            cacheType(tpe),
            declaredAs(typeSym),
            typeFullName(tpe),
            args,
            members,
            symPos,
            outerTypeId)
        case _ => nullInfo
      }
    }

    def nullInfo = {
      new TypeInfo("NA", -1, 'nil, "NA", List.empty, List.empty, None, None)
    }
  }

  object ParamSectionInfo {
    def apply(params: Iterable[Symbol]): ParamSectionInfo = {
      new ParamSectionInfo(params.map { s => (s.nameString, TypeInfo(s.tpe)) },
        params.forall { s => s.isImplicit })
    }
  }

  object CallCompletionInfo {

    def apply(tpe: Type): CallCompletionInfo = {
      tpe match {
        case tpe: MethodType => apply(tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case tpe: PolyType => apply(tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case _ => nullInfo()
      }
    }

    def apply(paramSections: List[ParamSectionInfo], finalResultType: Type): CallCompletionInfo = {
      new CallCompletionInfo(
        TypeInfo(finalResultType),
        paramSections)
    }

    def nullInfo() = {
      new CallCompletionInfo(TypeInfo.nullInfo, List.empty)
    }
  }

  object SymbolInfo {

    def apply(sym: Symbol): SymbolInfo = {
      val tpe = askOption(sym.tpe) match {
        case None => NoType
        case Some(t) => t
      }
      val name = if (sym.isClass || sym.isTrait || sym.isModule ||
        sym.isModuleClass || sym.isPackageClass) {
        typeFullName(tpe)
      } else {
        sym.nameString
      }
      val localName = sym.nameString
      val ownerTpe = if (sym.owner != NoSymbol && sym.owner.tpe != NoType) {
        Some(sym.owner.tpe)
      } else None
      new SymbolInfo(
        name,
        localName,
        locateSymbolPos(sym, PosNeededYes),
        TypeInfo(tpe, PosNeededYes),
        isArrowType(tpe),
        ownerTpe.map(cacheType))
    }
  }

  object CompletionInfo {

    def apply(
      name: String,
      tpeSig: CompletionSignature,
      tpeId: Int,
      isCallable: Boolean,
      relevance: Int,
      toInsert: Option[String]) = new CompletionInfo(
      name, tpeSig, tpeId, isCallable, relevance, toInsert)

    def fromSymbol(sym: Symbol, relevance: Int): CompletionInfo =
      CompletionInfo.fromSymbolAndType(sym, sym.tpe, relevance)

    def fromSymbolAndType(sym: Symbol, tpe: Type, relevance: Int): CompletionInfo = {
      CompletionInfo(
        sym.nameString,
        completionSignatureForType(tpe),
        cacheType(tpe.underlying),
        isArrowType(tpe.underlying),
        relevance,
        None)
    }

    def nullInfo() = {
      new CompletionInfo("NA", CompletionSignature(List.empty, ""), -1, false, 0, None)
    }
  }

  object NamedTypeMemberInfo {
    def apply(m: TypeMember): NamedTypeMemberInfo = {
      val decl = declaredAs(m.sym)
      val pos = if (m.sym.pos == NoPosition) None else Some(EmptySourcePosition())
      new NamedTypeMemberInfo(m.sym.nameString, TypeInfo(m.tpe), pos, decl)
    }
  }

  object ArrowTypeInfo {

    def apply(tpe: Type): ArrowTypeInfo = {
      tpe match {
        case tpe: MethodType => apply(tpe, tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case tpe: PolyType => apply(tpe, tpe.paramss.map(ParamSectionInfo.apply), tpe.finalResultType)
        case _ => nullInfo()
      }
    }

    def apply(tpe: Type, paramSections: List[ParamSectionInfo], finalResultType: Type): ArrowTypeInfo = {
      new ArrowTypeInfo(
        tpe.toString(),
        cacheType(tpe),
        TypeInfo(tpe.finalResultType),
        paramSections)
    }

    def nullInfo() = {
      new ArrowTypeInfo("NA", -1, TypeInfo.nullInfo, List.empty)
    }
  }

  object TypeInspectInfo {
    def nullInfo() = {
      new TypeInspectInfo(TypeInfo.nullInfo, None, List.empty)
    }
  }

}

object LineSourcePosition {

  // HACK: the emacs client currently can't open files in jars
  //       so we extract to the cache and report that as the source
  //       see the hack in the RichPresentationCompiler
  import org.ensime.util.RichFileObject._
  import pimpathon.java.io.outputStream._
  import pimpathon.any._
  import pimpathon.file._

  private def possiblyExtractFile(fo: FileObject)(implicit config: EnsimeConfig): File =
    fo.pathWithinArchive match {
      case None => fo.asLocalFile
      case Some(path) =>
        // subpath expected by the client
        (config.cacheDir / "dep-src" / "source-jars" / path) withSideEffect { f =>
          if (!f.exists) {
            f.getParentFile.mkdirs()
            f.outputStream.drain(fo.getContent.getInputStream)
            f.setWritable(false)
          }
        }
    }

  def fromFqnSymbol(sym: FqnSymbol)(implicit config: EnsimeConfig): Option[LineSourcePosition] =
    (sym.sourceFileObject, sym.line, sym.offset) match {
      case (None, _, _) => None
      case (Some(fo), lineOpt, offsetOpt) =>
        val f = possiblyExtractFile(fo)
        Some(new LineSourcePosition(f, lineOpt.getOrElse(0)))
    }

}

object OffsetSourcePosition {
  import pimpathon.file._

  def fromPosition(p: Position): Option[OffsetSourcePosition] = p match {
    case NoPosition => None
    case p =>
      Some(new OffsetSourcePosition(file(p.source.file.path).canon, p.point))
  }
}
