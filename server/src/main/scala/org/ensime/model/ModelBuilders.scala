package org.ensime.model

import java.io.File

import org.ensime.api._

import org.apache.commons.vfs2.FileObject
import org.ensime.config._
import org.ensime.core.RichPresentationCompiler
import org.ensime.indexer.DatabaseService._
import org.ensime.indexer.EnsimeVFS

import scala.collection.mutable
import scala.reflect.internal.util.{ NoPosition, Position, RangePosition }
import scala.tools.nsc.io.AbstractFile

trait ModelBuilders { self: RichPresentationCompiler =>

  import rootMirror.RootPackage

  private val typeCache = new mutable.HashMap[Int, Type]
  private val typeCacheReverse = new mutable.HashMap[Type, Int]

  def clearTypeCache(): Unit = {
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

  def locateSymbolPos(sym: Symbol, needPos: PosNeeded): Option[SourcePosition] = {
    if (sym == NoSymbol || needPos == PosNeededNo)
      None
    else if (sym.pos != NoPosition) {
      if (needPos == PosNeededYes || needPos == PosNeededAvail) {
        OffsetSourcePositionHelper.fromPosition(sym.pos)
      } else
        Some(EmptySourcePosition())
    } else {
      // only perform operations is actively requested - this is comparatively expensive
      if (needPos == PosNeededYes) {
        // we might need this for some Java fqns but we need some evidence
        // val name = genASM.jsymbol(sym).fullName
        val name = symbolIndexerName(sym)
        val hit = search.findUnique(name)
        logger.debug(s"search: $name = $hit")
        hit.flatMap(LineSourcePositionHelper.fromFqnSymbol(_)(config, vfs)).flatMap { sourcePos =>
          if (sourcePos.file.getName.endsWith(".scala"))
            askLinkPos(sym, AbstractFile.getFile(sourcePos.file)).
              flatMap(pos => OffsetSourcePositionHelper.fromPosition(pos))
          else
            Some(sourcePos)
        }
      } else
        None
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
        // and divide into different kinds..

        val nestedTypes = new mutable.ArrayBuffer[NamedTypeMemberInfo]()
        val constructors = new mutable.ArrayBuffer[NamedTypeMemberInfo]()
        val fields = new mutable.ArrayBuffer[NamedTypeMemberInfo]()
        val methods = new mutable.ArrayBuffer[NamedTypeMemberInfo]()

        for (tm <- sortedMembers) {
          val info = NamedTypeMemberInfo(tm)
          val decl = info.declAs
          if (decl == DeclaredAs.Method) {
            if (info.name == "this") {
              constructors += info
            } else {
              methods += info
            }
          } else if (decl == DeclaredAs.Field) {
            fields += info
          } else if (decl == DeclaredAs.Class || decl == DeclaredAs.Trait ||
            decl == DeclaredAs.Interface || decl == DeclaredAs.Object) {
            nestedTypes += info
          }
        }

        val sortedInfos = nestedTypes ++ fields ++ constructors ++ methods

        new InterfaceInfo(TypeInfo(ownerSym.tpe, PosNeededAvail, sortedInfos), viaView.map(_.name.toString))
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

    // TODO THIS SHOULD NOT EXIST
    val nullInfo = new PackageInfo("NA", "NA", List.empty)

    private def sortedMembers(items: Iterable[EntityInfo]) = {
      items.toList.sortBy(_.name)
    }

    def fromSymbol(sym: Symbol): PackageInfo = {
      val members = sortedMembers(packageMembers(sym).flatMap(packageMemberInfoFromSym))
      if (sym.isRoot || sym.isRootPackage) {
        new PackageInfo("root", "_root_", members)
      } else {
        new PackageInfo(sym.name.toString, sym.fullName, members)
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
      def basicTypeInfo(tpe: Type): BasicTypeInfo = {
        val typeSym = tpe.typeSymbol
        val symbolToLocate = if (typeSym.isModuleClass) typeSym.sourceModule else typeSym
        val symPos = locateSymbolPos(symbolToLocate, needPos)
        val outerTypeId = outerClass(typeSym).map(s => cacheType(s.tpe))
        new BasicTypeInfo(
          typeShortName(tpe),
          cacheType(tpe),
          declaredAs(typeSym),
          typeFullName(tpe),
          tpe.typeArgs.map(TypeInfo(_)),
          members,
          symPos,
          outerTypeId
        )
      }
      tpe match {
        case tpe: MethodType => ArrowTypeInfo(tpe)
        case tpe: PolyType => ArrowTypeInfo(tpe)
        case tpe: NullaryMethodType => basicTypeInfo(tpe.resultType)
        case tpe: Type => basicTypeInfo(tpe)
        case _ => nullInfo
      }
    }

    def nullInfo = {
      new BasicTypeInfo("NA", -1, DeclaredAs.Nil, "NA", List.empty, List.empty, None, None)
    }
  }

  object ParamSectionInfo {
    def apply(params: Iterable[Symbol]): ParamSectionInfo = {
      new ParamSectionInfo(
        params.map { s => (s.nameString, TypeInfo(s.tpe)) },
        params.exists(_.isImplicit)
      )
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
        paramSections
      )
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
      val nameString = sym.nameString
      val (name, localName) = if (sym.isClass || sym.isTrait || sym.isModule ||
        sym.isModuleClass || sym.isPackageClass) {
        (typeFullName(tpe), nameString)
      } else {
        (nameString, nameString)
      }
      val ownerTpe = if (sym.owner != NoSymbol && sym.owner.tpe != NoType) {
        Some(sym.owner.tpe)
      } else None
      new SymbolInfo(
        name,
        localName,
        locateSymbolPos(sym, PosNeededYes),
        TypeInfo(tpe, PosNeededAvail),
        isArrowType(tpe),
        ownerTpe.map(cacheType)
      )
    }
  }

  object CompletionInfo {

    def apply(
      name: String,
      tpeSig: CompletionSignature,
      tpeId: Int,
      isCallable: Boolean,
      relevance: Int,
      toInsert: Option[String]
    ) = new CompletionInfo(
      name, tpeSig, tpeId, isCallable, relevance, toInsert
    )

    def fromSymbol(sym: Symbol, relevance: Int): CompletionInfo =
      CompletionInfo.fromSymbolAndType(sym, sym.tpe, relevance)

    def fromSymbolAndType(sym: Symbol, tpe: Type, relevance: Int): CompletionInfo = {
      CompletionInfo(
        sym.nameString,
        completionSignatureForType(tpe),
        cacheType(tpe.underlying),
        isArrowType(tpe.underlying),
        relevance,
        None
      )
    }

  }

  object NamedTypeMemberInfo {
    def apply(m: TypeMember): NamedTypeMemberInfo = {
      val decl = declaredAs(m.sym)
      val pos = if (m.sym.pos == NoPosition) None else Some(EmptySourcePosition())
      val signatureString = if (decl == DeclaredAs.Method) Some(m.sym.signatureString) else None
      new NamedTypeMemberInfo(m.sym.nameString, TypeInfo(m.tpe), pos, signatureString, decl)
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
        paramSections
      )
    }

    def nullInfo() = {
      new ArrowTypeInfo("NA", -1, TypeInfo.nullInfo, List.empty)
    }
  }
}

object LineSourcePositionHelper {

  // HACK: the emacs client currently can't open files in jars
  //       so we extract to the cache and report that as the source
  //       see the hack in the RichPresentationCompiler
  import org.ensime.util.RichFileObject._
  import pimpathon.any._
  import pimpathon.file._
  import pimpathon.java.io._

  private def possiblyExtractFile(fo: FileObject)(implicit config: EnsimeConfig): File =
    fo.pathWithinArchive match {
      case None => fo.asLocalFile
      case Some(path) =>
        // subpath expected by the client
        (config.cacheDir / "dep-src" / "source-jars" / path) withSideEffect { f =>
          if (!f.exists) {
            f.getParentFile.mkdirs()
            f.outputStream().drain(fo.getContent.getInputStream)
            f.setWritable(false)
          }
        }
    }

  def fromFqnSymbol(sym: FqnSymbol)(implicit config: EnsimeConfig, vfs: EnsimeVFS): Option[LineSourcePosition] =
    (sym.sourceFileObject, sym.line, sym.offset) match {
      case (None, _, _) => None
      case (Some(fo), lineOpt, offsetOpt) =>
        val f = possiblyExtractFile(fo)
        Some(new LineSourcePosition(f, lineOpt.getOrElse(0)))
    }

}

object OffsetSourcePositionHelper {
  import pimpathon.file._

  def fromPosition(p: Position): Option[OffsetSourcePosition] = p match {
    case NoPosition => None
    case realPos =>
      Some(new OffsetSourcePosition(file(realPos.source.file.path).canon, realPos.point))
  }
}

object ERangePositionHelper {
  def fromRangePosition(rp: RangePosition): ERangePosition = new ERangePosition(rp.source.path, rp.point, rp.start, rp.end)
}
