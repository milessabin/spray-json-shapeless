package org.ensime.core
import org.ensime.model._

// Transform symbols to scaladoc URI paths, with anchors to select specific
// members of a container type.
//
// See scala/src/scaladoc/scala/tools/nsc/doc/base/MemberLookupBase.scala for
// details related to link construction.
trait DocFinding { self: RichPresentationCompiler =>

  private def isRoot(s: Symbol) = (s eq NoSymbol) || s.isRootSymbol || s.isEmptyPackage || s.isEmptyPackageClass

  private def fullPackage(sym: Symbol): String =
    sym.ownerChain.reverse.filterNot(isRoot)
      .takeWhile(_.hasPackageFlag).map(_.nameString).mkString(".")

  private def fullTypeName(sym: Symbol, nestedTypeSep: String, nameString: (Symbol => String)): String =
    sym.ownerChain.takeWhile(!_.hasPackageFlag).reverseMap(nameString).mkString(nestedTypeSep)

  private val ScalaPrim = """^(Boolean|Byte|Char|Double|Float|Int|Long|Short)$""".r
  private val ScalaAny = """^(Any|AnyVal|AnyRef)$""".r
  private def javaFqn(tpe: Type): DocFqn = {
    def nameString(sym: Symbol) = sym.nameString.replace("$", "")
    val sym = tpe.typeSymbol
    val s = if (sym.hasPackageFlag) {
      DocFqn(fullPackage(sym), "package")
    } else {
      DocFqn(fullPackage(sym), fullTypeName(sym, ".", nameString))
    }
    s match {
      case DocFqn("scala", ScalaPrim(datatype)) => DocFqn("", datatype.toLowerCase)
      case DocFqn("scala", ScalaAny(datatype)) => DocFqn("java.lang", "Object")
      case DocFqn("scala", "Array") =>
        tpe.typeArgs.headOption.map { tpe =>
          val fqn = javaFqn(tpe)
          fqn.copy(typeName = fqn.typeName + "[]")
        }.getOrElse(s)
      case _ => s
    }
  }

  protected def scalaFqn(sym: Symbol): DocFqn = {
    def nameString(s: Symbol) = s.nameString + (if ((s.isModule || s.isModuleClass) && !s.hasPackageFlag) "$" else "")
    if (sym.isPackageObjectOrClass) {
      DocFqn(fullPackage(sym.owner), "package")
    } else if (sym.hasPackageFlag) {
      DocFqn(fullPackage(sym), ".package")
    } else {
      DocFqn(fullPackage(sym), fullTypeName(sym, "$", nameString))
    }
  }

  private def linkName(sym: Symbol, java: Boolean): DocFqn = {
    if (java) javaFqn(sym.tpe) else scalaFqn(sym)
  }

  private def signatureString(sym: Symbol, java: Boolean): String = {
    sym.nameString + (if (java) {
      if (sym.paramLists.isEmpty) ""
      else sym.paramLists.flatMap(_.map { sym => javaFqn(sym.tpe).mkString }).mkString("(", ", ", ")")
    } else sym.signatureString.replaceAll("[\\s]", ""))
  }

  def docSignature(sym: Symbol, pos: Option[Position]): Option[DocSigPair] = {
    def docSig(java: Boolean) = {
      val owner = sym.owner
      if (sym.isCaseApplyOrUnapply) {
        DocSig(linkName(owner.companionClass, java), None)
      } else if (sym.isClass || sym.isModule || sym.isTrait || sym.hasPackageFlag)
        DocSig(linkName(sym, java), None)
      else if (owner.isClass || owner.isModule || owner.isTrait || owner.hasPackageFlag) {
        val ownerAtSite = pos.flatMap(specificOwnerOfSymbolAt).getOrElse(owner)
        DocSig(linkName(ownerAtSite, java), Some(signatureString(sym, java)))
      } else
        DocSig(linkName(sym.tpe.typeSymbol, java), None)
    }
    Some(DocSigPair(docSig(java = false), docSig(java = true)))
  }

}
