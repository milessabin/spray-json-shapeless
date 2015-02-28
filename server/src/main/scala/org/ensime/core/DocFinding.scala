package org.ensime.core
import scala.tools.nsc.util._
import org.ensime.model._
import org.ensime.util.Symbols

// Transform symbols to scaladoc URI paths, with anchors to select specific
// members of a container type.
//
// See scala/src/scaladoc/scala/tools/nsc/doc/base/MemberLookupBase.scala for
// details related to link construction.
trait DocFinding { self: RichPresentationCompiler =>

  private def isRoot(s: Symbol) = (s eq NoSymbol) || s.isRootSymbol || s.isEmptyPackage || s.isEmptyPackageClass

  private def fullPackage(sym: Symbol): String =
    sym.ownerChain.reverse.filterNot(isRoot(_))
      .takeWhile(_.hasPackageFlag).map(_.nameString).mkString(".")

  private def fullTypeName(sym: Symbol, nestedTypeSep: String, nameString: (Symbol => String)): String =
    sym.ownerChain.takeWhile(!_.hasPackageFlag).reverse.map(nameString(_)).mkString(nestedTypeSep)

  private val ScalaPrim = """^scala\.(Boolean|Byte|Char|Double|Float|Int|Long|Short)$""".r
  private val ScalaAny = """^scala\.(Any|AnyVal|AnyRef)$""".r
  private def javaFqn(tpe: Type): String = {
    def nameString(sym: Symbol) = sym.nameString.replace("$", "")
    val sym = tpe.typeSymbol
    val s = if (sym.hasPackageFlag) {
      fullPackage(sym) + ".package"
    } else {
      fullPackage(sym) + "." + fullTypeName(sym, ".", nameString)
    }
    s match {
      case ScalaPrim(datatype) => datatype.toLowerCase()
      case ScalaAny(datatype) => "java.lang.Object"
      case "scala.Array" => {
        tpe.typeArgs.headOption.map { tpe => javaFqn(tpe) + "[]" }.getOrElse(s)
      }
      case _ => s
    }
  }

  protected def scalaFqn(sym: Symbol): String = {
    def nameString(s: Symbol) = s.nameString + (if ((s.isModule || s.isModuleClass) && !s.hasPackageFlag) "$" else "")
    if (sym.isPackageObjectOrClass) {
      fullPackage(sym.owner) + ".package"
    } else if (sym.hasPackageFlag) {
      fullPackage(sym) + ".package"
    } else {
      fullPackage(sym) + "." + fullTypeName(sym, "$", nameString)
    }
  }

  private def linkName(sym: Symbol, java: Boolean): String = {
    if (java) javaFqn(sym.tpe) else scalaFqn(sym)
  }

  private def signatureString(sym: Symbol, java: Boolean): String = {
    sym.nameString + (if (java) {
      if (sym.paramLists.isEmpty) ""
      else sym.paramLists.flatMap(_.map { sym => javaFqn(sym.tpe) }).mkString("(", ", ", ")")
    } else sym.signatureString.replaceAll("[\\s]", ""))
  }

  def docSignatureAt(p: Position): Option[DocSigPair] = {
    symbolAt(p).orElse(typeAt(p).map(_.typeSymbol)).flatMap {
      sym =>
        def docSig(java: Boolean) = {
          val owner = sym.owner
          if (sym.isCaseApplyOrUnapply) {
            DocSig(linkName(owner.companionClass, java), None)
          } else if (sym.isClass || sym.isModule || sym.isTrait || sym.hasPackageFlag)
            DocSig(linkName(sym, java), None)
          else if (owner.isClass || owner.isModule || owner.isTrait || owner.hasPackageFlag) {
            val ownerAtSite = specificOwnerOfsymbolAt(p).getOrElse(owner)
            DocSig(linkName(ownerAtSite, java), Some(signatureString(sym, java)))
          } else
            DocSig(linkName(sym.tpe.typeSymbol, java), None)
        }
        Some(DocSigPair(docSig(false), docSig(true)))
    }
  }

}
