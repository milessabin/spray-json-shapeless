package org.ensime.model
import scala.tools.nsc.interactive.Global

trait Helpers { self: Global =>

  import rootMirror.{ RootPackage, EmptyPackage }

  def applySynonyms(sym: Symbol): List[Symbol] = {
    val members = if (sym.isModule || sym.isModuleClass || sym.isPackageObject) {
      sym.tpe.members
    } else if (sym.isClass || sym.isPackageClass || sym.isPackageObjectClass) {
      sym.companionModule.tpe.members
    } else { List() }
    members.toList.filter { _.name.toString == "apply" }
  }

  def constructorSynonyms(sym: Symbol): List[Symbol] = {
    val members = if (sym.isClass || sym.isPackageClass || sym.isPackageObjectClass) {
      sym.tpe.members
    } else if (sym.isModule || sym.isModuleClass || sym.isPackageObject) {
      sym.companionClass.tpe.members
    } else { List() }
    members.toList.filter { _.isConstructor }
  }

  def isArrowType(tpe: Type) = {
    tpe match {
      case _: MethodType => true
      case _: PolyType => true
      case _ => false
    }
  }

  def isNoParamArrowType(tpe: Type) = {
    tpe match {
      case t: MethodType => t.paramss.flatten.isEmpty
      case t: PolyType => t.paramss.flatten.isEmpty
      case t: Type => false
    }
  }

  def typeOrArrowTypeResult(tpe: Type) = {
    tpe match {
      case t: MethodType => t.finalResultType
      case t: PolyType => t.finalResultType
      case t: Type => t
    }
  }

  def completionSignatureForType(tpe: Type): CompletionSignature = {
    if (isArrowType(tpe)) {
      CompletionSignature(tpe.paramss.map { sect =>
        sect.map { p => (p.name.toString, typeShortNameWithArgs(p.tpe)) }
      },
        typeShortNameWithArgs(tpe.finalResultType))
    } else CompletionSignature(List(), resultTypeName(tpe))
  }

  /**
   * Convenience method to generate a String describing the type. Omit
   * the package name. Include the arguments postfix.
   *
   * Used for type-names of symbol and member completions
   */
  def typeShortNameWithArgs(tpe: Type): String = {
    if (isArrowType(tpe)) {
      (tpe.paramss.map { sect =>
        "(" +
          sect.map { p => typeShortNameWithArgs(p.tpe) }.mkString(", ") +
          ")"
      }.mkString(" => ")
        + " => " +
        typeShortNameWithArgs(tpe.finalResultType))
    } else resultTypeName(tpe)
  }

  def resultTypeName(tpe: Type): String = {
    typeShortName(tpe) + (if (tpe.typeArgs.length > 0) {
      "[" +
        tpe.typeArgs.map(typeShortNameWithArgs).mkString(", ") +
        "]"
    } else { "" })
  }

  /**
   * Generate qualified name, without args postfix.
   */
  def typeFullName(tpe: Type): String = {
    def nestedClassName(sym: Symbol): String = {
      outerClass(sym) match {
        case Some(outerSym) =>
          nestedClassName(outerSym) + "$" + typeShortName(sym)
        case None => typeShortName(sym)
      }
    }
    val typeSym = tpe.typeSymbol
    if (typeSym.isNestedClass) {
      typeSym.enclosingPackage.fullName + "." + nestedClassName(typeSym)
    } else {
      typeSym.enclosingPackage.fullName + "." + typeShortName(typeSym)
    }
  }

  def typeShortName(tpe: Type): String = {
    if (tpe.typeSymbol != NoSymbol) typeShortName(tpe.typeSymbol)
    else tpe.toString()
  }

  def typeShortName(sym: Symbol): String = {
    if (sym.isModule || sym.isModuleClass) sym.nameString + "$"
    else sym.nameString
  }

  /* Give the outerClass of a symbol representing a nested type */
  def outerClass(typeSym: Symbol): Option[Symbol] = {
    try {
      if (typeSym.isNestedClass) {
        Some(typeSym.outerClass)
      } else None
    } catch {
      // TODO accessing outerClass sometimes throws java.lang.Error
      // Notably, when tpe = scala.Predef$Class
      case e: java.lang.Error => None
    }
  }

  def companionTypeOf(tpe: Type): Option[Type] = {
    val sym = tpe.typeSymbol
    if (sym != NoSymbol) {
      if (sym.isModule || sym.isModuleClass) {
        val comp = sym.companionClass
        if (comp != NoSymbol && comp.tpe != tpe) {
          Some(comp.tpe)
        } else None
      } else if (sym.isTrait || sym.isClass || sym.isPackageClass) {
        val comp = sym.companionModule
        if (comp != NoSymbol && comp.tpe != tpe) {
          Some(comp.tpe)
        } else None
      } else None
    } else None
  }

  def packageSymFromPath(path: String): Option[Symbol] = {
    val candidates = symsAtQualifiedPath(path, RootPackage)
    candidates.find { s => s.hasPackageFlag }
  }

  // Where path is the qualified name of a symbol that is a direct or
  // indirect member of rootSym, without containing the name of rootSym.
  def symsAtQualifiedPath(path: String, rootSym: Symbol): List[Symbol] = {
    def memberSymsNamed(sym: Symbol, name: String) = {
      (sym.info.members ++ sym.info.decls).filter { s =>
        s.nameString == name && s != EmptyPackage && s != RootPackage
      }
    }
    if (path == "") List(rootSym)
    else {
      val pathSegs = path.split("\\.")
      pathSegs.foldLeft(List(rootSym)) { (baseSyms, seg) =>
        baseSyms.flatMap { s =>
          memberSymsNamed(s, seg)
        }
      }
    }
  }

  /*
  * Get the valid member symbols of the package denoted by aSym.
  */
  def packageMembers(parent: Symbol): Iterable[Symbol] = {

    def isRoot(s: Symbol) = s.isRoot || s.isRootPackage

    def filterAndSort(symbols: Iterable[Symbol]) = {
      val validSyms = symbols.filter { s =>
        s != EmptyPackage && !isRoot(s) &&
          // This check is necessary to prevent infinite looping..
          ((isRoot(s.owner) && isRoot(parent)) || (s.owner.fullName == parent.fullName))
      }
      validSyms.toList.sortWith { (a, b) => a.nameString <= b.nameString }
    }

    if (isRoot(parent)) {
      filterAndSort(parent.info.members ++ EmptyPackage.info.members)
    } else {
      filterAndSort(parent.info.members)
    }
  }

  import scala.tools.nsc.symtab.Flags._

  def declaredAs(sym: Symbol): scala.Symbol = {
    import org.ensime.util.{ Symbols => S }
    if (sym.isMethod)
      S.Method
    else if (sym.isTrait)
      S.Trait
    else if (sym.isTrait && sym.hasFlag(JAVA))
      S.Interface
    else if (sym.isInterface)
      S.Interface
    else if (sym.isModule)
      S.Object
    else if (sym.isModuleClass)
      S.Object
    else if (sym.isClass)
      S.Class
    else if (sym.isPackageClass)
      S.Class

    // check this last so objects are not
    // classified as fields
    else if (sym.isValue || sym.isVariable)
      S.Field
    else S.Nil
  }

  def symbolSummary(sym: Symbol): Map[String, Any] = {
    import scala.tools.nsc.symtab.Flags._
    Map(
      "name" -> sym.toString(),
      "  isMethod" -> sym.isMethod,
      "  isAbstractClass" -> sym.isAbstractClass,
      "  isPackage" -> sym.hasPackageFlag,
      "  isValue" -> sym.isValue,
      "  isVariable" -> sym.isVariable,
      "  isClass" -> sym.isClass,
      "  isType" -> sym.isType,
      "  isTrait" -> sym.isTrait,
      "  isInterface" -> sym.isInterface,
      "  isModule" -> sym.isModule,
      "  isModuleClass" -> sym.isModuleClass,
      "  isConstructor" -> sym.isConstructor,
      "  hasAccessorFlag" -> sym.hasFlag(ACCESSOR),
      "  hasLocalFlag" -> sym.hasFlag(LOCAL),
      "  isCase" -> sym.isCase,
      "  isCaseAccessor" -> sym.isCaseAccessor,
      "  isValueParameter" -> sym.isValueParameter,
      "  isTypeParameter" -> sym.isTypeParameter,
      "  isSynthetic" -> sym.isSynthetic,
      "  isMutable" -> sym.isMutable,
      "  isFinal" -> sym.isFinal,
      "  isGetter" -> sym.isGetter,
      "  isSetter" -> sym.isSetter,
      "  hasTraitFlag" -> sym.hasFlag(TRAIT)
    )
  }

}
