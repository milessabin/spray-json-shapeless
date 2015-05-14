package org.ensime.model

import scala.tools.nsc.interactive.Global

import org.ensime.api._

trait Helpers { self: Global =>

  import rootMirror.{ EmptyPackage, RootPackage }

  def applySynonyms(sym: Symbol): List[Symbol] = {
    val members = if (sym.isModule || sym.isModuleClass || sym.isPackageObject) {
      sym.tpe.members
    } else if (sym.isClass || sym.isPackageClass || sym.isPackageObjectClass) {
      sym.companionModule.tpe.members
    } else { List.empty }
    members.toList.filter { _.name.toString == "apply" }
  }

  def constructorSynonyms(sym: Symbol): List[Symbol] = {
    val members = if (sym.isClass || sym.isPackageClass || sym.isPackageObjectClass) {
      sym.tpe.members
    } else if (sym.isModule || sym.isModuleClass || sym.isPackageObject) {
      sym.companionClass.tpe.members
    } else { List.empty }
    members.toList.filter { _.isConstructor }
  }

  def isArrowType(tpe: Type): Boolean = {
    tpe match {
      case _: MethodType => true
      case _: PolyType => true
      case _ => false
    }
  }

  def isNoParamArrowType(tpe: Type): Boolean = {
    tpe match {
      case t: MethodType => t.paramss.flatten.isEmpty
      case t: PolyType => t.paramss.flatten.isEmpty
      case t: Type => false
    }
  }

  def typeOrArrowTypeResult(tpe: Type): Type = {
    tpe match {
      case t: MethodType => t.finalResultType
      case t: PolyType => t.finalResultType
      case t: Type => t
    }
  }

  def completionSignatureForType(tpe: Type): CompletionSignature = {
    if (isArrowType(tpe)) {
      CompletionSignature(
        tpe.paramss.map { sect =>
          sect.map { p => (p.name.toString, typeShortNameWithArgs(p.tpe)) }
        },
        typeShortNameWithArgs(tpe.finalResultType)
      )
    } else CompletionSignature(List.empty, resultTypeName(tpe))
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
    typeShortName(tpe) + (if (tpe.typeArgs.size > 0) {
      "[" +
        tpe.typeArgs.map(typeShortNameWithArgs).mkString(", ") +
        "]"
    } else { "" })
  }

  /**
   *  Return the string used to index a symbol
   */
  def symbolIndexerName(sym: Symbol): String = {
    def typeIndexerName(sym: Symbol): String = {
      val owner = sym.owner
      if (owner.isRoot || owner.isRootPackage) {
        sym.nameString
      } else if (owner.hasPackageFlag) {
        owner.fullName + "." + sym.nameString
      } else {
        typeIndexerName(owner) + "$" + sym.nameString
      }
    }

    if (sym.isType) {
      typeIndexerName(sym)
    } else if (sym.isModule) {
      typeIndexerName(sym) + "$"
    } else {
      symbolIndexerName(sym.owner) + "." + sym.nameString
    }
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
    val s = sym.nameString
    if ((sym.isModule || sym.isModuleClass) && !s.endsWith("$")) s + "$" else s
  }

  /**
   * Returns the type, object, or package symbol uniquely identified by name.
   */
  protected def symbolByName(name: String, rootSymbol: Symbol = RootClass): Option[Symbol] = {
    def segments(name: String): List[Name] = {
      val len = name.length
      if (len == 0) {
        throw new IllegalArgumentException("Empty symbol name")
      }

      val idx = name.indexWhere(ch => ch == '.' || ch == '$')
      idx match {
        case 0 => throw new IllegalArgumentException("Empty symbol part")
        case -1 =>
          // Last part of symbol, no '$' -> a class
          return List(newTypeName(name))
        case _ =>
      }

      val (cur, div, rest) = (name.take(idx), name.charAt(idx), name.drop(idx + 1))
      if (div == '.') {
        if (rest == "") {
          throw new IllegalArgumentException("Unexpected period at end of symbol name")
        } else {
          // part ends with '.' : a package
          newTermName(cur) :: segments(rest)
        }
      } else {
        if (rest == "") {
          // Last part ends with '$': an object or package
          List(newTermName(cur))
        } else if (rest.charAt(0) == '$') {
          // Part ends with "$$": an object
          newTermName(cur) :: segments(rest.drop(1))
        } else {
          // Part ends with "$": a class
          newTypeName(cur) :: segments(rest)
        }
      }
    }
    // Special convenience handling for the case where name is a top-level
    // package, e.g., 'java'.
    val s = segments(name)
    val sym = s.foldLeft[Symbol](rootSymbol) { (sym, name) =>
      sym.info.member(name)
    }
    try {
      sym match {
        case NoSymbol => None
        case sym: Symbol => Some(sym)
        case _ => None
      }
    } catch {
      case e: Throwable => None
    }
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

  /**
   * Return a Symbol representing a package if it exists given the package path (com.foo.bar)
   * @param path The full package path (com.foo.bar)
   * @return Some(packageSymbol) if `path` represents a valid package or None
   */
  def packageSymFromPath(path: String): Option[Symbol] = {
    symbolByName(
      if (path.endsWith("$")) path else path + "$",
      RootPackage
    ).find { s => s.hasPackageFlag }
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

      // the nameString operation is depressingly expensive - mapping to tuples first reduces the overhead.
      val vsPairsAsList: List[(String, Symbol)] = validSyms.map(vs => (vs.nameString, vs))(scala.collection.breakOut)
      vsPairsAsList.sortBy(_._1).map(_._2)
    }

    if (isRoot(parent)) {
      filterAndSort(parent.info.members ++ EmptyPackage.info.members)
    } else {
      filterAndSort(parent.info.members)
    }
  }

  import scala.tools.nsc.symtab.Flags._

  def declaredAs(sym: Symbol): DeclaredAs = {
    if (sym.isMethod)
      DeclaredAs.Method
    else if (sym.isTrait && sym.hasFlag(JAVA))
      DeclaredAs.Interface
    else if (sym.isTrait)
      DeclaredAs.Trait
    else if (sym.isInterface)
      DeclaredAs.Interface
    else if (sym.isModule)
      DeclaredAs.Object
    else if (sym.isModuleClass)
      DeclaredAs.Object
    else if (sym.isClass)
      DeclaredAs.Class
    else if (sym.isPackageClass)
      DeclaredAs.Class

    // check this last so objects are not
    // classified as fields
    else if (sym.isValue || sym.isVariable)
      DeclaredAs.Field
    else
      DeclaredAs.Nil
  }
}
