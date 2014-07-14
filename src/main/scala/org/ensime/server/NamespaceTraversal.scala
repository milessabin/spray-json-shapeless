package org.ensime.server

trait NamespaceTraversal { self: RichPresentationCompiler =>

  trait NamespaceVisitor {
    def visitPackage(sym: Symbol)
    def visitType(sym: Symbol)
  }

  import rootMirror.EmptyPackage

  def traverse(v: NamespaceVisitor, sym: Symbol) {
    try {
      if (sym.hasPackageFlag) {
        v.visitPackage(sym)
        traverseMembers(v, sym)
      } else if (!sym.nameString.contains("$") && (sym != NoSymbol) && (sym.tpe != NoType)) {
        if (sym.isClass || sym.isTrait || sym.isModule ||
          sym.isModuleClass || sym.isPackageClass) {
          v.visitType(sym)
        }
      }
    } catch {
      case e: Throwable => None
    }
  }

  def traverseMembers(v: NamespaceVisitor, sym: Symbol) {
    def isRoot(s: Symbol) = s.isRoot || s.isRootPackage
    def iter(s: Symbol) {
      if (s != EmptyPackage && !isRoot(s) &&
        // This check is necessary to prevent infinite looping..
        ((isRoot(s.owner) && isRoot(sym)) || (s.owner.fullName == sym.fullName))) {
        traverse(v, s)
      }
    }
    if (isRoot(sym)) {
      EmptyPackage.info.members.foreach(iter)
    }
    sym.info.members.foreach(iter)
  }

}
