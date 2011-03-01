package org.ensime.server
import org.ensime.util._
import scala.collection.{ immutable, mutable }
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.{ Global }
import org.ardverk.collection._
import scala.collection.JavaConversions._

trait NamespaceTraversal { self: RichPresentationCompiler =>

  trait NamespaceVisitor {
    def visitPackage(sym: Symbol)
    def visitType(sym: Symbol)
  }

  import definitions.{ RootPackage, EmptyPackage }

  def traverse(v: NamespaceVisitor, sym: Symbol) {
    try {
      if (sym.isPackage) {
        v.visitPackage(sym)
        traverseMembers(v, sym)
      } else if (!(sym.nameString.contains("$")) && (sym != NoSymbol) && (sym.tpe != NoType)) {
        if (sym.isClass || sym.isTrait || sym.isModule ||
          sym.isModuleClass || sym.isPackageClass) {
          v.visitType(sym)
        }
      }
    } catch {
      case e => None
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
