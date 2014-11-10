package org.ensime.sexp

import scala.annotation.tailrec

/**
 * The output is a non-standard interpretation of "pretty lisp" ---
 * emacs style formatting requires counting the length of the text on
 * the current line and indenting off that, which is not so easy when
 * all you have is a `StringBuilder`.
 */
trait SexpPrettyPrinter extends SexpPrinter {
  val Indent = 2

  def print(sexp: Sexp, sb: StringBuilder): Unit = print(sexp, sb, 0)

  private def print(sexp: Sexp, sb: StringBuilder, indent: Int): Unit = sexp match {
    case SexpData(data) => printData(data, sb, indent)
    case SexpList(els) => printList(els, sb, indent)
    case SexpCons(x, y) => printCons(x, y, sb, indent)
    case atom: SexpAtom => printAtom(atom, sb)
  }

  protected def printCons(x: Sexp, y: Sexp, sb: StringBuilder, indent: Int): Unit = {
    // recursive, could blow up for big trees
    sb.append('(')
    print(x, sb, indent)
    sb.append(" .\n")
    printIndent(sb, indent + Indent)
    print(y, sb, indent + Indent)
    sb.append(')')
  }

  protected def printData(data: Map[SexpSymbol, Sexp], sb: StringBuilder, indent: Int): Unit =
    if (data.isEmpty) print(SexpNil, sb)
    else {
      sb.append("(\n")
      printSeq(data, sb.append('\n')) { el =>
        printIndent(sb, indent + Indent)
        printSymbol(el._1.value, sb)
        sb.append(' ')
        print(el._2, sb, indent + Indent)
      }
      sb.append('\n')
      printIndent(sb, indent)
      sb.append(')')
    }

  protected def printList(els: List[Sexp], sb: StringBuilder, indent: Int): Unit =
    if (els.isEmpty) print(SexpNil, sb)
    else {
      sb.append('(')
      printSeq(els, { sb.append("\n"); printIndent(sb, indent + Indent) }) {
        print(_, sb, indent + Indent)
      }
      sb.append(')')
    }

  protected def printIndent(sb: StringBuilder, indent: Int): Unit =
    (0 until indent) foreach { _ =>
      sb.append(' ')
    }

}

object SexpPrettyPrinter extends SexpPrettyPrinter
