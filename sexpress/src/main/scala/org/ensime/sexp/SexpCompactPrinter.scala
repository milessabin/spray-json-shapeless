package org.ensime.sexp

object SexpCompactPrinter extends SexpPrinter {

  def print(sexp: Sexp, sb: StringBuilder): Unit = sexp match {
    case atom: SexpAtom => printAtom(atom, sb)
    case SexpData(data) => printData(data, sb)
    case SexpList(els) => printList(els, sb)
    case SexpCons(x, y) => printCons(x, y, sb)
  }

  protected def printCons(x: Sexp, y: Sexp, sb: StringBuilder): Unit = {
    // recursive, could blow up for big trees
    sb.append('(')
    print(x, sb)
    sb.append(" . ")
    print(y, sb)
    sb.append(')')
  }

  protected def printData(data: Map[SexpSymbol, Sexp], sb: StringBuilder): Unit =
    if (data.isEmpty) print(SexpNil, sb)
    else {
      sb.append('(')
      printSeq(data, sb.append(' ')) { el =>
        printSymbol(el._1.value, sb)
        sb.append(' ')
        print(el._2, sb)
      }
      sb.append(')')
    }

  protected def printList(els: List[Sexp], sb: StringBuilder): Unit =
    if (els.isEmpty) print(SexpNil, sb)
    else {
      sb.append('(')
      printSeq(els, sb.append(' ')) {
        print(_, sb)
      }
      sb.append(')')
    }

}
