import sys,os

def print_verbatim(s):
    print "\\begin{mylisting}"
    print "\\begin{verbatim}"
    print s
    print "\\end{verbatim}"
    print "\\end{mylisting}"

def print_bold(s):
    print "\\textbf{" + s + "}"

def print_nl():
    print "\\\\"

def print_vspace(height):
    print "\\vspace{" + height + "}"

class FileReader:

    def __init__(self, fin):
        self.fin = fin

    def lines(self):
        for line in self.fin:
            yield line
        yield None


class DataStructure:

    def __init__(self, offset):
        self.offset = offset
    
    def read(self, fin):
        self.name = next(fin)[self.offset + 2:]

        assert next(fin).find("Summary:") > -1

        self.summary = ""
        line = next(fin)
        while line.find("Structure:") == -1:
            self.summary = self.summary + line[self.offset + 2:]
            line = next(fin)

        self.structure = ""
        line = next(fin)
        while line.find("*/") == -1:
            self.structure = self.structure + line[self.offset + 2:]
            line = next(fin)

        self.summary = self.summary.lstrip().rstrip()
        self.structure = self.structure.lstrip().rstrip()
        self.name = self.name.lstrip().rstrip()

    def print_latex(self):
        print_bold(self.name)
        print_nl()
        print self.summary
        print_nl()
        print_verbatim(self.structure)
        print_nl()
        print_vspace("5 mm")
        print "\n"




fin = (FileReader(open("../src/main/scala/org/ensime/protocol/SwankProtocol.scala")).lines())
line = next(fin)
while line:
    i = line.find("Doc DataStructure")
    if i > -1:
        ds = DataStructure(i)
        ds.read(fin)
        ds.print_latex()
    line = next(fin)


