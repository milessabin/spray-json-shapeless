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

def print_indent():
    print "\\indent"

class FileReader:

    def __init__(self, fin):
        self.fin = fin

    def lines(self):
        for line in self.fin:
            yield line
        yield None


def read_until(fin,stop,offset):
    result = ""
    line = next(fin)
    while line.find(stop) == -1:
        result = result + line[offset:]
        line = next(fin)
    return result.lstrip().rstrip()


class ChangeLog:

    def __init__(self, offset):
        self.offset = offset

    def read(self, fin):
        off = self.offset + 2
        self.log = read_until(fin,"*/",off)

    def print_latex(self):
        print self.log


class DataStructure:

    def __init__(self, offset):
        self.offset = offset

    def read(self, fin):
        off = self.offset + 2
        self.name = read_until(fin,"Summary:",off)
        self.summary = read_until(fin,"Structure:",off)
        self.structure = read_until(fin,"*/",off)

    def print_latex(self):
        print_bold(self.name)
        print_nl()
        print "\\begin{quote}"
        print self.summary
        print_nl()
        print_verbatim(self.structure)
        print "\\end{quote}"
        print_vspace("5 mm")
        print "\n"



class Event:

    def __init__(self, offset):
        self.offset = offset

    def read(self, fin):
        off = self.offset + 2
        self.name = read_until(fin,"Summary:",off)
        self.summary = read_until(fin,"Structure:",off)
        self.structure = read_until(fin,"*/",off)

    def print_latex(self):
        print_bold(self.name)
        print_nl()
        print "\\begin{quote}"
        print self.summary
        print_nl()
        print_verbatim(self.structure)
        print "\\end{quote}"
        print_vspace("5 mm")
        print "\n"

class RPCCall:

    def __init__(self, offset):
        self.offset = offset

    def read(self, fin):
        off = self.offset + 2
        self.name = read_until(fin,"Summary:",off)
        self.summary = read_until(fin,"Arguments:",off)
        self.arguments = read_until(fin,"Return:",off)
        self.return_struct = read_until(fin,"Example call:",off)
        self.example_call = read_until(fin,"Example return:",off)
        self.example_return = read_until(fin,"*/",off)

    def print_latex(self):
        print_bold(self.name)
        print_nl()

        print "\\begin{quote}"
        print self.summary + "\\\\\\\\"

        print_bold("Arguments:")
        if self.arguments == "None":
            print " None\\\\\\\\"
        else:
            print_verbatim(self.arguments)

        print_bold("Return:")
        if self.return_struct == "None":
            print " None\\\\\\\\"
        else:
            print_verbatim(self.return_struct)

        print_bold("Example Call:")
        print_verbatim(self.example_call)

        print_bold("Example Return:")
        print_verbatim(self.example_return)
        print "\\end{quote}"
        print_vspace("5 mm")
        print "\n"


class Property:

    def __init__(self, offset):
        self.offset = offset

    def read(self, fin):
        off = self.offset + 2
        self.name = read_until(fin,"Summary:",off)
        self.summary = read_until(fin,"Arguments:",off)
        self.arguments = read_until(fin,"*/",off)

    def print_latex(self):
        print_bold(self.name)
        print_nl()

        print "\\begin{quote}"
        print self.summary + "\\\\\\\\"

        print_bold("Arguments:")
        if self.arguments == "None":
            print " None\\\\\\\\"
        else:
            print_verbatim(self.arguments)

        print "\\end{quote}"
        print_vspace("5 mm")
        print "\n"


mode = sys.argv[1]
assert mode in set(["data", "rpc", "version", "events", "changelog", "property"])

input_file = sys.argv[2]
fin = (FileReader(open(input_file)).lines())
line = next(fin)
while line:
    if mode == "data":
        i = line.find("Doc DataStructure:")
        if i > -1:
            handler = DataStructure(i)
            handler.read(fin)
            handler.print_latex()

    elif mode == "rpc":
        i = line.find("Doc RPC:")
        if i > -1:
            handler = RPCCall(i)
            handler.read(fin)
            handler.print_latex()

    elif mode == "property":
        i = line.find("Doc Property:")
        if i > -1:
            handler = Property(i)
            handler.read(fin)
            handler.print_latex()

    elif mode == "events":
        i = line.find("Doc Event:")
        if i > -1:
            handler = Event(i)
            handler.read(fin)
            handler.print_latex()

    elif mode == "changelog":
        i = line.find("Protocol Change Log:")
        if i > -1:
            handler = ChangeLog(i)
            handler.read(fin)
            handler.print_latex()

    elif mode == "version":
        key = "Protocol Version: "
        i = line.find("Protocol Version: ")
        if i > -1:
            print line[i + len(key):].rstrip()




    line = next(fin)


