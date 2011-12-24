
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

    def print_latex(self):
        print self.name
        print self.summary
        print self.structure
        print "--------------------------------"


fin = (FileReader(open("../src/main/scala/org/ensime/protocol/SwankProtocol.scala")).lines())
line = next(fin)
while line:
    i = line.find("Doc DataStructure")
    if i > -1:
        ds = DataStructure(i)
        ds.read(fin)
        ds.print_latex()
    line = next(fin)


