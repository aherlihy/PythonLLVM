class Symbol(object):
    """
    Structure for symbol with type information.

    >>> s = Symbol("a", float, "variable")
    >>> print s
    a(<type 'float'>)
    >>> s = Symbol("b", float, "variable", cls = "bora")
    >>> print s
    b(<type 'float'>) attrs: cls = bora
    >>> import VecTypes
    >>> s = Symbol("c", VecTypes.vec, "variable")
    >>> print s
    c(<class 'VecTypes.vec'>)
    """

    def __init__(self, name, type, kind, **kwargs):

        assert type is not None

        # print "; [SymbolTable] add %s(ty= %s, knd = %s)" % (name, type, kind)
        self.name  = name
        self.type  = type
        self.kind  = kind

        self.attrs = []

        for (k, v) in kwargs.items():
            self.__dict__[k] = v
            self.attrs.append(k)


    def __str__(self):

        s = "%s(%s)" % (self.name, self.type)
        
        if len(self.attrs) > 0:
            s += " attrs: "

        for k in self.attrs:
            s += "%s = %s" % (k, self.__dict__[k])
            if k != self.attrs[-1]: s += ", "


        return s
        
            
class SymbolTable:
    """
    Symbol table
    """
    
    def __init__(self):

        self.symbols = [('global', {})]      # stack of (function scope name, dict)

        self.genNum  = 0

    def popScope(self):

        assert len(self.symbols) > 1

        del self.symbols[-1]


    def pushScope(self, name):

        self.symbols.append((name, {}))


    def append(self, sym):

        assert isinstance(sym, Symbol)

        d = self.symbols[-1][1]

        # if d.has_key(sym.name):
        #    raise Exception("Symbol %s is already defined" % sym.name)

        d[sym.name] = sym


    def find(self, name):
        """
        Find a symbol with name.
        If a symbol was not found, return None.
        """

        for i in range(len(self.symbols)):

            d = self.symbols[i][1]
            if d.has_key(name):
                return d[name]

        return None


    def lookup(self, name):
        """
        Find a symbol with name.
        If a symbol was not found, raise a exeption.
        """

        for i in range(len(self.symbols)):

            d = self.symbols[i][1]
            if d.has_key(name):
                return d[name]

        raise Exception("Undefine symbol: ", name)

    def genUniqueSymbol(self, type):
        """
        Generate unique symbol.
        """

        nMax = 1000

        baseName = "tmp" 

        done = False
        i = 0
        while 1:

            name = baseName + str(self.genNum)

            if self.find(name) == None:

                newSym = Symbol(name, type, "variable")
                self.append(newSym)

                return newSym

            self.genNum += 1
            i           += 1
            
            if i > nMax:
                raise Exception("Can't define unique symbol.")

def _test():
    import doctest
    doctest.testmod()

if __name__ == '__main__':
    _test()
