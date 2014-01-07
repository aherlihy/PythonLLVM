import re
import compiler

from SymbolTable import *
from MUDA import *

class void(object):
    """
    Represents void type
    """
    def __init__(self):
        pass


class TypeInference(object):
    """
    Simple type inference mechanism for python AST.
    >>> t = TypeInference()
    >>> t.inferType(compiler.parse("1+3"))
    <type 'int'>
    """

    def __init__(self, symTable):

        assert isinstance(symTable, SymbolTable)

        # First class types
        self.typeDic = {
              'int'    : int
            , 'float'  : float
            , 'void'   : void
            , 'string' : str
            }

        self.typeDic.update(GetMUDATypeDic())    # register MUDA type

        self.symbolTable = symTable

        # Register intrinsic functions from MUDA module
        self.intrinsics = GetIntrinsicFunctions()

        for (k, v) in self.intrinsics.items():
            retTy  = v[0]
            argTys = v[1]
            sym = Symbol(k, retTy, "function", argtypes = argTys)
            self.symbolTable.append(sym)


    def isFloatType(self, ty):
        if (ty == float or
            ty == vec     ):
            return True

        return False


    def isNameOfFirstClassType(self, name):
        if self.typeDic.has_key(name):
            return self.typeDic[name]

        return None

    def getIntrinsicFunctionFromName(self, name):
        if self.intrinsics.has_key(name):
            return self.intrinsics[name]

        return None

    def inferType(self, node):
        """
        Return type if type inference was succeeded, None if failed.
        """

        assert node is not None

        g  = re.compile("(\w+)\(.*\)")
        op = g.match(str(node)) 

        if op == None:
            raise Exception("Invalid node name?", str(node)) 

        op_name = op.group(1)

        #
        # call the method whose name is "infer + ${op_name}"
        #

        method_name = "infer%s" % op_name
        
        if not callable(getattr(self, method_name)):
            raise Exception("Unknown node name:", op_name)

        method = getattr(self, method_name)

        return method(node) 

    def inferModule(self, node):

        return self.inferType(node.node)
    

    def inferStmt(self, node):

        return self.inferType(node.nodes[0])

    def checkSwizzleLetter(self, name):

        assert len(name) >= 1 and len(name) < 5

        for s in name:
            if not s in ('x', 'y', 'z', 'w'):
                raise Exception("Not a swizzle letter:", name) 

        return True

    def inferGetattr(self, node):
        """
        a.x
        a.xyz
        a.xyzw
        
        node.expr must be a vector type.
        """

        ty = self.inferType(node.expr)
        assert ty == vec, "swizzle pattern must be specified for vector variable, but variable has type %s: %s" % (ty, node)

        swizzleName = node.attrname
        self.checkSwizzleLetter(swizzleName)

        if len(swizzleName) is 1:
            # scalar
            if ty == vec:
                return float
            else:
                raise Exception("Unknown type:", ty)

        else:
            # vector
            return ty

    def inferDiscard(self, node):

        return self.inferType(node.expr)

    def inferCallFunc(self, node):

        assert isinstance(node.node, compiler.ast.Name)

        print "; => CalFunc:", node

        # Intrinsic function?
        f = self.getIntrinsicFunctionFromName(node.node.name)
        if f is not None:
            print "; => Intrinsic:", f
            return f[0]

        
        return self.inferType(node.node)


    def inferUnarySub(self, node):

        return self.inferType(node.expr)

    def inferAdd(self, node):
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left


    def inferSub(self, node):
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left

    def inferMul(self, node):
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left


    def inferDiv(self, node):
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left


    #
    # -- Leaf
    #

    def inferAssName(self, node):

        name = node.name

        # Firstly, name of type?
        if self.typeDic.has_key(name):
            return self.typeDic[name]

        # Next, lookup symbol
        # return vec
        return None
    
    def inferName(self, node):

        name = node.name

        # Firstly, name of type?
        if self.typeDic.has_key(name):
            print "; => found type for ", name
            return self.typeDic[name]

        # Next, lookup symbol from the symbol table.
        sym = self.symbolTable.find(name)
        if sym is not None:
            return sym.type

        print "; => not found. name=", name
        return None


    def inferConst(self, node):

        value = node.value

        if value == None:
            return void

        if isinstance(value, type(1.0)):
            return float

        elif isinstance(value, type(1)):
            return int

        elif isinstance(value, type('muda')):
            return str

        else:
            raise Exception("Unknown type of value:", value)
