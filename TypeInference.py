import re
import compiler
import sys
from SymbolTable import *
from MUDA import *
from mmath import *
from PyllvmError import PyllvmError
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
            , 'list'   : str
            , 'list'   : list
            }

        self.typeDic.update(GetMUDATypeDic())    # register MUDA type

        self.symbolTable = symTable

        # Register intrinsic functions from MUDA module
        self.intrinsics = GetIntrinsicFunctions()
#        for (k,v) in GetIntrinsicMathFunctions().items():
#            self.intrinsics[k]=v

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
        if name[:4]=='list':
            return list
        if name[:3]=='str':
            return str
        return None

    def getIntrinsicFunctionFromName(self, name):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
        if self.intrinsics.has_key(name):
            return self.intrinsics[name]

        return None

    def inferType(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
        """
        Return type if type inference was succeeded, None if failed.
        """

        assert node is not None

        g  = re.compile("(\w+)\(.*\)")
        op = g.match(str(node)) 

        if op == None:
            raise PyllvmError("Type Inference: Invalid node name?", str(node)) 

        op_name = op.group(1)

        #
        # call the method whose name is "infer + ${op_name}"
        #

        method_name = "infer%s" % op_name
        
        if not callable(getattr(self, method_name)):
            raise PyllvmError("Type Inference: Unknown node name:", op_name)

        method = getattr(self, method_name)

        return method(node) 

    def inferModule(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"

        return self.inferType(node.node)
    
    def inferReturn(self, node):
        return self.inferType(node.value)


    def inferStmt(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"

        return self.inferType(node.nodes[0])

    def checkSwizzleLetter(self, name):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"

        assert len(name) >= 1 and len(name) < 5

        for s in name:
            if not s in ('x', 'y', 'z', 'w'):
                raise PyllvmError("Type Inference: Not a swizzle letter:", name) 

        return True

    def inferGetattr(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
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
                raise PyllvmError("Type Inference: Unknown type:", ty)

        else:
            # vector
            return ty

    def inferDiscard(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"

        return self.inferType(node.expr)

    def inferCallFunc(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"

        assert isinstance(node.node, compiler.ast.Name)

        #print "; => CalFunc:", node
        # Intrinsic function?
        f = self.getIntrinsicFunctionFromName(node.node.name)
        if f is not None:
            #print "; => Intrinsic:", f
            return f[0]
        if isIntrinsicMathFunction(node.node.name):
            x = GetIntrinsicMathFunctions()
            # special case for casting functions
            if(node.node.name=='int'):
                return int
            if(node.node.name=='float'):
                return float
            if(node.node.name=='range' or node.node.name=='zeros'):
                return list
            if x.has_key(node.node.name):
                #return x[node.node.name][0]
                return self.inferType(node.args[0])
            else:
                return void
        
        return self.inferType(node.node)


    def inferUnarySub(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
        return self.inferType(node.expr)

    def inferAdd(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left


    def inferSub(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left

    def inferMul(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left


    def inferDiv(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
    
        left  = self.inferType(node.left)
        right = self.inferType(node.right) 

        if left != right:
            print "; [type inference] Type mismatch found at line %d: left = %s, right = %s" % (node.lineno, left, right)
            print ";                 node = %s" % (node)
            return None

        return left

    def inferAnd(self, node):
        return float
    def inferOr(self, node):
        return float
    def inferNot(self, node):
        return float

    def inferMod(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
    
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
        #print ";TI--" + sys._getframe().f_code.co_name + "----"

        name = node.name

        # Firstly, name of type?
        if self.typeDic.has_key(name):
            return self.typeDic[name]

        # Next, lookup symbol
        # return vec
        return None
    
    def inferName(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
        name = node.name
        if(node.name=='True' or node.name=='False'):
            return int
        # Firstly, name of type?
        if self.typeDic.has_key(name):
            print "; => found type for ", name, "=", self.typeDic[name]
            return self.typeDic[name]
        # Next, lookup symbol from the symbol table.
        sym = self.symbolTable.find(name)
        if sym is not None:
            return sym.type

        print "; => not found. name=", name
        return None

    def inferCompare(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"
        return float


    def inferConst(self, node):
        #print ";TI--" + sys._getframe().f_code.co_name + "----"

        value = node.value
        if value == None:
            return void

        if isinstance(value, type(1.0)):
            return float

        elif isinstance(value, type(1)):
            return int

        elif isinstance(value, type('muda')):
            #STR: changed from str to list
            return list

        else:
            raise PyllvmError("Type Inference: Unknown type of value:", value)
    
    def inferSubscript(self, node):
        ty = None
        if isinstance(node.expr, compiler.ast.Name):
            ty = self.symbolTable.find(node.expr.name).getDim()[0]
        if isinstance(node.expr, compiler.ast.List):
            ty = self.inferType(node.expr.nodes[0])
        if isinstance(node.expr, compiler.ast.CallFunc):
            ty = self.symbolTable.find(node.expr.node.name).getDim()[0]
        if isinstance(node.expr, compiler.ast.Const):
            ty = int
        if ty is None:
            raise PyllvmError("Type Inference: cannot index into value", node.expr)
        return ty
    def inferList(self, node):
        return list
