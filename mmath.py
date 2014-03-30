import math
import llvm.core
import compiler 
llTruthType    = llvm.core.Type.int(1)
llVoidType     = llvm.core.Type.void()
llIntType      = llvm.core.Type.int()
llFloatType    = llvm.core.Type.float()
llFVec4Type    = llvm.core.Type.vector(llFloatType, 4)
llFVec4PtrType = llvm.core.Type.pointer(llFVec4Type)
llIVec4Type    = llvm.core.Type.vector(llIntType, 4)
#
# Intrinsic math functions
#
intrinsics = {
    # Name    : ( ret type , arg types
      'abs'   : ( int    , [ int        ] )
    , 'exp'   : ( int    , [ int        ] )
    , 'log'   : ( int    , [ int, int   ] )
    , 'sqrt'  : ( int    , [ int        ] )
    , 'mod'   : ( int    , [ int, int   ] )
    , 'fabs'  : ( float  , [ float      ] )
    , 'fexp'  : ( float  , [ float, float ] )
    , 'flog'  : ( float  , [ float, float ] )
    , 'fsqrt' : ( float  , [ float      ] )
    , 'range' : ( list   , [ int        ])
}

class mMathFuncs(object):
    def __init__(self, codegen):
        self.codeGen = codegen
    def emitabs(self, node):
        return llvm.core.Constant.int(llIntType, 10)
    # NOTE: need to pass constants because creating array from dims
    def emitrange(self, node):
        print ";IN RANGE", node
        # get start and end points
        for n in node.args:
            if not isinstance(n, compiler.ast.Const):
                raise Exception("pyllvm err: need to pass range constant values")
        
        if len(node.args) == 1:
            start = 0
            end = node.args[0].value
        else:
            start = node.args[0].value
            end = node.args[1].value

        if(end<start):
            raise Exception("pyllvm err: bad range args")
        # malloc array
        arrTy = llvm.core.Type.array(llIntType, end-start+1)
        m_ptr = self.codeGen.builder.malloc_array(arrTy, llvm.core.Constant.int(llIntType, end-start+1))

        # copy all the values from the stack one into the heap
        zero = llvm.core.Constant.int(llIntType, 0)
        count = 0
        for v in range(start, end+1):
            index = llvm.core.Constant.int(llIntType, count)
            # create value to store
            val = llvm.core.Constant.int(llIntType, v)
            # store values in malloc'd array
            m = self.codeGen.builder.gep(m_ptr, [zero, index])
            self.codeGen.builder.store(val, m)
            count = count+1
        # reset expr to the malloc'd array ptr
        return m_ptr

        # assign values to array


        return start

def isIntrinsicMathFunction(func):
    return intrinsics.has_key(func)
       
def GetIntrinsicMathFunctions():
    return intrinsics

