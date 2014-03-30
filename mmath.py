import math
import llvm.core

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

}

class mMathFuncs(object):
    def __init__(self):
        pass
    def emitabs(self, node):
        return llvm.core.Constant.int(llIntType, 10)

def isIntrinsicMathFunction(func):
    return intrinsics.has_key(func)
       
def GetIntrinsicMathFunctions():
    return intrinsics

