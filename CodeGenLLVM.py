#!/usr/bin/env python

import os, sys
import re
import compiler

import llvm.core

# from VecTypes import *
from MUDA import *
from TypeInference import *
from SymbolTable import *


symbolTable    = SymbolTable() 
typer          = TypeInference(symbolTable)

llVoidType     = llvm.core.Type.void()
llIntType      = llvm.core.Type.int()
llFloatType    = llvm.core.Type.float()
llFVec4Type    = llvm.core.Type.vector(llFloatType, 4)
llFVec4PtrType = llvm.core.Type.pointer(llFVec4Type)
llIVec4Type    = llvm.core.Type.vector(llIntType, 4)

def toLLVMTy(ty):

    floatTy = llvm.core.Type.float()

    if ty is None:
        return llVoidType

    d = {
          float : llFloatType
        , int   : llIntType
        , vec   : llFVec4Type
        , void  : llVoidType
        # str   : TODO
        }

    if d.has_key(ty):
        return d[ty]

    raise Exception("Unknown type:", ty)
        
        

class CodeGenLLVM:
    """
    LLVM CodeGen class 
    """

    def __init__(self):

        self.body             = ""
        self.globalscope      = ""

        self.module           = llvm.core.Module.new("module")
        self.funcs            = []
        self.func             = None # Current function
        self.builder          = None

        self.currFuncRetType  = None
        self.prevFuncRetNode  = None    # for reporiting err

        self.externals        = {}


    def visitModule(self, node):

        # emitExternalSymbols() should be called before self.visit(node.node)
        self.emitExternalSymbols()

        self.visit(node.node)

        print self.module   # Output LLVM code to stdout.
        print self.emitCommonHeader()


    def visitPrint(self, node):
        return None # Discard


    def visitPrintnl(self, node):
        return None # Discard


    def visitReturn(self, node):

        ty   = typer.inferType(node.value)
        print "; Return ty = ", ty

        # Return(Const(None))
        if isinstance(node.value, compiler.ast.Const):
            if node.value.value == None:
                self.currFuncRetType = void
                self.prevFuncRetNode = node
                return self.builder.ret_void()
                
        expr = self.visit(node.value)

        if self.currFuncRetType is None:
            self.currFuncRetType = ty
            self.prevFuncRetNode = node

        elif self.currFuncRetType != ty:
            raise Exception("Different type for return expression: expected %s(lineno=%d, %s) but got %s(lineno=%d, %s)" % (self.currFuncRetType, self.prevFuncRetNode.lineno, self.prevFuncRetNode, ty, node.lineno, node))

        return self.builder.ret(expr)

    def mkFunctionSignature(self, retTy, node):

        # Argument should have default value which represents type of argument.
        if len(node.argnames) != len(node.defaults):
            raise Exception("Function argument should have default values which represents type of the argument:", node)

        argLLTys = []

        for (name, tyname) in zip(node.argnames, node.defaults):

            assert isinstance(tyname, compiler.ast.Name)

            ty = typer.isNameOfFirstClassType(tyname.name)
            if ty is None:
                raise Exception("Unknown name of type:", tyname.name)

            llTy = toLLVMTy(ty)
            
            # vector argument is passed by pointer.
            # if llTy == llFVec4Type:
            #     llTy = llFVec4PtrType

            argLLTys.append(llTy)

        funcLLVMTy = llvm.core.Type.function(retTy, argLLTys)
        func = llvm.core.Function.new(self.module, funcLLVMTy, node.name)

        # Assign name for each arg
        for i, name in enumerate(node.argnames):

            # if llTy == llFVec4Type:
            #     argname = name + "_p"
            # else: 
            #     argname = name
            argname = name

            func.args[i].name = argname


        return func
        
    def visitFunction(self, node):

        """
        Do nasty trick to handle return type of function correctly.

        We visit node AST two times.
        First pass just determines return type of the function
        (All LLVM code body generated are discarded).
        Then second pass we emit LLVM code body with return type found
        in the first pass.
        """

        # init
        self.currFuncRetType = None 
        

        symbolTable.pushScope(node.name)
        retLLVMTy    = llvm.core.Type.void() # Dummy
        func         = self.mkFunctionSignature(retLLVMTy, node)
        entry        = func.append_basic_block("entry")
        builder      = llvm.core.Builder.new(entry)
        self.func    = func
        self.builder = builder
        self.funcs.append(func)

        # Add function argument to symblol table.
        # And emit function prologue.
        for i, (name, tyname) in enumerate(zip(node.argnames, node.defaults)):

            ty = typer.isNameOfFirstClassType(tyname.name)

            bufSym = symbolTable.genUniqueSymbol(ty)

            llTy = toLLVMTy(ty)

            # if llTy == llFVec4Type:
            #     # %name.buf = alloca ty
            #     # %tmp = load %arg
            #     # store %tmp, %name.buf
            #     allocaInst = self.builder.alloca(llTy, bufSym.name)
            #     pTy = llFVec4PtrType
            #     tmpSym     = symbolTable.genUniqueSymbol(ty)
            #     loadInst   = self.builder.load(func.args[i], tmpSym.name)
            #     storeInst  = self.builder.store(loadInst, allocaInst)
            #     symbolTable.append(Symbol(name, ty, "variable", llstorage=allocaInst))
            # else:
            #     # %name.buf = alloca ty
            #     # store val, %name.buf
            #     allocaInst = self.builder.alloca(llTy, bufSym.name)
            #     storeInst  = self.builder.store(func.args[i], allocaInst)
            #     symbolTable.append(Symbol(name, ty, "variable", llstorage=allocaInst))
            # %name.buf = alloca ty
            # store val, %name.buf
            allocaInst = self.builder.alloca(llTy, bufSym.name)
            storeInst  = self.builder.store(func.args[i], allocaInst)
            symbolTable.append(Symbol(name, ty, "variable", llstorage=allocaInst))

        self.visit(node.code)
        symbolTable.popScope()

        # Discard llvm code except for return type 
        func.delete()
        del(self.funcs[-1])


        symbolTable.pushScope(node.name)
        retLLVMTy    = toLLVMTy(self.currFuncRetType)
        func         = self.mkFunctionSignature(retLLVMTy, node)
        entry        = func.append_basic_block("entry")
        builder      = llvm.core.Builder.new(entry)
        self.func    = func
        self.builder = builder
        self.funcs.append(func)

        # Add function argument to symblol table.
        # And emit function prologue.
        for i, (name, tyname) in enumerate(zip(node.argnames, node.defaults)):

            ty = typer.isNameOfFirstClassType(tyname.name)

            bufSym = symbolTable.genUniqueSymbol(ty)

            llTy = toLLVMTy(ty)

            # if llTy == llFVec4Type:
            #     # %name.buf = alloca ty
            #     # %tmp = load %arg
            #     # store %tmp, %name.buf
            #     allocaInst = self.builder.alloca(llTy, bufSym.name)
            #     pTy = llFVec4PtrType
            #     tmpSym     = symbolTable.genUniqueSymbol(ty)
            #     loadInst   = self.builder.load(func.args[i], tmpSym.name)
            #     storeInst  = self.builder.store(loadInst, allocaInst)
            #     symbolTable.append(Symbol(name, ty, "variable", llstorage=allocaInst))
            # else:
            #     # %name.buf = alloca ty
            #     # store val, %name.buf
            #     allocaInst = self.builder.alloca(llTy, bufSym.name)
            #     storeInst  = self.builder.store(func.args[i], allocaInst)
            #     symbolTable.append(Symbol(name, ty, "variable", llstorage=allocaInst))
            # %name.buf = alloca ty
            # store val, %name.buf
            allocaInst = self.builder.alloca(llTy, bufSym.name)
            storeInst  = self.builder.store(func.args[i], allocaInst)
            symbolTable.append(Symbol(name, ty, "variable", llstorage=allocaInst))

        self.visit(node.code)

        if self.currFuncRetType is None:
            # Add ret void.
            self.builder.ret_void()
            self.currFuncRetType = void

        symbolTable.popScope()

        # Register function to symbol table
        symbolTable.append(Symbol(node.name, self.currFuncRetType, "function", llstorage=func))



    def visitStmt(self, node):

        for node in node.nodes:

            print "; [stmt]", node  

            self.visit(node)

    def visitAssign(self, node):

        if len(node.nodes) != 1:
            raise Exception("TODO:", node)

        print "; [Asgn]"
        rTy     = typer.inferType(node.expr)
        print "; [Asgn]. rTy = ", rTy

        print "; [Asgn]. node.expr = ", node.expr
        rLLInst = self.visit(node.expr)
        print "; [Asgn]. rhs = ", rLLInst

        lhsNode = node.nodes[0]

        lTy = None
        if isinstance(lhsNode, compiler.ast.AssName):

            sym = symbolTable.find(lhsNode.name)
            if sym is None:
                # The variable appears here firstly.

                # alloc storage
                llTy = toLLVMTy(rTy)
                llStorage = self.builder.alloca(llTy, lhsNode.name) 

                sym = Symbol(lhsNode.name, rTy, "variable", llstorage = llStorage)
                symbolTable.append(sym)
                print "; [Sym] New symbol added: ", sym

                lTy = rTy

            else:
                # symbol is already defined.
                lTy = sym.type



        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s: %s" % (lTy, rTy, node)) 

        lSym = symbolTable.find(lhsNode.name)

        storeInst = self.builder.store(rLLInst, lSym.llstorage)

        print ";", storeInst

        print "; [Asgn]", node  
        print "; [Asgn] nodes = ", node.nodes 
        print "; [Asgn] expr  = ", node.expr

        # No return

    def visitIf(self, node):

        print "; ", node.tests
        print "; ", node.else_

        raise Exception("muda")

    def emitVCompare(self, op, lInst, rInst):

        d = { "==" : llvm.core.RPRED_OEQ
            , "!=" : llvm.core.RPRED_ONE
            , ">"  : llvm.core.RPRED_OGT 
            , ">=" : llvm.core.RPRED_OGE
            , "<"  : llvm.core.RPRED_OLT 
            , "<=" : llvm.core.RPRED_OLE
            }

        llop = d[op]

        i0 = llvm.core.Constant.int(llIntType, 0);
        i1 = llvm.core.Constant.int(llIntType, 1);
        i2 = llvm.core.Constant.int(llIntType, 2);
        i3 = llvm.core.Constant.int(llIntType, 3);
        vizero = llvm.core.Constant.vector([llvm.core.Constant.int(llIntType, 0)] * 4)

        tmp0  = symbolTable.genUniqueSymbol(float)
        tmp1  = symbolTable.genUniqueSymbol(float)
        tmp2  = symbolTable.genUniqueSymbol(float)
        tmp3  = symbolTable.genUniqueSymbol(float)
        tmp4  = symbolTable.genUniqueSymbol(float)
        tmp5  = symbolTable.genUniqueSymbol(float)
        tmp6  = symbolTable.genUniqueSymbol(float)
        tmp7  = symbolTable.genUniqueSymbol(float)
        le0   = self.builder.extract_element(lInst, i0, tmp0.name) 
        le1   = self.builder.extract_element(lInst, i1, tmp1.name) 
        le2   = self.builder.extract_element(lInst, i2, tmp2.name) 
        le3   = self.builder.extract_element(lInst, i3, tmp3.name) 
        re0   = self.builder.extract_element(rInst, i0, tmp4.name) 
        re1   = self.builder.extract_element(rInst, i1, tmp5.name) 
        re2   = self.builder.extract_element(rInst, i2, tmp6.name) 
        re3   = self.builder.extract_element(rInst, i3, tmp7.name) 

        ftmp0 = symbolTable.genUniqueSymbol(float)
        ftmp1 = symbolTable.genUniqueSymbol(float)
        ftmp2 = symbolTable.genUniqueSymbol(float)
        ftmp3 = symbolTable.genUniqueSymbol(float)

        f0 = self.builder.fcmp(llop, le0, re0, ftmp0.name)
        f1 = self.builder.fcmp(llop, le1, re1, ftmp1.name)
        f2 = self.builder.fcmp(llop, le2, re2, ftmp2.name)
        f3 = self.builder.fcmp(llop, le3, re3, ftmp3.name)

        # i1 -> i32
        ctmp0 = symbolTable.genUniqueSymbol(int)
        ctmp1 = symbolTable.genUniqueSymbol(int)
        ctmp2 = symbolTable.genUniqueSymbol(int)
        ctmp3 = symbolTable.genUniqueSymbol(int)
        c0 = self.builder.sext(f0, llIntType)
        c1 = self.builder.sext(f1, llIntType)
        c2 = self.builder.sext(f2, llIntType)
        c3 = self.builder.sext(f3, llIntType)

        # pack
        s0 = symbolTable.genUniqueSymbol(llIVec4Type)
        s1 = symbolTable.genUniqueSymbol(llIVec4Type)
        s2 = symbolTable.genUniqueSymbol(llIVec4Type)
        s3 = symbolTable.genUniqueSymbol(llIVec4Type)

        r0 = self.builder.insert_element(vizero, c0, i0, s0.name)
        r1 = self.builder.insert_element(r0    , c1, i1, s1.name)
        r2 = self.builder.insert_element(r1    , c2, i2, s2.name)
        r3 = self.builder.insert_element(r2    , c3, i3, s3.name)

        return r3

    def visitCompare(self, node):

        print "; ", node.expr
        print "; ", node.ops[0]

        lTy = typer.inferType(node.expr)
        rTy = typer.inferType(node.ops[0][1])

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.expr)
        rLLInst = self.visit(node.ops[0][1])

        op  = node.ops[0][0]

        if rTy == vec:
            return self.emitVCompare(op, lLLInst, rLLInst)

        if op == "<":
            print "muda"
        elif op == ">":
            print "muda"
        else:
            raise Exception("Unknown operator:", op)

        raise Exception("muda")

    def visitUnarySub(self, node):

        ty       = typer.inferType(node.expr)
        e        = self.visit(node.expr)
        zeroInst = llvm.core.Constant.null(toLLVMTy(ty))
        tmpSym   = symbolTable.genUniqueSymbol(ty)

        subInst = self.builder.sub(zeroInst, e, tmpSym.name)

        return subInst

    def visitGetattr(self, node):

        d = { 'x' : llvm.core.Constant.int(llIntType, 0)
            , 'y' : llvm.core.Constant.int(llIntType, 1)
            , 'z' : llvm.core.Constant.int(llIntType, 2)
            , 'w' : llvm.core.Constant.int(llIntType, 3)
            }


        ty = typer.inferType(node)
        print "; getattr: expr", node.expr
        print "; getattr: attrname", node.attrname
        print "; getattr: ty", ty

        rLLInst  = self.visit(node.expr)
        tmpSym   = symbolTable.genUniqueSymbol(ty)

        if len(node.attrname) == 1:
            # emit extract element
            s = node.attrname[0]

            inst = self.builder.extract_element(rLLInst, d[s], tmpSym.name)

        return inst

        

    def visitAdd(self, node):

        lTy = typer.inferType(node.left)
        rTy = typer.inferType(node.right)

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.left)
        rLLInst = self.visit(node.right)
        
        tmpSym = symbolTable.genUniqueSymbol(lTy)

        addInst = self.builder.add(lLLInst, rLLInst, tmpSym.name)
        print "; [AddOp] inst = ", addInst

        return addInst

    def visitSub(self, node):

        lTy = typer.inferType(node.left)
        rTy = typer.inferType(node.right)

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.left)
        rLLInst = self.visit(node.right)
        
        tmpSym = symbolTable.genUniqueSymbol(lTy)

        subInst = self.builder.sub(lLLInst, rLLInst, tmpSym.name)
        print "; [SubOp] inst = ", subInst

        return subInst


    def visitMul(self, node):

        lTy = typer.inferType(node.left)
        rTy = typer.inferType(node.right)

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.left)
        rLLInst = self.visit(node.right)
        
        tmpSym = symbolTable.genUniqueSymbol(lTy)

        mulInst = self.builder.mul(lLLInst, rLLInst, tmpSym.name)
        print "; [MulOp] inst = ", mulInst

        return mulInst


    def visitDiv(self, node):

        lTy = typer.inferType(node.left)
        rTy = typer.inferType(node.right)

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.left)
        rLLInst = self.visit(node.right)
        
        tmpSym = symbolTable.genUniqueSymbol(lTy)

        if typer.isFloatType(lTy):
            divInst = self.builder.fdiv(lLLInst, rLLInst, tmpSym.name)
        else:
            raise Exception("TODO: div for type: ", lTy)

        print "; [DIvOp] inst = ", divInst

        return divInst


    def handleInitializeTypeCall(self, ty, args):

        llty = toLLVMTy(ty)

        if llty == llFVec4Type:

            i0 = llvm.core.Constant.int(llIntType, 0);
            i1 = llvm.core.Constant.int(llIntType, 1);
            i2 = llvm.core.Constant.int(llIntType, 2);
            i3 = llvm.core.Constant.int(llIntType, 3);

            vf = llvm.core.Constant.vector([llvm.core.Constant.real(llFloatType, "0.0")] * 4)

            # args =  [List([float, float, float, float])]
            #      or [List(float)]
            
            if isinstance(args[0], list):
                elems = args[0]
            else:
                elems = [args[0], args[0], args[0], args[0]]

            s0 = symbolTable.genUniqueSymbol(llFVec4Type)
            s1 = symbolTable.genUniqueSymbol(llFVec4Type)
            s2 = symbolTable.genUniqueSymbol(llFVec4Type)
            s3 = symbolTable.genUniqueSymbol(llFVec4Type)

            r0 = self.builder.insert_element(vf, elems[0] , i0, s0.name)
            r1 = self.builder.insert_element(r0, elems[1] , i1, s1.name)
            r2 = self.builder.insert_element(r1, elems[2] , i2, s2.name)
            r3 = self.builder.insert_element(r2, elems[3] , i3, s3.name)

            return r3
        
    def emitVSel(self, node):

        self.builder.call
        f3 = self.builder.call(func, [e3], ftmp3.name)
        
    def visitCallFunc(self, node):

        assert isinstance(node.node, compiler.ast.Name)

        print "; callfunc", node.args

        args = [self.visit(a) for a in node.args]

        print "; callfuncafter", args

        ty = typer.isNameOfFirstClassType(node.node.name)
        print "; callfuncafter: ty = ",ty

        #
        # value initialier? 
        #
        if ty:
            # int, float, vec, ...
            return self.handleInitializeTypeCall(ty, args)
                
        
        #
        # vector math function?
        # 
        ret = self.isVectorMathFunction(node.node.name)
        if ret is not False:
            return self.emitVMath(ret[1], args)

        #
        # Special function?
        #
        if (node.node.name == "vsel"):
            func = self.getExternalSymbolInstruction("vsel")
            tmp  = symbolTable.genUniqueSymbol(vec)

            print "; ", args
            c    = self.builder.call(func, args, tmp.name)

            return c
            
        #
        # Defined in the source?
        #
        ty      = typer.inferType(node.node)
        funcSig = symbolTable.lookup(node.node.name)

        if funcSig.kind is not "function":
            raise Exception("Symbol isn't registered as function:", node.node.name)

        # emit call 
        tmp  = symbolTable.genUniqueSymbol(vec)
        return self.builder.call(funcSig.llstorage, args, tmp.name)


    def visitList(self, node):

        return [self.visit(a) for a in node.nodes]

    #
    # Leaf
    #
    def visitName(self, node):

        sym = symbolTable.lookup(node.name) 

        tmpSym = symbolTable.genUniqueSymbol(sym.type)

        # %tmp = load %name

        loadInst = self.builder.load(sym.llstorage, tmpSym.name)

        print "; [Leaf] inst = ", loadInst
        return loadInst


    def visitDiscard(self, node):

        self.visit(node.expr)

        #
        # return None
        #


    def mkLLConstInst(self, ty, value):

        # ty = typer.inferType(node)
        # print "; [Typer] %s => %s" % (str(node), str(ty))

        llTy   = toLLVMTy(ty)
        bufSym = symbolTable.genUniqueSymbol(ty)
        tmpSym = symbolTable.genUniqueSymbol(ty)

        # %tmp  = alloca ty
        # store ty val, %tmp
        # %inst = load ty, %tmp

        allocInst = self.builder.alloca(llTy, bufSym.name)

        llConst   = None
        if llTy   == llIntType:
            llConst = llvm.core.Constant.int(llIntType, value)
    
        elif llTy == llFloatType:
            llConst = llvm.core.Constant.real(llFloatType, value)

        elif llTy == llFVec4Type:
            print ";", value
            raise Exception("muda")
    
        storeInst = self.builder.store(llConst, allocInst)
        loadInst  = self.builder.load(allocInst, tmpSym.name)

        print ";", loadInst

        return loadInst

    def visitConst(self, node):
        
        ty = typer.inferType(node)

        return self.mkLLConstInst(ty, node.value)

    def emitCommonHeader(self):

        s = """
define <4xfloat> @vsel(<4xfloat> %a, <4xfloat> %b, <4xi32> %mask) {
entry:
    %a.i     = bitcast <4xfloat> %a to <4xi32>
    %b.i     = bitcast <4xfloat> %b to <4xi32>
    %tmp0    = and <4xi32> %b.i, %mask
    %tmp.addr = alloca <4xi32>
    store <4xi32> <i32 -1, i32 -1, i32 -1, i32 -1>, <4xi32>* %tmp.addr
    %allone  = load <4xi32>* %tmp.addr
    %invmask = xor <4xi32> %allone, %mask
    %tmp1    = and <4xi32> %a.i, %invmask
    %tmp2    = or <4xi32> %tmp0, %tmp1
    %r       = bitcast <4xi32> %tmp2 to <4xfloat>

    ret <4xfloat> %r
}

"""
        return s

    #
    #
    #
    def emitExternalSymbols(self):

        d = {
              'fabsf'  : ( llFloatType, [llFloatType] )
            , 'expf'   : ( llFloatType, [llFloatType] )
            , 'logf'   : ( llFloatType, [llFloatType] )
            , 'sqrtf'  : ( llFloatType, [llFloatType] )
            , 'vsel'   : ( llFVec4Type, [llFVec4Type, llFVec4Type, llIVec4Type] )
            }

        for k, v in d.items():
            fty = llvm.core.Type.function(v[0], v[1])
            f   = llvm.core.Function.new(self.module, fty, k)

            self.externals[k] = f

    def getExternalSymbolInstruction(self, name):

        if self.externals.has_key(name):
            return self.externals[name]
        else:
            raise Exception("Unknown external symbol:", name, self.externals)

    def isExternalSymbol(self, name):
        if self.externals.has_key(name):
            return True
        else:
            return False

    #
    # Vector math
    #
    def isVectorMathFunction(self, name):
        d = {
              'vabs'  : 'fabsf'
            , 'vexp'  : 'expf'
            , 'vlog'  : 'logf'
            , 'vsqrt' : 'sqrtf'
            }

        if d.has_key(name):
            return (True, d[name])
        else:
            return False

    def emitVMath(self, fname, llargs):
        """
        TODO: Use MUDA's optimized vector math function for LLVM.
        """

        i0    = llvm.core.Constant.int(llIntType, 0)
        i1    = llvm.core.Constant.int(llIntType, 1)
        i2    = llvm.core.Constant.int(llIntType, 2)
        i3    = llvm.core.Constant.int(llIntType, 3)
        vzero = llvm.core.Constant.vector([llvm.core.Constant.real(llFloatType, "0.0")] * 4)

        func = self.getExternalSymbolInstruction(fname)

        # Decompose vector element
        tmp0  = symbolTable.genUniqueSymbol(float)
        tmp1  = symbolTable.genUniqueSymbol(float)
        tmp2  = symbolTable.genUniqueSymbol(float)
        tmp3  = symbolTable.genUniqueSymbol(float)
        e0    = self.builder.extract_element(llargs[0], i0, tmp0.name) 
        e1    = self.builder.extract_element(llargs[0], i1, tmp1.name) 
        e2    = self.builder.extract_element(llargs[0], i2, tmp2.name) 
        e3    = self.builder.extract_element(llargs[0], i3, tmp3.name) 

        ftmp0 = symbolTable.genUniqueSymbol(float)
        ftmp1 = symbolTable.genUniqueSymbol(float)
        ftmp2 = symbolTable.genUniqueSymbol(float)
        ftmp3 = symbolTable.genUniqueSymbol(float)
        f0 = self.builder.call(func, [e0], ftmp0.name)
        f1 = self.builder.call(func, [e1], ftmp1.name)
        f2 = self.builder.call(func, [e2], ftmp2.name)
        f3 = self.builder.call(func, [e3], ftmp3.name)

        # pack
        s0 = symbolTable.genUniqueSymbol(llFVec4Type)
        s1 = symbolTable.genUniqueSymbol(llFVec4Type)
        s2 = symbolTable.genUniqueSymbol(llFVec4Type)
        s3 = symbolTable.genUniqueSymbol(llFVec4Type)

        r0 = self.builder.insert_element(vzero, f0, i0, s0.name)
        r1 = self.builder.insert_element(r0   , f1, i1, s1.name)
        r2 = self.builder.insert_element(r1   , f2, i2, s2.name)
        r3 = self.builder.insert_element(r2   , f3, i3, s3.name)
        return r3

        # r0 = self.builder.insert_element(vzero, f2, i2, s0.name)
        # r1 = self.builder.insert_element(r0   , e1, i1, s1.name)
        # r2 = self.builder.insert_element(r1   , e2, i0, s2.name)
        # return r2

    def emitVAbs(self, llargs):

        return self.emitVMath("fabsf", llargs)
        

def _test():
    import doctest
    doctest.testmod()
    sys.exit()
    

def main():

    if len(sys.argv) < 2:
        _test()

    ast = compiler.parseFile(sys.argv[1])
    # print ast

    compiler.walk(ast, CodeGenLLVM())


if __name__ == '__main__':
    main()
