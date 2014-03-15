#!/usr/bin/env python

import os, sys
import re
import compiler

import llvm.core
# original=commented
from VecTypes import *
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

# converts from python to LLVM type
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
        print ";----" + sys._getframe().f_code.co_name + "----"

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
        print ";----" + sys._getframe().f_code.co_name + "----"

        # emitExternalSymbols() should be called before self.visit(node.node)
        self.emitExternalSymbols()

        self.visit(node.node)
        #print self.module   # Output LLVM code to stdout.
        #print self.emitCommonHeader()

    def getString(self):
        return self.module

    def visitPrint(self, node):
        return None # Discard


    def visitPrintnl(self, node):
        return None # Discard


    def visitReturn(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        ty   = typer.inferType(node.value)
        #print "; Return ty = ", ty

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
        print ";----" + sys._getframe().f_code.co_name + "----"

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
        print ";----" + sys._getframe().f_code.co_name + "----"

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
        print ";----" + sys._getframe().f_code.co_name + "----"
        for node in node.nodes:

            self.visit(node)

    def visitAssign(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        if len(node.nodes) != 1:
            raise Exception("TODO:", node)

        #print "; [Asgn]"
        rTy     = typer.inferType(node.expr)
        #print "; [Asgn]. rTy = ", rTy
        
        print ";RHS [Asgn]. node.expr = ", node.expr
        rLLInst = self.visit(node.expr)
        #print "; [Asgn]. rhs = ", rLLInst
        
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
                #print "; [Sym] New symbol added: ", sym

                lTy = rTy

            else:
                # symbol is already defined.
                lTy = sym.type


        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s: %s" % (lTy, rTy, node))

        lSym = symbolTable.find(lhsNode.name)

        storeInst = self.builder.store(rLLInst, lSym.llstorage)
        #print ";", storeInst

        #print "; [Asgn]", node
        #print "; [Asgn] nodes = ", node.nodes
        #print "; [Asgn] expr  = ", node.expr
        # No return

    def visitIf(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"
        is_else = (node.else_ is not None)
        cond = self.visit(node.tests[0][0])
        # TODO: cast return value from cond to truth value
        condition_bool = self.builder.fcmp(llvm.core.FCMP_ONE, cond, llvm.core.Constant.real(llvm.core.Type.float(), 0), 'ifcond')
        # get function
        function = self.builder.basic_block.function
        
        # create blocks
        then_block = function.append_basic_block('if_then')
        if(is_else):
            else_block = function.append_basic_block('if_else')
        merge_block = function.append_basic_block('if_merge')
        if(is_else):
            self.builder.cbranch(condition_bool, then_block, else_block) 
        else:
            self.builder.cbranch(condition_bool, then_block, merge_block) 
            
        # emit then
        self.builder.position_at_end(then_block)
        symbolTable.pushScope("if_")
        then_val = self.visit(node.tests[0][1])
        symbolTable.popScope() 
        self.builder.branch(merge_block)
        # update then for phi 
        then_block = self.builder.basic_block
        if(is_else):
            # emit else
            self.builder.position_at_end(else_block)
            symbolTable.pushScope("else_")
            else_val = self.visit(node.else_)
            symbolTable.popScope()
            self.builder.branch(merge_block)
            else_block = self.builder.basic_block

        # emit merge
        self.builder.position_at_end(merge_block)
        #TODO: insert dummy instruction
        #phi = self.builder.phi(llvm.core.Type.double(), 'iftmp')
        #phi.add_incoming(then_val, then_block)
        #phi.add_incoming(else_val, else_block)

        #return phi
    def visitFor(self, node):
        print "VISITED FOR"
        print node

    def visitWhile(self, node):
        print ";VISITED WHILE"
        print ";----" + sys._getframe().f_code.co_name + "----"
        # get function
        function = self.builder.basic_block.function
        
        # create blocks
        start_while = function.append_basic_block('start_while')
        do_while = function.append_basic_block('do_while')
        end_while = function.append_basic_block('end_while')

        self.builder.branch(start_while)
        self.builder.position_at_end(start_while)

        cond = self.visit(node.test)
        # TODO: cast return value from cond to truth value
        condition_bool = self.builder.fcmp(llvm.core.FCMP_ONE, cond, llvm.core.Constant.real(llvm.core.Type.float(), 0), 'whilecond')

        self.builder.cbranch(condition_bool, do_while, end_while) 
        
        self.builder.position_at_end(do_while)
        while_body = self.visit(node.body)
        self.builder.branch(start_while)
        self.builder.position_at_end(end_while)
        #TODO: handle while-else?
    
    # emit vector comparisons (TODO?)
    def emitVCompare(self, op, lInst, rInst):
        print ";----" + sys._getframe().f_code.co_name + "----"

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
        print ";----" + sys._getframe().f_code.co_name + "----"

        #print "; ", node.expr
        #print "; ", node.ops[0]

        lTy = typer.inferType(node.expr)
        rTy = typer.inferType(node.ops[0][1])

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.expr)
        rLLInst = self.visit(node.ops[0][1])

        op  = node.ops[0][0]




        if rTy == vec:
            return self.emitVCompare(op, lLLInst, rLLInst)
        elif rTy == int:
            d = { "==" : llvm.core.ICMP_EQ
                , "!=" : llvm.core.ICMP_NE
                , ">"  : llvm.core.ICMP_UGT
                , ">=" : llvm.core.ICMP_UGE
                , "<"  : llvm.core.ICMP_ULT
                , "<=" : llvm.core.ICMP_ULE
                }
            result = self.builder.icmp(d[op], lLLInst, rLLInst, 'cmptmp')
            return self.builder.uitofp(result, llvm.core.Type.float(), 'booltmp')
        elif rTy == float:
            d = { "==" : llvm.core.FCMP_OEQ
                , "!=" : llvm.core.FCMP_ONE
                , ">"  : llvm.core.FCMP_OGT
                , ">=" : llvm.core.FCMP_OGE
                , "<"  : llvm.core.FCMP_OLT
                , "<=" : llvm.core.FCMP_OLE
                }
            result = self.builder.fcmp(d[op], lLLInst, rLLInst, 'cmptmp')
            return self.builder.uitofp(result, llvm.core.Type.float(), 'flttmp')
        else:  
            raise Exception("unable to compare type " + rTy)

        #if op == "<":
            #print "muda"
        #elif op == ">":
            #print "muda"
        #else:
        

        if (op !="<") and (op != ">"):
            raise Exception("Unknown operator:", op)

        raise Exception("muda")

    def visitUnarySub(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        ty       = typer.inferType(node.expr)
        e        = self.visit(node.expr)
        zeroInst = llvm.core.Constant.null(toLLVMTy(ty))
        tmpSym   = symbolTable.genUniqueSymbol(ty)

        subInst = self.builder.sub(zeroInst, e, tmpSym.name)

        return subInst

    def visitGetattr(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        d = { 'x' : llvm.core.Constant.int(llIntType, 0)
            , 'y' : llvm.core.Constant.int(llIntType, 1)
            , 'z' : llvm.core.Constant.int(llIntType, 2)
            , 'w' : llvm.core.Constant.int(llIntType, 3)
            }


        ty = typer.inferType(node)
        #print "; getattr: expr", node.expr
        #print "; getattr: attrname", node.attrname
        #print "; getattr: ty", ty

        rLLInst  = self.visit(node.expr)
        tmpSym   = symbolTable.genUniqueSymbol(ty)

        if len(node.attrname) == 1:
            # emit extract element
            s = node.attrname[0]

            inst = self.builder.extract_element(rLLInst, d[s], tmpSym.name)

        return inst



    def visitAdd(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        lTy = typer.inferType(node.left)
        rTy = typer.inferType(node.right)

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.left)
        rLLInst = self.visit(node.right)

        tmpSym = symbolTable.genUniqueSymbol(lTy)

        addInst = self.builder.add(lLLInst, rLLInst, tmpSym.name)
        #print "; [AddOp] inst = ", addInst

        return addInst

    def visitSub(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        lTy = typer.inferType(node.left)
        rTy = typer.inferType(node.right)

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.left)
        rLLInst = self.visit(node.right)

        tmpSym = symbolTable.genUniqueSymbol(lTy)

        subInst = self.builder.sub(lLLInst, rLLInst, tmpSym.name)
        #print "; [SubOp] inst = ", subInst

        return subInst


    def visitMul(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        lTy = typer.inferType(node.left)
        rTy = typer.inferType(node.right)

        if rTy != lTy:
            raise Exception("ERR: TypeMismatch: lTy = %s, rTy = %s for %s, line %d" % (lTy, rTy, node, node.lineno))

        lLLInst = self.visit(node.left)
        rLLInst = self.visit(node.right)

        tmpSym = symbolTable.genUniqueSymbol(lTy)

        mulInst = self.builder.mul(lLLInst, rLLInst, tmpSym.name)
        #print "; [MulOp] inst = ", mulInst

        return mulInst


    def visitDiv(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

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

        #print "; [DIvOp] inst = ", divInst

        return divInst


    def handleInitializeTypeCall(self, ty, args):
        print ";----" + sys._getframe().f_code.co_name + "----"

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
        print ";----" + sys._getframe().f_code.co_name + "----"

        self.builder.call
        f3 = self.builder.call(func, [e3], ftmp3.name)

    def visitCallFunc(self, node):
        print ";----" + sys._getframe().f_code.co_name + ": " + node.node.name + "----"

        assert isinstance(node.node, compiler.ast.Name)

        #print "; callfunc", node.args

        args = [self.visit(a) for a in node.args]
        
        #print "; callfuncafter", args

        ty = typer.isNameOfFirstClassType(node.node.name)
        #print "; callfuncafter: ty = ",ty

        #
        # value initialier?
        #
        if ty:
            # int, float, vec, ...
            return self.handleInitializeTypeCall(ty, args)


        #
        # vector math function?
        #
        # UNCOMMENT:
        #orginal=comment start
        ret = self.isVectorMathFunction(node.node.name)
        if ret is not False:
            return self.emitVMath(ret[1], args)

        #
        # Special function?
        #
        if (node.node.name == "vsel"):
            func = self.getExternalSymbolInstruction("vsel")
            tmp  = symbolTable.genUniqueSymbol(vec)

            #print "; ", args
            c    = self.builder.call(func, args, tmp.name)

            return c
        #original=commented end
        #
        # Defined in the source?
        #
        ty      = typer.inferType(node.node)
        funcSig = symbolTable.lookup(node.node.name)

        if funcSig.kind is not "function":
            raise Exception("Symbol isn't registered as function:", node.node.name)

        # emit call
        tmp  = symbolTable.genUniqueSymbol(vec)
        print "args, name"
        print args
        print tmp.name
        return self.builder.call(funcSig.llstorage, args, tmp.name)


    def visitList(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        return [self.visit(a) for a in node.nodes]

    #
    # Leaf
    #
    def visitName(self, node):
        print ";----" + sys._getframe().f_code.co_name + " : " + node.name + "----"
   
        sym = symbolTable.lookup(node.name)

        tmpSym = symbolTable.genUniqueSymbol(sym.type)

        # %tmp = load %name

        loadInst = self.builder.load(sym.llstorage, tmpSym.name)

        #print "; [Leaf] inst = ", loadInst
        return loadInst


    def visitDiscard(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        self.visit(node.expr)
        #
        # return None
        #


    def mkLLConstInst(self, ty, value):
        print ";----" + sys._getframe().f_code.co_name + "----"

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

        #print ";", loadInst

        return loadInst

    def visitConst(self, node):
        print ";----" + sys._getframe().f_code.co_name + "----"

        ty = typer.inferType(node)

        return self.mkLLConstInst(ty, node.value)

    def emitCommonHeader(self):
        print ";----" + sys._getframe().f_code.co_name + "----"

        s = """ 
define <4 x float> @vsel(<4 x float> %a, <4 x float> %b, <4 x i32> %mask) {
entry:
    %a.i     = bitcast <4 x float> %a to <4 x i32>
    %b.i     = bitcast <4 x float> %b to <4 x i32>
    %tmp0    = and <4 x i32> %b.i, %mask
    %tmp.addr = alloca <4 x i32>
    store <4 x i32> <i32 -1, i32 -1, i32 -1, i32 -1>, <4 x i32>* %tmp.addr
    %allone  = load <4 x i32>* %tmp.addr
    %invmask = xor <4 x i32> %allone, %mask
    %tmp1    = and <4 x i32> %a.i, %invmask
    %tmp2    = or <4 x i32> %tmp0, %tmp1
    %r       = bitcast <4 x i32> %tmp2 to <4 x float>

    ret <4 x float> %r
}
@.str = private unnamed_addr constant [3 x i8] c"%i\00", align 1
declare i32 @printf(i8*, ...) #0
;%0 = load i32* %x, align 4        ; set %0 to x
;%call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32 %0)
        
        """
        return s

    #
    #
    # THIS IS WHERE HEADER DEFS LIVE
    def emitExternalSymbols(self):
        print ";----" + sys._getframe().f_code.co_name + "----"

        d = {
              'fabsf'  : ( llFloatType, [llFloatType] )
            , 'expf'   : ( llFloatType, [llFloatType] )
            , 'logf'   : ( llFloatType, [llFloatType] )
            , 'sqrtf'  : ( llFloatType, [llFloatType] )
            #, 'vsel'   : ( llFVec4Type, [llFVec4Type, llFVec4Type, llIVec4Type] )
            # FOR SOME REASON, REDEFINITION OF vsel BREAKS LLI, TODO:FIX
            }

        for k, v in d.items():
            fty = llvm.core.Type.function(v[0], v[1])
            f   = llvm.core.Function.new(self.module, fty, k)

            self.externals[k] = f
        print "; SYMBOL TABLE " + str(symbolTable)

    def getExternalSymbolInstruction(self, name):
        print ";----" + sys._getframe().f_code.co_name + "----"

        if self.externals.has_key(name):
            return self.externals[name]
        else:
            raise Exception("Unknown external symbol:", name, self.externals)

    def isExternalSymbol(self, name):
        print ";----" + sys._getframe().f_code.co_name + "----"
        if self.externals.has_key(name):
            return True
        else:
            return False

    #
    # Vector math
    #
    def isVectorMathFunction(self, name):
        print ";----" + sys._getframe().f_code.co_name + "----"
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
        print ";----" + sys._getframe().f_code.co_name + "----"
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
        print ";----" + sys._getframe().f_code.co_name + "----"

        return self.emitVMath("fabsf", llargs)


def _test():
    import doctest
    doctest.testmod()
    sys.exit()

def py2llvm(filename):
    ast = compiler.parseFile(filename)
    print ";" +  str(ast)
    codegen = compiler.walk(ast, CodeGenLLVM())

    main = codegen.getString()
    header = codegen.emitCommonHeader()

    return str(main) + "\n" + str(header)

def main():

    if len(sys.argv) < 2:
        _test()

    ast = compiler.parseFile(sys.argv[1])
    #print ast

    compiler.walk(ast, CodeGenLLVM())


if __name__ == '__main__':
    main()
