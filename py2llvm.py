#!/usr/bin/env python

import os, sys

from CodeGenLLVM import *

def usage():
    print "Usage: py2llvm.py <input.py>"
    sys.exit(1)

def main():

    if len(sys.argv) < 2:
        usage()

    ast = compiler.parseFile(sys.argv[1])
    print "-------AST--------"
    print ast
    print "-----end AST------"
    compiler.walk(ast, CodeGenLLVM())


if __name__ == '__main__':
    main()
