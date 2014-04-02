#!/usr/bin/env python

import os, sys

from CodeGenLLVM import *

def usage():
    print "Usage: py2llvm.py <input.py>"
    sys.exit(1)

def compiler(filename):
    return py2llvm(filename)

def main():

    if len(sys.argv) < 2:
        usage()
    print py2llvm(sys.argv[1])

if __name__ == '__main__':
    main()
