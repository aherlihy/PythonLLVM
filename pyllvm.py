#!/usr/bin/env python
import os, sys

from CodeGenLLVM import *

def usage():
    print "Usage: pyllvm.py <input.py>"
    sys.exit(1)

def compiler(filename):
    return pyllvm(filename)

def main():

    if len(sys.argv) < 2:
        usage()
    print compiler(sys.argv[1])

if __name__ == '__main__':
    main()
