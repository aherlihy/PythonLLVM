from pyllvm import compiler
import timeit
import sys

def run_test():
    compiler(sys.argv[1])

if __name__ == '__main__':
    print "ARGS=", sys.argv
    if len(sys.argv) != 2:
        print "Usage: <udf file>"
        sys.exit(0)
    
    time = timeit.timeit("run_test()", setup="from __main__ import run_test", number=500)
    
    print time
