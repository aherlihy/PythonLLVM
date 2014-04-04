import timeit
import sys
from numba import jit

def run_test():
    from test import main
    x = jit(main)
    x()

if __name__ == '__main__':
    print "ARGS=", sys.argv
    if len(sys.argv) != 2:
        print "Usage: <udf file>"
        sys.exit(0)
    
    time = timeit.timeit("run_test()", setup="from __main__ import run_test", number=100)
    
    print time
