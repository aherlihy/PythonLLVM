import timeit
import sys
from numba import jit

def run_test():
    from np2_kmeans import run
    # if running tests from set1, then generate data here. Otherwise just call v with no arguments.
    i=99
    CENT=1
    data=[a for a in range(100)]
    x=[b for b in range(100)]
    y=[d for d in range(100)]
    c=[e for e in range(100)]
    t=[f for f in range(100)]
    v = jit(run)
    v(i, data, x, y, c, t, CENT)

if __name__ == '__main__':
    print "RUNNING" 
    time = timeit.timeit("run_test()", setup="from __main__ import run_test", number=5)
    
    print time
