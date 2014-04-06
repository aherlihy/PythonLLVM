import timeit
import sys
from numba import jit

def run_test():
    from test_numba import main
    v = jit(main)
    v()

if __name__ == '__main__':
    run_test()
