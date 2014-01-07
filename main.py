from MUDA import *
import simplebs

# import test

def mytest():
    s = vec([1.0, 2.0, 3.0, 4.0]) 
    x = vec([1.1, 2.1, 3.1, 4.1]) 
    t = vec([1.2, 2.2, 4.0, 4.2]) 
    r = vec([0.01, 0.02, 0.04, 0.05]) 
    v = vec([0.3, 0.2, 0.4, 0.3]) 

    # r = test.test(s)
    # print r

    for i in range(10000):
        r = simplebs.BlackScholes(s, x, t, r, v)
        # print r

    print r


if __name__ == '__main__':
    mytest()
