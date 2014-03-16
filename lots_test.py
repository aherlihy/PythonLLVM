from MUDA import *

# test visitReturn and visitFunction
def iret(i=int):
    return i+1
def fret(f=float):
    return 1.0000+f
def vret(v=vec):
    return v
def ifvret(i=int, v=vec, f=float):
    return i+i
def nret():
    nret1= 5+5
    nret2 = nret1+5
def test_if():
    test_if1 = 4
    if(4==test_if1 and 5==5):
        test_if2 = -1
        test_if1 = 9
    if(test_if1!=8 or 5==6):
        test_if2 = -3
        test_if1 = 11
    else:
        test_if2 = -4
        test_if1 = 0
    return test_if1
def test_compare():
    a = (1==1)
    b = (1.00==1.00)
    c = (1>=0)
    d = (1.00>=0.00)
    e = (0<=1)
    fcp = (0.00 <= 1.00)
    g = (4!=6)
    h = (4.00!=6.00)
    z = a+b+c+d+e+fcp+g+h
    return z-8.00
def test_while():
    test_while1 = 10
    while(test_while1>0):
        test_while1 = test_while1-1
    return test_while1
# test visitStmt
def main():
    # test visitAssign
    ri = 5
    rf = 5.00
    rv = vec([1.00,2.00,3.00,4.00])
    v1 = vec(1.00)
    # test visitFunctionCall
    print iret(ri)
    print fret(rf)
    print vret(rv)
    print vret(v1)
    # test visitIf, visitWhile, and visitCompare
    ift = test_if()
    whilet = test_while()
    comparet = test_compare()

    print(ift, whilet, comparet)
