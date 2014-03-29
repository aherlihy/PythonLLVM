from MUDA import *

# test visitReturn and visitFunction
def iret(i=int):
    return i+1
def fret(f=float):
    return 1.0000+f
def vret(v=vec):
    print v
    return v
def ifvret(i=int, v=vec, f=float):
    print f
    print v
    return i+i
def lretv5(lv5=listf5):
    return lv5
def lreti8(li8=listi8):
    print li8
    return li8
def lsubi(isub=int, lsi=listf5):
    return lsi[isub]
def lsubf(isubf=int, lsf=listi8):
    return lsf[isubf]+isubf
def nret():
    nret1= 5+5
    nret2 = nret1+5
def test_if():
    test_if1 = 4
    if(4==test_if1 and 5==5):
        test_if2 = -1
        test_if1 = 9
    if(test_if1==9 or 5==6):
        test_if2 = -3
        test_if1 = 11
    else:
        test_if2 = -4
        test_if1 = 0
    return test_if1#ret 11
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
    return z-7.00#ret 1.00
def test_while():
    test_while1 = 10
    while(test_while1>0):
        test_while1 = test_while1-1
    return test_while1#ret0
# test visitStmt
def test_ret1i():
    x = [9,7,4]   
    return x
def test_ret2i():
    return [6,7,8]
def test_ret3i():
    return test_ret1i()
def test_ret4i(x=listi3):
    print x
def test_ret5i(x=listi3):
    return x
def test_ret1f():
    x = [9.1,7.1,4.1]   
    return x
def test_ret2f():
    return [6.1,7.1,8.1]
def test_ret3f():
    return test_ret1f()
def test_ret4f(x=listf3):
    print x
def test_ret5f(x=listf3):
    return x


def main():
# ASSIGN
    ri = 5
    rf = 5.00
    rv = vec([1.00,2.00,3.00,4.00])
    v1 = vec(1.00)
# VEC
    print iret(ri)                  #6
    print fret(rf)                  #6.00000
    print vret(rv)                  #vec[1.00,2.00,3.00,4.00] 
                                    #vec[1.00,2.00,3.00,4.00]
    print vret(v1)                  #vec[1.00,1.00,1.00,1.00]
                                    #vec[1.00,1.00,1.00,1,00]
    print ifvret(ri, rv, rf)        #5.00
                                    #vec[1.00,2.00,3.00,4.00]
                                    #10
# IF/WHILE/CMP
    ift = test_if()
    whilet = test_while()
    comparet = test_compare()
   
    print(ift, whilet, comparet)    #11, 0, 1.00
# ARRAYS + SUB
    f5 = [1.0,2.0,3.0,4.0,5.0]
    xf5 = f5
    zf5 = lretv5(xf5)               
    print f5                        #[1.0,2.0,3.0,4.0,5.0]
    print xf5                       #[1.0,2.0,3.0,4.0,5.0]
    print zf5                       #[1.0,2.0,3.0,4.0,5.0]
    i8 = [1,2,3,4,5,6,7,8]
    xi8 = i8
    zi8 = lreti8(i8)                #[1,2,3,4,5,6,7,8]
    print i8                        #[1,2,3,4,5,6,7,8]
    print zi8                       #[1,2,3,4,5,6,7,8]
    zlsi = lsubi(4, f5)
    zlsf = lsubf(2, xi8)
    print zlsi                      #5.0
    print zlsf                      #5

# ARRAYS
    # INT
    # test returning types of lists
    print test_ret1i()
    print test_ret2i()
    print test_ret3i()
    # test passing lists as args
    ilst = [10,20,30]
    test_ret4i([90,100,110])
    test_ret4i(ilst)
    test_ret4i(test_ret3i())
    # test returning lists passed as args
    print test_ret5i([90,100,110])
    print test_ret5i(ilst)
    print test_ret5i(test_ret3i())
    # FLOAT
    # test returning types of lists
    print test_ret1f()
    print test_ret2f()
    print test_ret3f()
    # test passing lists as args
    flst = [10.1,20.1,30.1]
    test_ret4f([90.1,100.1,110.1])
    test_ret4f(flst)
    test_ret4f(test_ret3f())
    # test returning lists passed as args
    print test_ret5f([90.1,100.1,110.1])
    print test_ret5f(flst)
    print test_ret5f(test_ret3f())

#LEN
    ilen = [1,2,3]
    print(len(ilen))
    flen = [5.3,6.5,3.5,6.3]
    print(len(flen))
    slen = 'hello there'
    print(len(slen))

