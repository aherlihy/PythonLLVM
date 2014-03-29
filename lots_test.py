from MUDA import *

# FUNCTION ARGS+RET

#   INT
def iret(i=int):
    return i+1
#   FLOAT
def fret(f=float):
    return 1.0000+f
#   VEC
def vret(v=vec):
    return v
#   LIST<int>
def lretv5(lv5=listf5):
    return lv5
#   LIST<float>
def lreti8(li8=listi8):
    return li8
#   STR
def sret(s=listi5):
    return s
#   SUB
def lsubi(isub=int, lsi=listf5):
    return lsi[isub]
def lsubf(isubf=int, lsf=listi8):
    return lsf[isubf]+isubf
#   FUNC
def ffret():
    return fret(1.00)
def firet():
    return iret(1)
def fvret():
    return vret(vec([4.0,2.0,2.4,1.0]))
def flret():
    return lretv5([1.0,2.0,3.0,4.0,5.0])
def f2lret():
    return [1.0,2.0]
def fliret():
    return [9,99]
def fsret():
    return sret('hello')
#   ALL
def ifvret(i=int, v=vec, f=float, l=listf2, s=listi5):
    print f
    print v
    print i
    print l
    print s
    return i+i
# IF/WHILE/CMP
    #AND/OR/NOT
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
# NOTE: can't have return type in only one branch
def test_if1(t=int):
    if(not t==0):
        return 0
    return -1
def test_if2(t=int):
    y = 0
    if(t==0):
        y = 10
    if((t-1)==0):
        y = 12
    return y
def test_if3(t=int):
    if(t==0):
        return 5
    return 7
def test_if4(t=int):
    if(t==1):
        return 5
    else:
        return 13
    return 8
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

def test_while2():
    test_while1 = 10
    while(test_while1>0):
        test_while1 = test_while1-1
        if(test_while1==5):
            return 100
    return test_while1#ret100

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
    li1 = [1,2,3]
    lf1 = [1.4,5.4]
    f5 = [1.0,2.0,3.0,4.0,5.0]
    i8 = [1,2,3,4,5,6,7,8]
    rs = 'hello'
    print( ri, rf, rv, v1, li1, lf1, f5, i8, rs )
# LIST CONSTR and SUB
    ll1 = [ri, ri+1, ri+2] #[5,6,7]
    ll2 = [li1[0], li1[2]] #[1,3]
    ll3 = [[1,2,3][0], ll2[0], fliret()[0]] #[1,1,9]
    ll4 = [fliret()[0], fliret()[1]] #[9,99]
    ll5 = [fret(3.0), fret(6.0)] #[4.0,7.0]
    print( ll1, ll2, ll3, ll4, ll5)

#TODO: empty lists
# FUNC ARGS+RET
    xf5 = f5
    zf5 = lretv5(xf5)
    xi8 = i8
    zi8 = lreti8(i8)                
    zlsi = lsubi(4, f5)
    zlsf = lsubf(2, xi8)
    # { 6, 6.0, vec:[1.0,2.0,3.0,4.0], vec:[1.0,1.0,1.0,1.0], 'hello' }
    print( iret(ri), fret(rf), vret(rv), vret(v1), sret(rs) )
    # { [1.0,2.0,3.0,4.0,5.0], [1.0,2.0,3.0,4.0,5.0], [1,2,3,4,5,6,7,8], 5.0, 5 }
    print( xf5, zf5, zi8, zlsi, zlsf )
    # { 2.0, 2, vec:[4.0,2.0,2.4,1.0], [1.0,2.0,4.0,5.0], 'hello' }
    print( ffret(), firet(), fvret(), flret(), fsret() )


    # PRINTING
    # { 5 // 5.0 // vec:[1.0,2.0,3.0,4.0] // [1.4,5.4] // 'hello' }
    ifvret( ri, rv, rf, lf1, rs )
    # { 2, [4.0,2.0,4.2,2.0], 2.0, [1.0,2.0], 'hello' }
    ifvret( firet(), fvret(), ffret(), f2lret(), fsret() )
# IF/WHILE/CMP
    ift = test_if()
    ift1 = test_if1(0)
    ift2 = test_if2(0)
    ift3 = test_if3(0)
    ift4 = test_if4(0)
    ift1x = test_if1(1)
    ift2x = test_if2(1)
    ift3x = test_if3(1)
    ift4x = test_if4(1)
    # { 11, -1, 10, 5, 13, 0, 12, 7, 5 }
    print( ift, ift1, ift2, ift3, ift4, ift1x, ift2x, ift3x, ift4x )
    whilet = test_while()
    whilet2 = test_while2()
    comparet = test_compare()
    # { 0, 100, 1.0 }
    print( whilet, whilet2, comparet )

# ARRAYS
    # INT
    # test returning types of lists
    # { [9,7,4], [6,7,8], [9,7,4] }
    print( test_ret1i(), test_ret2i(), test_ret3i() )
    # test passing lists as args
    ilst = [10,20,30]
    #PRINTING
    #{[90,100,110]}
    test_ret4i([90,100,110])
    #{ [10,20,30] }
    test_ret4i(ilst)
    #{ [9,7,4] }
    test_ret4i(test_ret3i())
    # test returning lists passed as args
    # { [90,100,110], [10,20,30], [9,7,4] }
    print( test_ret5i([90,100,110]), test_ret5i(ilst), test_ret5i(test_ret3i()) )
    # FLOAT
    # test returning types of lists
    # { [9.1,7.1,4.1], [6.1,7.1,8.1], [9.1,7.1,4.1] }
    print( test_ret1f(), test_ret2f(), test_ret3f() )
    # test passing lists as args
    flst = [10.1,20.1,30.1]
    #{ [90.1, 100.1, 110.1] }
    test_ret4f([90.1,100.1,110.1])
    #{ [10.1,20.1,30.1] }
    test_ret4f(flst)
    #{ [9.1, 7.1, 4.1] }
    test_ret4f(test_ret3f())
    # test returning lists passed as args
    # { [90.1,100.1,110.1], [10.1,20.1,30.1], [9.1,7.1,4.1]
    print( test_ret5f([90.1,100.1,110.1]), test_ret5f(flst), test_ret5f(test_ret3f()) )

#LEN
    ilen = [1,2,3]
    flen = [5.3,6.5,3.5,6.3]
    slen = 'hello there'
    # { 3, 4, 11 }
    print( len(ilen), len(flen), len(slen) )
#TODO: getAttr on Vec
