from math import *
from MUDA import *

def cnd(d = vec):

    A1       = vec(0.31938153)
    A2       = vec(-0.356563782)
    A3       = vec(1.781477937)
    A4       = vec(-1.821255978)
    A5       = vec(1.330274429)
    RSQRT2PI = vec(0.3989422804)

    K        = vec(1.0) / (vec(1.0) + vec(0.2316419) * vabs(d))

    cnd = RSQRT2PI * vexp(vec(-0.5) * d * d) * (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))))

    one_minus_cnd = vec(1.0) - cnd
	
    # return one_minus_cnd
    return vsel(cnd, one_minus_cnd, d > vec(0.0))


def BlackScholes(S = vec, X = vec, T = vec, R = vec, V = vec):

    sqrtT  = vsqrt(S)

    d1     = (vlog(S / X) + (R + vec(0.5) * V * V) *T) / (V * sqrtT)
    d2     = d1 - V * sqrtT

    cnd_d1 = cnd(d1)
    cnd_d2 = cnd(d2)

    expRT  = vexp(vec(-1.0) * R * T)
    
    # Only calculates value of call option.
    retCall = S * cnd_d1 - X * expRT * cnd_d2

    return retCall
