from math import exp
listf100=[]
def run(data=listf100, g=listf100, w=listf100, dims=int): 
    dot = 0.0
    j = 0
    while(j<dims):
        dot += data[j]*w[j]
        j+=1
    label = data[dims]
    scale = (1.0 / (1.0+ exp(-label*dot)) - 1.0) * label
    j2 = 0
    while(j2<dims):
        g[j2]+=scale*data[j2]
        j2+=1

