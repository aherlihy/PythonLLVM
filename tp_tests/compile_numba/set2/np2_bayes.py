listi100=[]
def run(data=listi100, counts=listi100,dims=int, vals=int, labels=int):
    label = data[dims]
    counts[label]  += 1
    offset = labels + label * vals
    c = 0
    while(c<dims):
        counts[c]+=1
        c=c+1
