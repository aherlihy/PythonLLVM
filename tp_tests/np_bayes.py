listi100=[]
def naive_bayes(data=listi100, counts=listi100,dims=int, vals=int, labels=int):
    label = data[dims]
    counts[label]  += 1
    offset = labels + label * vals
    c = 0
    while(c<dims):
        counts[c]+=1
        c=c+1
def run():
    x = 1
    data = range(100)
    data[99]=1
    counts = range(100)
    dims = 99
    vals = 1
    labels = 1
    naive_bayes(data, counts, dims, vals, labels)
