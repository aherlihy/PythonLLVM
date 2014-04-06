def naive_bayes(data=listi100, counts=listi100,dims=int, vals=int, labels=int):
    label = data[dims]
    counts[label] = counts[label] + 1
    offset = labels + label * dims * vals
    c = 0
    while(c<dims):
        counts[offset+c*vals+data[c]]=counts[offset+c*vals+data[c]]+1
        c=c+1
def main():
    data = range(100)
    counts = range(100)
    dims = 100
    vals = 1
    labels = 1
    naive_bayes(data, counts, dims, vals, labels)
