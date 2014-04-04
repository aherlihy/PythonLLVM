def naive_bayes(data=listi8, counts=listi4,dims=int, vals=int, labels=int):
    label = data[dims]
    counts[label] = counts[label] + 1
    offset = labels + label * dims * vals
    c = 0
    while(c<dims):
        counts[offset+c*vals+data[c]]=counts[offset+c*vals+data[c]]+1
        c=c+1

def main():
    data=[1,2,3,4,5,6,7,8]
    counts = [0,0,0,0]
    dims=4
    vals=10
    labels=1
    naive_bayes(data, counts, dims, vals, labels)
