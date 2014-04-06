# Original C++
#void naive_bayes(char *data, int *counts, int dims, int vals, int labels){
#   char label = data[dims];
#    ++counts[label];
#    int offset = labels + label * dims * vals;
#    for (int j = 0; j < dims; j++)
#        ++counts[offset + j * vals + data[j]];
#}

listi100=[]
def naive_bayes(data=listi100, counts=listi100,dims=int, vals=int, labels=int):
    label = data[dims]
    counts[label]  += 1
    offset = labels + label * dims * vals
    c = 0
    while(c<dims):
        #mod w 100 bc using fake data and don't want out of bounds errors
        counts[(offset+c*vals+data[c])%100]+=1
        c=c+1

def main():
    data = zeros(100, 7)
    counts = zeros(100)
    dims = 100
    vals = 1
    labels = 1
    naive_bayes(data, counts, dims, vals, labels)
