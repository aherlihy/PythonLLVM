# Original C++
#void naive_bayes(char *data, int *counts, int dims, int vals, int labels){
#   char label = data[dims];
#    ++counts[label];
#    int offset = labels + label * dims * vals;
#    for (int j = 0; j < dims; j++)
#        ++counts[offset + j * vals + data[j]];
#}
import numba
listi100=[]
@numba.jit
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
    #data = [7 for x in range(100)]
    #counts = [0 for x in range(100)]
    data = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99]
    counts=[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    dims = 99
    vals = 1
    labels = 1
    naive_bayes(data, counts, dims, vals, labels)
