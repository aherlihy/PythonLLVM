#import <stdio.h>
void naive_bayes(int* data, int *counts, int dims, int vals, int labels){
    char label = data[dims];
    ++counts[label];
    int offset = labels + label * dims * vals;
    for (int j = 0; j <= dims; j++) {
        ++counts[(offset + j * vals + data[j])%100];
    }
}
int main() {
    int* data = new int[100];
    int* counts = new int[100];
    for(int i=0;i<100;i++) {
        data[i]=i;
        counts[i]=0;
    }
    
    /*
    printf("[");
    for(int i=0;i<100;i++) {
        printf("%i, ", counts[i]);
    }
    printf("]\n");
    */
    naive_bayes(data, counts, 99, 1, 1);
    delete[] data;
    delete[] counts;
    return 0; 

}
