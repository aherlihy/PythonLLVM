#import <stdio.h>
#import <math.h>

void kmeans(int i, int* data, int* x, int* y, int* c, int* t, int CENT) {
    
    int xd0 = c[0] - x[i];
    int xd1 = c[1] - x[i];
    int xd2 = c[2] - x[i];
    int yd0 = c[3] - y[i];
    int yd1 = c[4] - y[i];
    int yd2 = c[5] - y[i];
    int d0 = sqrt(xd0 * xd0 + yd0 * yd0);
    int d1 = sqrt(xd1 * xd1 + yd1 * yd1);
    int d2 = sqrt(xd2 * xd2 + yd2 * yd2);

    int min = d0;
    int assign = 0;
    if(d1<min){
        min=d1;
        assign=1;
    }
    if(d2<min)
        assign=2;
    t[assign]+=x[i];
    t[CENT+assign]+=y[i];
    t[2*CENT+assign]++;
}

int main() {
    int* data = new int[100];
    int* x = new int[100];
    int* y = new int[100];
    int* c = new int[100];
    int* t = new int[100];
    for(int i=0;i<100;i++) {
        data[i]=8;
        x[i]=i;
        y[i]=i;
        c[i]=i;
        t[i]=0;
    }
    kmeans(99, data, x, y, c, t, 25);
    /*
    printf("[");
    for(int i=0;i<100;i++) {
        printf("%i, ", t[i]);
    }
    printf("]\n");
    */
    delete[] data;
    delete[] x;
    delete[] y;
    delete[] c;
    delete[] t;
}
