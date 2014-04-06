#import <stdio.h>
#import <math.h>

void linreg(float* data, float* w, float* g, int DIMS) {
    float dot = 0;
    for (int j = 0; j <= DIMS; j++)
        dot +=data[j] * w[j];
    float label = data[DIMS];
    dot *= -label;
    for (int j = 0; j <= DIMS; j++)
        g[j] += dot * data[j];
}

int main() {
    float* data = new float[100];
    float* w = new float[100];
    float* g = new float[100];

    for(int i=0;i<100;i++) {
        data[i]=i;
        w[i]=i;
        g[i]=0.0;
    }
    data[99]=1.0;
    linreg(data, w, g, 99);
    /*
    printf("[");
    for(int i=0;i<100;i++) {
        printf("%f, ", g[i]);
    }
    printf("]\n");
    */
    delete[] data;
    delete[] w;
    delete[] g;
}
