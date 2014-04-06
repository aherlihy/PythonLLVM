
#import <stdio.h>
#import <math.h>

void logreg(float* data, float* g, float* w, int DIMS) {
    float dot = 0;
    for (int j = 0; j < DIMS; j++)
        dot += data[j] * w[j];
    float label = data[DIMS];
    float scale = (1 / (1 + exp(-label * dot)) - 1) * label;
    for (int j = 0; j < DIMS; j++)
        g[j] += scale * data[j];
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
    data[99]=-1.0;
    logreg(data, g, w, 99);
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
