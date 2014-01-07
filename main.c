#include <stdio.h>
#include <xmmintrin.h>


extern __m128 test(__m128 *a);
extern __m128 BlackScholes(__m128 S, __m128 X, __m128 T, __m128 R, __m128 V);

void
mytest()
{
    __m128 s;
    __m128 x;
    __m128 t;
    __m128 r;
    __m128 v;
    __m128 ret;
    float  f[4];

    s = _mm_set_ps(1.0, 2.0, 3.0, 4.0); 
    x = _mm_set_ps(1.1, 2.1, 3.1, 4.1); 
    t = _mm_set_ps(1.2, 2.2, 4.0, 4.2); 
    r = _mm_set_ps(0.01, 0.02, 0.04, 0.05); 
    v = _mm_set_ps(0.3, 0.2, 0.4, 0.3); 

    int i;

    for (i = 0; i < 1; i++) {
        ret = BlackScholes(s, x, t, r, v);
    }

    // ret = test(&s);

    _mm_storeu_ps(&f[0], ret);
    printf("%f, %f, %f, %f\n", f[0], f[1], f[2], f[3]);

}

int
main()
{
    mytest();

    return 0;
}
