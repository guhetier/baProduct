
#include <pthread.h>

int fp (int a) {
    return 1;
}

int fr(int a, char b, double d) {
    return 1;
}

int fq(int a, int b, int c, int d) {
    return 1;
}

int g1, g2;

void *t1(void *d) {
    int v1, v2, v3;

    v2 = 7;
 l1:;
    v1 = g1;
    v3 = v1 + 1;
    g1 = 8;
 lg1:;
    v2 = 5;
    g2 = 3;
    v1 = g1;
 l2:;
    v3 = v1 + v2;
    g2 = 5;
 lg2:;
    pthread_exit(NULL);
}

void *t2(void *d) {
    int v4, k;
    g1 = 4;
    v4 = 8;
    g2 = 5;
    k = 42;
    g2 += k;
    pthread_exit(NULL);
}

int main() {
    pthread_t th1, th2;
    pthread_create(&th1, NULL, t1, NULL);
    pthread_create(&th2, NULL, t2, NULL);
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    return 0;
}
