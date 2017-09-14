
#include <pthread.h>

int nprod = 0;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;;

int fp1(int nprod) {
    return nprod < 0;
}

void* prod(void* d) {
 pb:
    pthread_mutex_lock(&m);
    nprod++;
    pthread_mutex_unlock(&m);
    pthread_mutex_lock(&m);
    nprod++;
    pthread_mutex_unlock(&m);
    pthread_mutex_lock(&m);
    nprod++;
    pthread_mutex_unlock(&m);
    pthread_mutex_lock(&m);
    nprod++;
    pthread_mutex_unlock(&m);
    pthread_mutex_lock(&m);
    nprod++;
    pthread_mutex_unlock(&m);
 pe:;
    pthread_exit(NULL);
}

void* cons1(void* d) {
 c1b:
    while(1) {
        /* while(nprod < 1); */
        pthread_mutex_lock(&m);
        if (nprod >= 1)
            nprod--;
        pthread_mutex_unlock(&m);
    }
 c1e:;
    pthread_exit(NULL);
}

void* cons2(void* d) {
 c2b:
    while(1) {
        /* while(nprod < 1); */
        pthread_mutex_lock(&m);
        if (nprod >= 1)
            nprod--;
        pthread_mutex_unlock(&m);
    }
 c2e:;
    pthread_exit(NULL);
}

int main(int argc, char** argv) {

    pthread_t t1, t2, t3;
    pthread_create(&t1, NULL, prod, NULL);
    pthread_create(&t2, NULL, cons1, NULL);
    pthread_create(&t3, NULL, cons2, NULL);
    return 0;
}
