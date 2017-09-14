#include <pthread.h>

int fp1() {
    return 1;
}

int fp2() {
    return 1;
}

void* thread1(void* d) {
    int energy_stored = 10;

    int i = 0;

    while(i < 3) {
        i++;
        for(; energy_stored > 0; energy_stored--);
    t1: energy_stored = 10;
    e1: ;
    }

    pthread_exit(NULL);
}

void* thread2(void* d) {

    int energy_stored = 10;

    int i = 0;

    while(i < 3) {
        i++;
        for(; energy_stored > 0; energy_stored--);
    t2: energy_stored = 10;
    e2: ;
    }

    pthread_exit(NULL);
}

int main(int argc, char** argv) {

    pthread_t t1, t2;
    pthread_create(&t1, NULL, thread1, NULL);
    pthread_create(&t2, NULL, thread2, NULL);
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    return 0;
}
