#include <pthread.h>

int fp1(int e) {
    return e < 3;
}

int fp2(int e) {
    return e < 5;
}

void* thread1(void* d) {
    int energy_stored = 10;
    int i = 0;

    while(i < 3) {
        i++;
    t1: while (energy_stored > 0) {
            energy_stored--;
        }
    energy_stored = 10;
    e1: ;
    }

    pthread_exit(NULL);
}

void* thread2(void* d) {

    int energy_stored = 10;
    int i = 0;
    while(i < 3) {
        i++;
    t2: while (energy_stored > 0) {
            energy_stored--;
        }
    energy_stored = 10;
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
