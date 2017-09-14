
#include <pthread.h>

int score = 0;

int p_ended(int f) {
    return f;
}

int w1(int s) {
    return s == 10;
}

void* player1(void* d) {
    int f = 0;
 r1b: ;
    f = 1;
    score = 10;
 r1e:;
    pthread_exit(NULL);
}

void* player2(void* d) {
    int f = 0;
 r2b:
    score = -10;
    f = 1;
 r2e:;
    pthread_exit(NULL);
}

int main() {

    pthread_t t1, t2;
    pthread_create(&t2, NULL, player1, NULL);
    pthread_create(&t1, NULL, player2, NULL);
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    return 0;
}
