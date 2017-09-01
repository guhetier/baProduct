
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

#define GREEN 0
#define ORANGE 1
#define RED 2

pthread_mutex_t m1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t m2 = PTHREAD_MUTEX_INITIALIZER;

int isGreen1 (int s) {
    return s == GREEN;
}

int isGreen2 (int s) {
    return s == GREEN;
}

void * signal1(void* d) {
    int status = GREEN;
 b1:
    printf("1 -> GREEN\n");
    sleep(2);
    status = ORANGE;
    printf("1 -> ORANGE\n");
    sleep(1);
    status = RED;
    printf("1 -> RED\n");
    pthread_mutex_unlock(&m1);

    pthread_mutex_lock(&m2);
    status = GREEN;
    printf("1 -> GREEN\n");
    sleep(2);
    status = ORANGE;
    printf("1 -> ORANGE\n");
    sleep(1);
    status = RED;
    printf("1 -> RED\n");
    pthread_mutex_unlock(&m1);

    /* pthread_mutex_lock(&m2); */
    /* status = GREEN; */
    /* printf("1 -> GREEN\n"); */
    /* sleep(2); */
    /* status = ORANGE; */
    /* printf("1 -> ORANGE\n"); */
    /* sleep(1); */
    /* status = RED; */
    /* printf("1 -> RED\n"); */
    /* pthread_mutex_unlock(&m1); */
 e1:
    pthread_exit(NULL);
}

void * signal2(void* d) {
    int status = RED;
    printf("2 -> RED\n");

 b2:
    pthread_mutex_lock(&m1);
    status = GREEN;
    printf("2 -> GREEN\n");
    sleep(2);
    status = ORANGE;
    printf("2 -> ORANGE\n");
    sleep(1);
    status = RED;
    printf("2 -> RED\n");
    pthread_mutex_unlock(&m2);

    /* pthread_mutex_lock(&m1); */
    /* status = GREEN; */
    /* printf("2 -> GREEN\n"); */
    /* sleep(2); */
    /* status = ORANGE; */
    /* printf("2 -> ORANGE\n"); */
    /* sleep(1); */
    /* status = RED; */
    /* printf("2 -> RED\n"); */
    /* pthread_mutex_unlock(&m2); */
 e2:

    pthread_exit(NULL);
}

int main() {
    pthread_t t1, t2;

    printf("Start\n");

    pthread_mutex_lock(&m1);
    pthread_mutex_lock(&m2);

    printf("Create\n");
    pthread_create(&t1, NULL, signal1, NULL);
    pthread_create(&t2, NULL, signal2, NULL);
    printf("Join\n");

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    printf("End\n");

    return 0;
}
