
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <stdio.h>

#define GREEN 0
#define ORANGE 1
#define RED 2

sem_t sem1, sem2;

int isRed1 (int s) {
    return s == RED;
}

int isRed2 (int s) {
    return s == RED;
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
    sem_post(&sem1);

    sem_wait(&sem2);
    status = GREEN;
    printf("1 -> GREEN\n");
    /* sleep(2); */
    /* status = ORANGE; */
    /* printf("1 -> ORANGE\n"); */
    /* sleep(1); */
    /* status = RED; */
    /* printf("1 -> RED\n"); */
    /* sem_post(&sem1); */

    /* sem_wait(&sem2); */
    /* status = GREEN; */
    /* printf("1 -> GREEN\n"); */
    /* sleep(2); */
    /* status = ORANGE; */
    /* printf("1 -> ORANGE\n"); */
    /* sleep(1); */
    /* status = RED; */
    /* printf("1 -> RED\n"); */
    /* sem_post(&sem1); */
 e1:
    pthread_exit(NULL);
}

void * signal2(void* d) {
    int status = RED;

 b2:
    sem_wait(&sem1);
    status = GREEN;
    printf("2 -> GREEN\n");
    sleep(2);
    status = ORANGE;
    printf("2 -> ORANGE\n");
    sleep(1);
    status = RED;
    printf("2 -> RED\n");
    sem_post(&sem2);

    /* sem_wait(&sem1); */
    /* status = GREEN; */
    /* printf("2 -> GREEN\n"); */
    /* sleep(2); */
    /* status = ORANGE; */
    /* printf("2 -> ORANGE\n"); */
    /* sleep(1); */
    /* status = RED; */
    /* printf("2 -> RED\n"); */
    /* sem_post(&sem2); */
 e2:

    pthread_exit(NULL);
}

int main() {
    pthread_t t1, t2;

    printf("Start\n");

    sem_init(&sem1, 0, 0);
    sem_init(&sem2, 0, 0);

    printf("Create\n");
    pthread_create(&t1, NULL, signal1, NULL);
    pthread_create(&t2, NULL, signal2, NULL);
    printf("Join\n");

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    printf("End\n");

    return 0;
}
