
#include <pthread.h>

pthread_mutex_t sem = PTHREAD_MUTEX_INITIALIZER;;
int ans;
int req;

int requete(int r) {
    return r != 0;
}

int answer(int a) {
    return a != 0;
}

void* client(void* d) {

 cb:;
    pthread_mutex_unlock(&sem);
    req = 42;
    pthread_mutex_lock(&sem);
    int myVar = ans;
    ans = 0;
 ce:;
    pthread_exit(NULL);
}

void* server(void* d) {

 sb: ;
    pthread_mutex_lock(&sem);
    ans = req;
    req = 0;
    pthread_mutex_unlock(&sem);
 se:;
    pthread_exit(NULL);
}

int main() {

    pthread_t t1, t2;
    pthread_mutex_lock(&sem);
    pthread_create(&t2, NULL, server, NULL);
    pthread_create(&t1, NULL, client, NULL);
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    return 0;
}
