#include <pthread.h>

int nprod;
int fp1(int nprod) {
    return nprod < 0;
}

void *prod(void *d) {
  pb: nprod ++;
  pe: pthread_exit(NULL);
}

void *cons1(void *d) {
  c1b:
  while (1) {
    while (nprod < 1) {}
    if (nprod >= 1) {
      nprod --;
    }
  }
  c1e:
  pthread_exit(NULL);
}

void *cons2(void *d )
{
  c2b:
  while (1) {
    while (nprod < 1) {}
    if (nprod >= 1) {
      nprod --;
    }
  }
  c2e:
  pthread_exit(NULL);
}

int main(int argc , char **argv ) {
  pthread_t t1 ;
  pthread_t t2 ;
  pthread_t t3 ;

  pthread_create(&t1, 0, &prod, NULL);
  pthread_create(&t2, 0, &cons1, NULL);
  pthread_create(&t3, 0, &cons2, NULL);
  pthread_join(t1, 0);
  pthread_join(t2, 0);
  pthread_join(t3, 0);
  return 0;
}
