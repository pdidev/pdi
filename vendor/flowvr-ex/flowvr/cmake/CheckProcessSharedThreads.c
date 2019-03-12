#include <sys/types.h>
#include <pthread.h>

#ifdef __CLASSIC_C__
int main(){
  int ac;
  char*av[];
#else
int main(int ac, char*av[]){
#endif
  pthread_mutex_t mutex;
  pthread_mutexattr_t attr;
  if (pthread_mutexattr_init(&attr))
    return 1;
  if (pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED))
    return 2;
  if (pthread_mutex_init(&mutex, &attr))
    return 3;
  if (pthread_mutexattr_destroy(&attr))
    return 4;
  if (pthread_mutex_destroy(&mutex))
    return 5;
   return 0;
}
