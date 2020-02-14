/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*
 * test_omp.c
 *
 *  Created on: Jun 29, 2010
 *      Author: dmontoya
 */


#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <time.h>
#include <math.h>

#include "sion.h"
#include "sion_debug.h"
/* from configure */

#ifdef SION_OMP
#include <omp.h>
#endif

static int lock_counter = 0;
static int unlock_counter = 0;

#ifdef SION_PTHREADS
#include <pthread.h>
static pthread_mutex_t lock_data = PTHREAD_MUTEX_INITIALIZER;

int user_lock(void *data) {
  int rc=SION_SUCCESS;
  if(pthread_mutex_lock( (pthread_mutex_t *) data)) {
    rc=SION_NOT_SUCCESS;
  };
  lock_counter++;
  DPRINTFP((1, "test_lock___1", -1, "lock___(%3d)\n",lock_counter));
  return(rc);
}
int user_unlock(void * data) {
  int rc=SION_SUCCESS;
  unlock_counter++;
  DPRINTFP((1, "test_lock___1", -1, "unlock_(%3d)\n",unlock_counter));
  if(pthread_mutex_unlock( (pthread_mutex_t *) data)) {
    rc=SION_NOT_SUCCESS;
  };
  return(rc);
}
#else
static  omp_lock_t lock_data;
int user_lock(void * data) {
  int rc=SION_SUCCESS;
  omp_set_lock(&lock_data);
  lock_counter++;
  DPRINTFP((1, "test_lock___1", -1, "lock___(%3d)\n",lock_counter));
  return(rc);
}
int user_unlock(void * data) {
  int rc=SION_SUCCESS;
  unlock_counter++;
  DPRINTFP((1, "test_lock___1", -1, "unlock_(%3d)\n",unlock_counter));
  omp_unset_lock(&lock_data);
  return(rc);
}
#endif



int main(int argc, char **argv)
{


#ifdef SION_OMP
  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  #ifndef SION_PTHREADS
  omp_init_lock(&lock_data);
  #endif

  _sion_debug_set_query_thread_num_function(omp_get_thread_num);

#pragma omp parallel
  {
    int thread_num = omp_get_thread_num();
    int         rc;

    DPRINTFP((1, "test_lock___1", thread_num, "start lock test\n"));

    fprintf(stderr, "on OMP thread %d: user callbacks defined: rc=%d\n",thread_num,sion_lock_user_callbacks_defined());
    /* to avoid race conditions when counting lock events */
    #pragma omp barrier
    rc=sion_lock_register_lock_callbacks(user_lock,user_unlock,&lock_data);
    fprintf(stderr, "                  lock_user_callbacks_defined returns rc=%d\n",rc);
    #pragma omp barrier
    fprintf(stderr, "on OMP thread %d: user callbacks defined: rc=%d\n",thread_num,sion_lock_user_callbacks_defined());

  }
  printf("on OMP thread 0: (PROLOG) lock_counter  =%d \n",lock_counter);
  printf("on OMP thread 0: (PROLOG) unlock_counter=%d \n",unlock_counter);

  /* -------------------------- */
  /* TEST A: write a empty file */
  /* -------------------------- */

#pragma omp parallel
  {
	  sion_int64  chunksize = 100;
	  sion_int32  fsblksize = 10;
    int         globalrank;
    char       *newfname=NULL;
    int         sid;
    FILE       *fp;

    sid = sion_paropen_omp("testA2.out", "bw",&chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) sion_parclose_omp(sid);
    fflush(stderr);
    fflush(stdout);
    printf("on OMP thread %d: END of TEST A 2 files\n",globalrank);
  }
  printf("on OMP thread 0: (TEST A) lock_counter  =%d \n",lock_counter);
  printf("on OMP thread 0: (TEST A) unlock_counter=%d \n",unlock_counter);


  /* ------------------------------------------------------ */
  /* TEST B: write a small file with some different pattern */
  /* ------------------------------------------------------ */


#define BUFSIZE 1000
#define CHUNKSIZE 100

#pragma omp parallel
  {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 16;
    int         globalrank;
    char       *newfname=NULL;
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum;
    size_t      bwrote;
    int         i,b,rc;

    int thread_num = omp_get_thread_num();

    sid = sion_paropen_omp("testB.out", "bw", &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {

      /* test buffer which exact fits */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + thread_num*3;
      b=0;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      fprintf(stderr, "on OMP thread %d: ensure_free_space (%d) rc = %d\n",thread_num, CHUNKSIZE,rc);
      if(rc) {
    	  bwrote=fwrite(buffer,1,CHUNKSIZE,fp);
      }
      bytes_written=sion_get_bytes_written(sid);
      printf("on OMP thread %d: bytes_written=%lld\n",thread_num,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on OMP thread %d: block=%d           bwrote=%3d blocksum=%8d \n",thread_num,b, (int) bwrote,(int) sum);

      /* test half size buffer */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + thread_num*3 + 1;
      b=1;
      rc=sion_ensure_free_space(sid,CHUNKSIZE/2);
      fprintf(stderr, "on OMP thread %d: ensure_free_space (%d) rc = %d\n",thread_num, CHUNKSIZE/2,rc);
      if(rc) {
    	  bwrote=fwrite(buffer,1,CHUNKSIZE/2,fp);
      }
      bytes_written=sion_get_bytes_written(sid);
      printf("on OMP thread %d: bytes_written=%lld\n",thread_num,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on OMP thread %d: block=%d           bwrote=%3d blocksum=%8d \n",thread_num,b, (int) bwrote,(int) sum);

      /* test buffer which exact fits, leaving gap */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + thread_num*3 + 2;
      b=2;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      fprintf(stderr, "on OMP thread %d: ensure_free_space (%d) rc = %d\n",thread_num, CHUNKSIZE,rc);
      if(rc) {
    	  bwrote=fwrite(buffer,1,CHUNKSIZE,fp);
      }
      bytes_written=sion_get_bytes_written(sid);
      printf("on OMP thread %d: bytes_written=%lld\n",thread_num,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on OMP thread %d: block=%d           bwrote=%3d blocksum=%8d \n",thread_num,b, (int) bwrote,(int) sum);

      sion_parclose_omp(sid);
    } else {
      fprintf(stderr, "on OMP thread %d: error sid = %d\n",thread_num,sid);

      printf("on OMP thread %d: END of TEST B write\n",thread_num);
    }
    fflush(stderr);
    fflush(stdout);



}


    /* ------------------------------------------------------ */
    /* TEST B: read small file with some different pattern    */
    /* ------------------------------------------------------ */

  #define BUFSIZE 1000
  #define CHUNKSIZE 100

  #pragma omp parallel
    {
      sion_int64  chunksize = CHUNKSIZE;
      sion_int32  fsblksize = 16;
      int         globalrank;
      char       *newfname=NULL;
      int         sid;
      FILE       *fp;
      char        buffer[BUFSIZE];
      sion_int64  bytes_avail=-1;
      size_t      bytestoread, bread;
      long        sum;
      int         i,b;

      int thread_num = omp_get_thread_num();

       sid = sion_paropen_omp("testB.out", "br", &chunksize, &fsblksize, &globalrank, &fp, &newfname);

       for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + thread_num;

		 printf("on OMP thread %d: sid           =%d\n",thread_num,sid);
		 printf("on OMP thread %d: chunksize     =%d\n",thread_num,(int) chunksize);
		 printf("on OMP thread %d: thread_num    =%d\n",thread_num, thread_num);
		 printf("on OMP thread %d: newfname      =%s\n",thread_num, newfname);

      if(sid>=0) {
        b=0;
        while((!sion_feof(sid))) {
  		bytes_avail=sion_bytes_avail_in_block(sid);
  		printf("on OMP thread %d: block=%d           bytes_avail=%d\n",thread_num,b,(int) bytes_avail);
  		bytestoread=bytes_avail;
  		bread=fread(buffer,1,bytestoread,fp);
  		for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
  		printf("on OMP thread %d: block=%d           bread=%3d blocksum=%8d \n",thread_num,b, (int) bread,(int) sum);
  		b++;
        }

        sion_parclose_omp(sid);
      } else {
        fprintf(stderr, "on OMP thread %d: error sid = %d\n",thread_num,sid);

      }

      #pragma omp barrier
      printf("on OMP thread %d: END of TEST B read collective\n",thread_num);
    }

#pragma omp parallel
    {
      int thread_num = omp_get_thread_num();
      printf("on OMP thread %d: END of last OMP barrier\n",thread_num);
      #pragma omp barrier
    }
    printf("on OMP thread 0: (END) lock_counter  =%d \n",lock_counter);
    printf("on OMP thread 0: (END) unlock_counter=%d \n",unlock_counter);

    fflush(stderr);
    fflush(stdout);


#endif
return 0;
}

