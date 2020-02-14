/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*
 * test_hyb.c
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

#ifdef SION_OMPI
#include <mpi.h>
#include <omp.h>
#endif

#include "sion.h"


int main(int argc, char **argv)
{

#ifdef SION_OMPI
  int  rank, size;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);


  /* -------------------------------------------------------------------------------------------- */
  /* TEST A: write a small file with some different pattern, with collective and individual calls */
  /* -------------------------------------------------------------------------------------------- */

#define BUFSIZE 1000
#define CHUNKSIZE 100

#pragma omp parallel
  if(1) {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 16;
    int         globalrank;
    int         numfiles  = 3;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum;
    size_t      bwrote;
    int         i,b,rc;

    int threadnum = omp_get_thread_num();


    sid = sion_paropen_ompi("testB.out", "bw",&numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    if(sid>=0) {

      printf("on MPI rank %d, OMP thread %d: globalrank=%d\n",rank, threadnum,globalrank);

      /* test buffer which exact fits */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + globalrank*3;
      b=0;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      fprintf(stderr, "on MPI rank %d, OMP thread %d: ensure_free_space (%d) rc = %d\n",rank, threadnum, CHUNKSIZE,rc);
      if(rc) {
	bwrote=sion_coll_fwrite(buffer,1,CHUNKSIZE,sid);
      }
      bytes_written=sion_get_bytes_written(sid);
      printf("on MPI rank %d, OMP thread %d: bytes_written=%lld\n",rank, threadnum,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on MPI rank %d, OMP thread %d: block=%d           bwrote=%3d blocksum=%8d \n",rank, threadnum,b, (int) bwrote,(int) sum);
      /* test half size buffer */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + globalrank*3 + 1;
      b=1;
      rc=sion_ensure_free_space(sid,CHUNKSIZE/2);
      fprintf(stderr, "on MPI rank %d, OMP thread %d: ensure_free_space (%d) rc = %d\n",rank, threadnum, CHUNKSIZE/2,rc);
      if(rc) {
	bwrote=sion_fwrite(buffer,1,CHUNKSIZE/2,sid);
      }
      bytes_written=sion_get_bytes_written(sid);
      printf("on MPI rank %d, OMP thread %d: bytes_written=%lld\n",rank, threadnum,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on MPI rank %d, OMP thread %d: block=%d           bwrote=%3d blocksum=%8d \n",rank, threadnum,b, (int) bwrote,(int) sum);
      /* test buffer which exact fits, leaving gap */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + globalrank*3 + 2;
      b=2;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      fprintf(stderr, "on MPI rank %d, OMP thread %d: ensure_free_space (%d) rc = %d\n",rank, threadnum, CHUNKSIZE,rc);
      if(rc) {
	bwrote=sion_coll_fwrite(buffer,1,CHUNKSIZE,sid);
      }
      bytes_written=sion_get_bytes_written(sid);
      printf("on MPI rank %d, OMP thread %d: bytes_written=%lld\n",rank, threadnum,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on MPI rank %d, OMP thread %d: block=%d           bwrote=%3d blocksum=%8d \n",rank, threadnum,b, (int) bwrote,(int) sum);
      sion_parclose_ompi(sid);
    } else {
      fprintf(stderr, "on MPI rank %d, OMP thread %d: error sid = %d\n",rank, threadnum,sid);
    }

#pragma omp master
    {
      MPI_Barrier(MPI_COMM_WORLD);
    }
#pragma omp barrier
    printf("on MPI rank %d, OMP thread %d: END of TEST B write\n",rank, threadnum);

  } /* omp parallel */

  fflush(stderr);
  fflush(stdout);

  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------ */
  /* TEST B: read small file with some different pattern    */
  /* ------------------------------------------------------ */

#define BUFSIZE 1000
#define CHUNKSIZE 100

#pragma omp parallel
  if(1) {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 16;
    int         globalrank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum;
    int         i,b;

    int threadnum = omp_get_thread_num();

    sid = sion_paropen_ompi("testB.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + globalrank;
    if(rank>=0){
      printf("on MPI rank %d, OMP thread %d: sid           =%d\n",rank,threadnum,sid);
      printf("on MPI rank %d, OMP thread %d: numfiles      =%d\n",rank,threadnum,numfiles);
      printf("on MPI rank %d, OMP thread %d: chunksize     =%d\n",rank,threadnum,(int) chunksize);
      printf("on MPI rank %d, OMP thread %d: globalrank    =%d\n",rank,threadnum, globalrank);
      printf("on MPI rank %d, OMP thread %d: newfname      =%s\n",rank,threadnum, newfname);
    }
    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {
	bytes_avail=sion_bytes_avail_in_block(sid);
	if(rank>=0)printf("on MPI rank %d, OMP thread %d: block=%d           bytes_avail=%d\n",rank,threadnum,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	if(rank>=0)printf("on MPI rank %d, OMP thread %d: block=%d           bread=%3d blocksum=%8d \n",rank,threadnum,b, (int) bread,(int) sum);
	b++;
      }

      sion_parclose_ompi(sid);
    } else {
      fprintf(stderr, "on MPI rank %d, OMP thread %d: error sid = %d\n",rank,threadnum,sid);

    }

#pragma omp master
    {
      MPI_Barrier(MPI_COMM_WORLD);
    }
#pragma omp barrier
    if(rank==0) printf("on MPI rank %d, OMP thread %d: END of TEST B read collective\n",rank,threadnum);
  }  /* omp parallel */


  MPI_Finalize();
#endif
  return 0;

}

