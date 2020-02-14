/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <mpi.h>
#include <time.h>
#include <math.h>

#include "sion.h"

int main(int argc, char **argv)
{
  int  rank, size;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* ------------------------------------------------------ */
  /* TEST A: write a small file with some different pattern */
  /* ------------------------------------------------------ */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 3;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum; 
    size_t      bwrote;
    int         i,b,rc;


    sid = sion_paropen_mpi("testA.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {

      /* test buffer which exact fits */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank*3;
      b=0;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE,rc);
      bwrote = (rc) ? fwrite(buffer,1,CHUNKSIZE,fp) : 0;
      bytes_written=sion_get_bytes_written(sid);
      printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);

      /* test half size buffer */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank*3 + 1;
      b=1;
      rc=sion_ensure_free_space(sid,CHUNKSIZE/2);
      fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE/2,rc);
      bwrote = (rc) ? fwrite(buffer,1,CHUNKSIZE/2,fp) : 0;
      bytes_written=sion_get_bytes_written(sid);
      printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);

      /* test buffer which exact fits, leaving gap */
      for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank*3 + 2;
      b=2;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE,rc);
      bwrote = (rc) ? fwrite(buffer,1,CHUNKSIZE,fp) : 0;
      bytes_written=sion_get_bytes_written(sid);
      printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);
      
      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST A write\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------ */
  /* TEST B: read multi file with serial interface          */
  /* ------------------------------------------------------ */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100

    if(rank==0) {
      char *fname="testA.out";
      int numTasks,numFiles,*ranks=0;
      sion_int64 *chunksizes=0;
      sion_int32 blocksize=-1;
      FILE *mainFileptr;
      int rank=0, posinblk=0;
      int fileId = sion_open(fname, "rb", &numTasks, &numFiles,
			     &chunksizes, &blocksize, &ranks, &mainFileptr);
      printf( "on rank %d: open(name=\"%s\", \"rb\", &numTasks, " "&numFiles, &chunksizes, &blocksize, &ranks, &mainFileptr)==%d\n",
	      rank,fname,fileId);
      FILE *fileptr;
      int success=sion_seek_fp(fileId,rank,posinblk,(sion_int64)0,&fileptr);
      printf("on rank %d: seek_fp(fileId=%d,0,0,0,&fileptr)==%d\n",rank, fileId,success);

      if(fileId>=0)   sion_close(fileId);
    } else {
      fprintf(stderr, "on rank %d: no action\n",rank);
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST A write\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
