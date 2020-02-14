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

#define ONRANK if(rank==0)

int main(int argc, char **argv)
{
  int  rank, size;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* -------------------------- */
  /* TEST A: write a empty file */
  /* -------------------------- */
  {
    sion_int64  chunksize = 100;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;

    sid = sion_paropen_mpi("testA.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) sion_parclose_mpi(sid);

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST A\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------ */
  /* TEST B: write a small file with some different pattern */
  /* ------------------------------------------------------ */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum; 
    size_t      bwrote;
    int         i,b,rc;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("testB.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);

    if(sid>=0) {
      /* test too large buffer */
      rc=sion_ensure_free_space(sid,BUFSIZE);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, BUFSIZE,rc);

      /* test buffer which exact fits */
      b=0;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE,rc);
      bwrote = (rc) ? fwrite(buffer,1,CHUNKSIZE,fp) : 0;
      bytes_written=sion_get_bytes_written(sid);
      ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK { printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);fflush(stdout); }

      /* test half size buffer */
      b=1;
      rc=sion_ensure_free_space(sid,CHUNKSIZE/2);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE/2,rc);
      bwrote = (rc) ? fwrite(buffer,1,CHUNKSIZE/2,fp) : 0;
      bytes_written=sion_get_bytes_written(sid);
      ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK { printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);fflush(stdout); }

      /* test buffer which exact fits, leaving gap */
      b=2;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE,rc);
      bwrote = (rc) ? fwrite(buffer,1,CHUNKSIZE,fp) : 0;
      bytes_written=sion_get_bytes_written(sid);
      ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK { printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);fflush(stdout); }

      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B write\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------ */
  /* TEST B: read small file with some different pattern    */
  /* ------------------------------------------------------ */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum; 
    int         i,b;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_paropen_mpi("testB.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    ONRANK {
      printf("on rank %d: sid           =%d\n",rank,sid);
      printf("on rank %d: numfiles      =%d\n",rank,numfiles);
      printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
      printf("on rank %d: globalrank    =%d\n",rank, globalrank);
      printf("on rank %d: newfname      =%s\n",rank, newfname);
    }

    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=fread(buffer,1,bytestoread,fp);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* --------------------------------------------------------------------- */
  /* TEST B: read small file with some different pattern non-collective    */
  /* --------------------------------------------------------------------- */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    int         sid;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum; 
    int         i,b;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_open_rank("testB.out", "br", &chunksize, &fsblksize, &globalrank, &fp);
    ONRANK printf("on rank %d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %d: globalrank    =%d\n",rank, globalrank);

    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);fflush(stdout);
	bytestoread=bytes_avail;
	bread=fread(buffer,1,bytestoread,fp);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
	b++;
      }             

      sion_close(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B read non-collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);



  /* ----------------------------------------------------------------------- */
  /* TEST C: write a small file with some different pattern, without fileptr */
  /* ----------------------------------------------------------------------- */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum; 
    size_t      bwrote;
    int         i,b,rc;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("testB.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);

    if(sid>=0) {
      /* test too large buffer */
      rc=sion_ensure_free_space(sid,BUFSIZE);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, BUFSIZE,rc);

      /* test buffer which exact fits */
      b=0;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE,rc);
      bwrote = (rc) ? sion_fwrite(buffer,1,CHUNKSIZE,sid) : 0;
      bytes_written=sion_get_bytes_written(sid);
      ONRANK { printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);fflush(stdout); }
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK { printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);fflush(stdout); }

      /* test half size buffer */
      b=1;
      rc=sion_ensure_free_space(sid,CHUNKSIZE/2);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE/2,rc);
      bwrote = (rc) ? sion_fwrite(buffer,1,CHUNKSIZE/2,sid) : 0;
      bytes_written=sion_get_bytes_written(sid);
      ONRANK { printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);fflush(stdout); }
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK { printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);fflush(stdout); }

      /* test buffer which exact fits, leaving gap */
      b=2;
      rc=sion_ensure_free_space(sid,CHUNKSIZE);
      ONRANK fprintf(stderr, "on rank %d: ensure_free_space (%d) rc = %d\n",rank, CHUNKSIZE,rc);
      bwrote = (rc) ? sion_fwrite(buffer,1,CHUNKSIZE,sid) : 0;
      bytes_written=sion_get_bytes_written(sid);
      ONRANK { printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);fflush(stdout); }
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK { printf("on rank %d: block=%d           bwrote=%3d blocksum=%8d \n",rank,b, (int) bwrote,(int) sum);fflush(stdout); }

      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST C write\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ----------------------------------------------------------------------- */
  /* TEST B: read small file with some different pattern, without fileptr    */
  /* ----------------------------------------------------------------------- */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum; 
    int         i,b;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_paropen_mpi("testB.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    ONRANK {
      printf("on rank %d: sid           =%d\n",rank,sid);
      printf("on rank %d: numfiles      =%d\n",rank,numfiles);
      printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
      printf("on rank %d: globalrank    =%d\n",rank, globalrank);
      printf("on rank %d: newfname      =%s\n",rank, newfname);
    }

    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);fflush(stdout);
	bytestoread=bytes_avail;
	bread=sion_fread(buffer,1,bytestoread,sid);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
	b++;
      }             

      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST C read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* --------------------------------------------------------------------------------- */
  /* TEST B: read small file with some different pattern non-collective, no fileptr    */
  /* --------------------------------------------------------------------------------- */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 100
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    int         sid;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    long        sum; 
    int         i,b;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_open_rank("testB.out", "br", &chunksize, &fsblksize, &globalrank, NULL);
    ONRANK {
      printf("on rank %d: sid           =%d\n",rank,sid);
      printf("on rank %d: numfiles      =%d\n",rank,numfiles);
      printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
      printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    }

    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);fflush(stdout);
	bytestoread=bytes_avail;
	bread=sion_fread(buffer,1,bytestoread,sid);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
	b++;
      }             

      sion_close(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B read non-collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();

  return(0);

}
