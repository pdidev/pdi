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

#define READ_NOT_COLLECTIVE
#define READ_COLLECTIVE

int main(int argc, char **argv)
{
  int  rank, size;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* ------------------------------------------------------------- */
  /* TEST A1: write a small file with some different pattern (Coll) */
  /* ------------------------------------------------------------- */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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
    int         i;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("test_coll1.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {
      /* write nothing to check 0 as input */
      bwrote=sion_coll_fwrite(buffer,0,CHUNKSIZE,sid);
      bwrote=sion_coll_fwrite(buffer,1,0,sid);

      bwrote=sion_coll_fwrite(buffer,1,CHUNKSIZE,sid);
      
      bytes_written=sion_get_bytes_written(sid);
      ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);
     
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST A1 write coll\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------------------------------- */
  /* TEST A2: write a small file with some different pattern (Coll, without fileptr) */
  /* ------------------------------------------------------------------------------- */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum; 
    size_t      bwrote;
    int         i;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("test_coll2.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    if(sid>=0) {
      bwrote=sion_coll_fwrite(buffer,1,CHUNKSIZE,sid);
      
      bytes_written=sion_get_bytes_written(sid);
      ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);
     
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST A2 write coll\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------ */
  /* TEST A3: test collsize as flag */
  /* ------------------------------ */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    char        buffer[BUFSIZE];
    int         i;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    /* without collsize */
    sid = sion_paropen_mpi("test_coll2.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    if(sid>=0) {
      sion_coll_fwrite(buffer,1,CHUNKSIZE,sid);
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
    sid = sion_paropen_mpi("test_coll2.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    if(sid>=0) {
      ONRANK printf("on rank %d: chunksize     =%lld\n",rank, chunksize);
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }

    chunksize = CHUNKSIZE;
    /* with collsize */
    sid = sion_paropen_mpi("test_coll2.out", "bw,collective,collsize=4", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    if(sid>=0) {
      sion_coll_fwrite(buffer,1,CHUNKSIZE,sid);
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
    sid = sion_paropen_mpi("test_coll2.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    if(sid>=0) {
      ONRANK printf("on rank %d: chunksize     =%lld\n",rank, chunksize);
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST A3 write coll\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* --------------------------------------------------------------- */
  /* TEST B: write a small file with some different pattern (Indiv.) */
  /* --------------------------------------------------------------- */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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
    int         i;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("test_indiv.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {
      bwrote=sion_fwrite(buffer,1,CHUNKSIZE,sid);
      
      bytes_written=sion_get_bytes_written(sid);
      ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);fflush(stdout);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      ONRANK printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);fflush(stdout);
     
      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B write coll\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);



#ifdef READ_NOT_COLLECTIVE
  /* --------------------------------------------------------------------------------- */
  /* TEST C.1: read small file with some different pattern (indiv., coll. file)        */
  /* --------------------------------------------------------------------------------- */
  {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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

    sid = sion_paropen_mpi("test_coll1.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    ONRANK printf("on rank %d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	ONRANK printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);fflush(stdout);
	bytestoread=bytes_avail;
        /* write nothing to check 0 as input */
	bread=sion_fread(buffer,0,bytestoread,sid);
	bread=sion_fread(buffer,1,0,sid);

	bread=sion_fread(buffer,1,bytestoread,sid);
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
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
  ONRANK printf("on rank %d: END of TEST C.1 read individual call from collective file\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* --------------------------------------------------------------------------------- */
  /* TEST C.2: read small file with some different pattern (indiv., indiv. file)       */
  /* --------------------------------------------------------------------------------- */
  {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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

    sid = sion_paropen_mpi("test_indiv.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    ONRANK printf("on rank %d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	ONRANK printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_fread(buffer,1,bytestoread,sid);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);
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
  ONRANK printf("on rank %d: END of TEST C.2 read individual call from individual file\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#endif

#ifdef READ_COLLECTIVE
  /* --------------------------------------------------------------------------------- */
  /* TEST D.1: read small file with some different pattern (coll., coll. file)         */
  /* --------------------------------------------------------------------------------- */
  {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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

    sid = sion_paropen_mpi("test_coll1.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    ONRANK printf("on rank %d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      /* to check that everyone has data to read, otherwise error */
      bytes_avail=sion_bytes_avail_in_block(sid);  
      if(bytes_avail<=0) {
	ONRANK fprintf(stderr,"on rank %d: ERROR: no bytes available in file bytes_avail=%d\n",rank,(int) bytes_avail);
      	MPI_Abort(MPI_COMM_WORLD,1);
      }

      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	ONRANK printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);fflush(stdout);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
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
  ONRANK printf("on rank %d: END of TEST D.1 read collective from collective file\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* --------------------------------------------------------------------------------------------- */
  /* TEST D.2: read small file with some different pattern (coll., coll. file, no fileptr)         */
  /* --------------------------------------------------------------------------------------------- */
  {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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

    sid = sion_paropen_mpi("test_coll1.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, NULL, &newfname);
    ONRANK printf("on rank %d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      /* to check that everyone has data to read, otherwise error */
      bytes_avail=sion_bytes_avail_in_block(sid);  
      if(bytes_avail<=0) {
	ONRANK fprintf(stderr,"on rank %d: ERROR: no bytes available in file bytes_avail=%d\n",rank,(int) bytes_avail);
      	MPI_Abort(MPI_COMM_WORLD,1);
      }

      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	ONRANK printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);fflush(stdout);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
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
  ONRANK printf("on rank %d: END of TEST D.2 read collective from collective file\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* --------------------------------------------------------------------------------- */
  /* TEST D.3: read small file with some different pattern (coll., indiv. file)        */
  /* --------------------------------------------------------------------------------- */
  {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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

    sid = sion_paropen_mpi("test_indiv.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    ONRANK printf("on rank %d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      /* to check that everyone has data to read, otherwise error */
      bytes_avail=sion_bytes_avail_in_block(sid);  
      if(bytes_avail<=0) {
	ONRANK fprintf(stderr,"on rank %d: ERROR: no bytes available in file bytes_avail=%d\n",rank,(int) bytes_avail);
      	MPI_Abort(MPI_COMM_WORLD,1);
      }

      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	ONRANK printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);fflush(stdout);
	bytestoread=bytes_avail;
	bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
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
  ONRANK printf("on rank %d: END of TEST D.3 read collective from individual file\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#endif

  /* --------------------------------------------------------------- */
  /* TEST E: write collective with a sequence of chunks,calls        */
  /* --------------------------------------------------------------- */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
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
    int         i,j;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("test_coll_sequence.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {

      /* 3 times full chunks  */
      for(j=0;j<3;j++) {
	bwrote=sion_coll_fwrite(buffer,1,CHUNKSIZE,sid);
	
	bytes_written=sion_get_bytes_written(sid);
	ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
	for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);
      }

      /* 6 times half chunks  */
      for(j=0;j<6;j++) {
	bwrote=sion_coll_fwrite(buffer,1,CHUNKSIZE/2,sid);
	
	bytes_written=sion_get_bytes_written(sid);
	ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
	for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);
      }

      /* 6 times 3/4 chunks  */
      for(j=0;j<6;j++) {
	bwrote=sion_coll_fwrite(buffer,1,CHUNKSIZE*3/4,sid);
	
	bytes_written=sion_get_bytes_written(sid);
	ONRANK printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
	for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
	ONRANK printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);
      }

      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST E write coll sequence\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* --------------------------------------------------------------- */
  /* TEST F: read collective with a sequence of chunks,calls         */
  /* --------------------------------------------------------------- */
  {
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 2500;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    char        buffer[BUFSIZE];
    size_t      bytestoread, bread;
    long        sum; 
    int         i,j,b;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_paropen_mpi("test_coll_sequence.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    ONRANK printf("on rank %d: sid           =%d\n",rank,sid);
    ONRANK printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    ONRANK printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    ONRANK printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    ONRANK printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      b=0;

      /* 3 times full chunks  */
      for(j=0;j<3;j++) {
	if(!sion_feof(sid)) {    
	  bytestoread=CHUNKSIZE;
	  bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	  for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];	  b++;
	  ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
	}             
      }

      /* 6 times half chunks  */
      for(j=0;j<6;j++) {
	if(!sion_feof(sid)) {    
	  bytestoread=CHUNKSIZE/2;
	  bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	  for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];	  b++;
	  ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
	}             
      }

      /* 6 times 3/4 chunks  */
      for(j=0;j<6;j++) {
	if(!sion_feof(sid)) {    
	  bytestoread=CHUNKSIZE*3/4;
	  bread=sion_coll_fread(buffer,1,bytestoread,sid);     
	  for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];	  b++;
	  ONRANK printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);fflush(stdout);
	}             
      }

      sion_parclose_mpi(sid);
    } else {
      ONRANK fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST F read collective from collective file\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();

  return(0);
}
