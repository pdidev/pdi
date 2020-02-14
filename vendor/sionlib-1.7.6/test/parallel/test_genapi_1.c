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

#include "sion_debug.h"
#include "sion.h"

#define ONRANK if(rank==0)

#include "test_genapi_INC_MPI_api.c"

int main(int argc, char **argv)
{
  int rank, size;
  int aid;
  _sample_api_commdata gcomm;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  DPRINTFP((1, "test_genapi_1", rank, "start API generation on rank %d\n",rank));

  /* -------------------------- */
  /* TEST 0: define new API     */
  /* -------------------------- */
  {
    aid=sion_generic_create_api("SampleAPI");
    printf("on rank %d: new API: aid=%d\n",rank,aid);  

    gcomm.comm=MPI_COMM_WORLD;
    gcomm.commset=1;
    gcomm.local=0;    gcomm.rank=rank; gcomm.rank=size;
  
    sion_generic_register_create_local_commgroup_cb(aid,&_sample_create_lcg_cb);
    sion_generic_register_free_local_commgroup_cb(aid,&_sample_free_lcg_cb);

    sion_generic_register_barrier_cb(aid,&_sample_barrier_cb);
    sion_generic_register_bcastr_cb(aid,&_sample_bcastr_cb);
    sion_generic_register_gatherr_cb(aid,&_sample_gatherr_cb);
    sion_generic_register_scatterr_cb(aid,&_sample_scatterr_cb);
    sion_generic_register_gathervr_cb(aid,&_sample_gathervr_cb);
    sion_generic_register_scattervr_cb(aid,&_sample_scattervr_cb);

    ONRANK printf("on rank %d: END of TEST 0\n",rank);
   
  }

  /* ------------------------------------------- */
  /* TEST B: write a empty file with generic API */
  /* ------------------------------------------- */
  if(1) {
    sion_int64  chunksize = 100;
    sion_int32  fsblksize = 10;
    int         filenumber  = 0;
    int         numfiles  = 1;
    int         lrank, lsize;
    int         sid;
    FILE       *fp;

    lrank=rank, lsize=size;
    sid = sion_generic_paropen(aid,"testB.out", "bw", &chunksize, &fsblksize, &gcomm, rank, size, &filenumber, &numfiles, &lrank, &lsize, &fp, NULL);
    if(sid>=0) sion_generic_parclose(sid); 

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#define BUFSIZE 1000  
#define CHUNKSIZE 100

  /* ------------------------------------------- */
  /* TEST C: write a multi-file with generic API */
  /* ------------------------------------------- */
  if(1){
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         filenumber  = 0;
    int         numfiles  = 1;
    int         lrank, lsize;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    int         sid;
    FILE       *fp;
    long        sum; 
    size_t      bwrote;
    int         i,b,rc;

    numfiles=2;
    filenumber=rank%2;
    lrank=(int) rank/2, lsize=size/2;
    sid = sion_generic_paropen(aid,"testC.out", "bw", &chunksize, &fsblksize, &gcomm, rank, size, &filenumber, &numfiles, &lrank, &lsize, &fp, NULL);

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

    
    if(sid>=0) sion_generic_parclose(sid); 

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST C (write)\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#define BUFSIZE 1000  
#define CHUNKSIZE 100

  /* ------------------------------------------- */
  /* TEST C: read a multi-file with generic API  */
  /* ------------------------------------------- */
  if(1){
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         filenumber  = 0;
    int         numfiles  = 1;
    int         lrank, lsize;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    int         sid;
    FILE       *fp;
    long        sum; 
    int         i,b;

    numfiles=2;
    filenumber=rank%2;
    lrank=(int) rank/2, lsize=size/2;
    sid = sion_generic_paropen(aid,"testC.out", "br", &chunksize, &fsblksize, &gcomm, rank, size, &filenumber, &numfiles, &lrank, &lsize, &fp, NULL);

    printf("on rank %d: chunksize=%lld\n",rank,chunksize);
    printf("on rank %d: fsblksize=%d\n",rank,fsblksize);
    printf("on rank %d: filenumber=%d\n",rank,filenumber);
    printf("on rank %d: numfiles=%d\n",rank,numfiles);
    printf("on rank %d: globalrank    =%d\n",rank, globalrank);

    if(sid>=0) {
      b=0;
      while((!sion_feof(sid))) {    
	bytes_avail=sion_bytes_avail_in_block(sid);  
	printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_fread(buffer,1,bytestoread,sid);     
	if(bread==0) {break;}
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }             
      sion_generic_parclose(sid); 
    }
  }
  
  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST C (read)\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------- */
  /* TEST D: read a multi-file with generic API (dup)  */
  /* ------------------------------------------------- */
  if(1){
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         filenumber  = 0;
    int         numfiles  = 1;
    int         lrank, lsize;
    char        buffer[BUFSIZE];
    sion_int64  bytes_avail=-1;
    size_t      bytestoread, bread;
    int         sid,new_sid;
    FILE       *fp;
    long        sum; 
    int         i,b;

    numfiles=2;
    filenumber=rank%2;
    lrank=(int) rank/2, lsize=size/2;
    sid = sion_generic_paropen(aid,"testC.out", "br", &chunksize, &fsblksize, &gcomm, rank, size, &filenumber, &numfiles, &lrank, &lsize, &fp, NULL);

    printf("on rank %d: chunksize=%lld\n",rank,chunksize);
    printf("on rank %d: fsblksize=%d\n",rank,fsblksize);
    printf("on rank %d: filenumber=%d\n",rank,filenumber);
    printf("on rank %d: numfiles=%d\n",rank,numfiles);
    printf("on rank %d: globalrank    =%d\n",rank, globalrank);

    if(sid>=0) {
      
      /* dup sion file pointer  */
      new_sid=sion_dup(sid,SION_DUP_ALL,0,0);
      printf("on rank %d: dup, new_sid       =%d\n",rank,new_sid);

      /* read procedure */
      b=0;
      while((!sion_feof(new_sid))) {    
	bytes_avail=sion_bytes_avail_in_block(new_sid);  
	printf("on rank %d: block=%d           bytes_avail=%d\n",rank,b,(int) bytes_avail);
	bytestoread=bytes_avail;
	bread=sion_fread(buffer,1,bytestoread,new_sid);     
	if(bread==0) {break;}
	for (i = 0,sum=0; i < bytestoread; i++) sum=sum+buffer[i];
	printf("on rank %d: block=%d           bread=%3d blocksum=%8d \n",rank,b, (int) bread,(int) sum);
	b++;
      }

      /* destry dupped sion file pointer */
      printf("on rank %d: dedup(%d)\n",rank,new_sid);
      sion_dedup(new_sid);
      
      sion_generic_parclose(sid); 
    }
  }
  
  sion_generic_free_api(aid);

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST D (read)\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
