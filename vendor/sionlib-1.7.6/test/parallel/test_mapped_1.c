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

  /* ------------------------------------------------------------- */
  /* TEST A: create from 4 task one sion file containing 16 tasks  */
  /* ------------------------------------------------------------- */
  if(0){
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int32  fsblksize = 100;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int        *mapping_filenrs;
    int        *mapping_lranks;
    int         numfiles    = 1;
    int         nlocaltasks = 4;
    int         sid;
    FILE       *fp;
    int         i;

#if 0
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum; 
    size_t      bwrote;
#endif

    chunksizes = (sion_int64 *) malloc(nlocaltasks * sizeof(sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,"TEST A: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(sion_int64));
      return(1);
    }
    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST A: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }
    mapping_filenrs = (int *) malloc(nlocaltasks * sizeof(int));
    if (mapping_filenrs == NULL) {
      fprintf(stderr,"TEST A: cannot allocate mapping_filenrs size %lu, aborting ...\n",
	      (unsigned long) mapping_filenrs * sizeof(int));
      return(1);
    }
    mapping_lranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (mapping_lranks == NULL) {
      fprintf(stderr,"TEST A: cannot allocate mapping_lranks size %lu, aborting ...\n",
	      (unsigned long) mapping_lranks * sizeof(int));
      return(1);
    }

#if 0
    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;
#endif
    for (i = 0; i < nlocaltasks; i++) {
      globalranks[i] = rank*nlocaltasks+i;
      chunksizes[i]  = CHUNKSIZE*2;
      mapping_filenrs[i] = 0; /* one file */
      mapping_lranks[i]  = globalranks[i]; /* one file */
    }

    sid = sion_paropen_mapped_mpi("test_mappedA.out", "bw", &numfiles, MPI_COMM_WORLD, 
				  &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				  &fsblksize, &fp);
    if(sid>=0) {

#if 0
      chunksize=CHUNKSIZE;
      sion_parreinit_mpi(sid,chunksize);
      bwrote=sion_fwrite(buffer,1,CHUNKSIZE,sid);
      
      bytes_written=sion_get_bytes_written(sid);
      printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
      for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
      printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);
#endif
     
      sion_parclose_mapped_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }


  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST A write mapped\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------ */
  /* TEST B: create from 4 task multiple sion file containing 16 tasks  */
  /* ------------------------------------------------------------------ */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int32  fsblksize = 100; 
    /* sion_int32  fsblksize = -1; */
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int        *mapping_filenrs;
    int        *mapping_lranks;
    int         numfiles    = 3;
    int         nlocaltasks = 5;
    int         sid;
    FILE       *fp;
    int         i,j,t,c,taskperfile,grank;

    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    long        sum; 
    size_t      bwrote;

    chunksizes = (sion_int64 *) malloc(nlocaltasks * sizeof(sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,"TEST B: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(sion_int64));
      return(1);
    }
    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST B: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }
    mapping_filenrs = (int *) malloc(nlocaltasks * sizeof(int));
    if (mapping_filenrs == NULL) {
      fprintf(stderr,"TEST B: cannot allocate mapping_filenrs size %lu, aborting ...\n",
	      (unsigned long) mapping_filenrs * sizeof(int));
      return(1);
    }
    mapping_lranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (mapping_lranks == NULL) {
      fprintf(stderr,"TEST B: cannot allocate mapping_lranks size %lu, aborting ...\n",
	      (unsigned long) mapping_lranks * sizeof(int));
      return(1);
    }

#if 1
    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;
#endif
    taskperfile=(nlocaltasks*size)/numfiles;
    if((nlocaltasks*size)%numfiles>0)  taskperfile++;
 
    for (i = 0; i < nlocaltasks; i++) {
      globalranks[i] = rank+i*size;  /* strided ordering */
      chunksizes[i]  = CHUNKSIZE;
      mapping_filenrs[i] = globalranks[i]/taskperfile; 
      mapping_lranks[i]  = globalranks[i]%taskperfile; 
    }

    for (t = 0; t < size; t++) {
      MPI_Barrier(MPI_COMM_WORLD);
      if(t==rank) {
	for (i = 0; i < nlocaltasks; i++) {
	  ONRANK printf("Test B: rank=%02d ltask=%0d globalrank=%02d, chunksize=%02d mapping_filenrs=%02d,mapping_lranks=%02d\n",
		 rank,i, globalranks[i], (int) chunksizes[i], mapping_filenrs[i],mapping_lranks[i]);
	}
      }

    }
    MPI_Barrier(MPI_COMM_WORLD);

    sid = sion_paropen_mapped_mpi("test_mappedB.out", "bw", &numfiles, MPI_COMM_WORLD, 
				  &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				  &fsblksize, &fp);
    if(sid>=0) {

      for(j=0;j<2;j++) {
        for (i = 0; i < nlocaltasks; i++) {
	  grank=globalranks[i];
	  sion_seek_fp(sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);
	  bwrote=sion_fwrite(buffer,1,CHUNKSIZE/2+grank,sid);
	  bytes_written=sion_get_bytes_written(sid);
	  for (c = 0,sum=0; c < bwrote; c++) sum=sum+buffer[c];
	  ONRANK printf("on rank %d/(%2d --> %2d): bytes_written=%lld (%d) blocksum=%ld\n",rank,i,grank,bytes_written,(int) bwrote,sum);
    	}
      }

      sion_parclose_mapped_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------ */
  /* TEST C: read from 4 task multiple sion file containing 16 tasks    */
  /* ------------------------------------------------------------------ */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int32  fsblksize = 100; 
    /* sion_int32  fsblksize = -1; */
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int        *mapping_filenrs=NULL;
    int        *mapping_lranks=NULL;
    int         numfiles    = 3;
    int         nlocaltasks = 5;
    int         sid;
    FILE       *fp;
    int         i,j,c,grank;

    char        buffer[BUFSIZE];
    long        sum; 
    size_t      bread;

    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST C: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }

    /* define access pattern */
    for (i = 0; i < nlocaltasks; i++) {
      globalranks[i] = rank+i*size;  /* strided ordering */
    }

    MPI_Barrier(MPI_COMM_WORLD);

    sid = sion_paropen_mapped_mpi("test_mappedB.out", "br", &numfiles, MPI_COMM_WORLD, 
				  &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				  &fsblksize, &fp);
    ONRANK printf("Test C: rank=%02d sid=%d after open\n",rank,sid);
    if(sid>=0) {

      for (i = 0; i < nlocaltasks; i++) {
	ONRANK printf("Test C: rank=%02d ltask=%0d globalrank=%02d, chunksize=%02d mapping_filenrs=%02d,mapping_lranks=%02d\n",
	       rank, i, globalranks[i], (int) chunksizes[i], mapping_filenrs[i],mapping_lranks[i]);
      }
      
      for(j=0;j<2;j++) {
	for (i = 0; i < nlocaltasks; i++) {
	  grank=globalranks[i];
	  sion_seek_fp(sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);
	    bread=sion_fread(buffer,1,CHUNKSIZE/2+grank,sid);
	    for (c = 0,sum=0; c < bread; c++) sum=sum+buffer[c];
	    ONRANK printf("on rank %d/(%2d --> %2d): bytes_read=%d  blocksum=%ld\n",rank,i,grank,(int)bread,sum);
	}
      }

      ONRANK printf("Test C: rank=%02d sid=%d before close\n",rank,sid);
      sion_parclose_mapped_mpi(sid);

    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST C multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------------------------------------------ */
  /* TEST D: read from 4 task multiple sion file containing 16 tasks, multiple open to tasks    */
  /* ------------------------------------------------------------------------------------------ */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int32  fsblksize = 100; 
    /* sion_int32  fsblksize = -1; */
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int        *mapping_filenrs=NULL;
    int        *mapping_lranks=NULL;
    int         numfiles    = 3;
    int         nlocaltasks = 8;
    int         sid;
    FILE       *fp;
    int         i,j,c,grank, start;

    char        buffer[BUFSIZE];
    long        sum; 
    size_t      bread;

    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST D: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }

    /* define access pattern */
    start=rank*4;
    for (i = 0; i < nlocaltasks; i++) {
      globalranks[i] = start+i;  /* overlapping block ordering */
    }

    MPI_Barrier(MPI_COMM_WORLD);

    sid = sion_paropen_mapped_mpi("test_mappedB.out", "br", &numfiles, MPI_COMM_WORLD, 
				  &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				  &fsblksize, &fp);
    ONRANK printf("Test D: rank=%02d sid=%d after open\n",rank,sid);
    if(sid>=0) {

      for (i = 0; i < nlocaltasks; i++) {
	ONRANK printf("Test D: rank=%02d ltask=%0d globalrank=%02d, chunksize=%02d mapping_filenrs=%02d,mapping_lranks=%02d\n",
	       rank, i, globalranks[i], (int) chunksizes[i], mapping_filenrs[i],mapping_lranks[i]);
      }
      
      for(j=0;j<2;j++) {
	for (i = 0; i < nlocaltasks; i++) {
	  grank=globalranks[i];
	  sion_seek_fp(sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);
	    bread=sion_fread(buffer,1,CHUNKSIZE/2+grank,sid);
	    for (c = 0,sum=0; c < bread; c++) sum=sum+buffer[c];
	    ONRANK printf("on rank %d/(%2d --> %2d): bytes_read=%d  blocksum=%ld\n",rank,i,grank,(int)bread,sum);
	}
      }

      ONRANK printf("Test D: rank=%02d sid=%d before close\n",rank,sid);
      sion_parclose_mapped_mpi(sid);

    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST D multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------------------------ */
  /* TEST E: read from 4 task multiple sion file containing 16 tasks, pre-computed distribution */
  /* ------------------------------------------------------------------------------------------ */
  {
#define BUFSIZE 10000 
#define CHUNKSIZE 1000
    sion_int32  fsblksize = 100; 
    /* sion_int32  fsblksize = -1; */
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int        *mapping_filenrs=NULL;
    int        *mapping_lranks=NULL;
    int         numfiles    = 3;
    int         nlocaltasks = -1;
    int         sid;
    FILE       *fp;
    int         i,j,c,grank;

    char        buffer[BUFSIZE];
    long        sum; 
    size_t      bread;

    MPI_Barrier(MPI_COMM_WORLD);

    sid = sion_paropen_mapped_mpi("test_mappedB.out", "br", &numfiles, MPI_COMM_WORLD, 
				  &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				  &fsblksize, &fp);
    ONRANK printf("Test E: rank=%02d sid=%d after open\n",rank,sid);
    if(sid>=0) {

      for (i = 0; i < nlocaltasks; i++) {
	ONRANK printf("Test E: rank=%02d ltask=%0d globalrank=%02d, chunksize=%02d mapping_filenrs=%02d,mapping_lranks=%02d\n",
	       rank, i, globalranks[i], (int) chunksizes[i], mapping_filenrs[i],mapping_lranks[i]);
      }
      
      for(j=0;j<2;j++) {
	for (i = 0; i < nlocaltasks; i++) {
	  grank=globalranks[i];
	  sion_seek_fp(sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);
	    bread=sion_fread(buffer,1,CHUNKSIZE/2+grank,sid);
	    for (c = 0,sum=0; c < bread; c++) sum=sum+buffer[c];
	    ONRANK printf("on rank %d/(%2d --> %2d): bytes_read=%d  blocksum=%ld\n",rank,i,grank,(int)bread,sum);
	}
      }

      ONRANK printf("Test E: rank=%02d sid=%d before close\n",rank,sid);
      sion_parclose_mapped_mpi(sid);

    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST D multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();

  return(0);
}
