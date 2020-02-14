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

#define SAFE_FREE(ptr, null) {if (ptr) {free(ptr); ptr = null;}}

#define BUFSIZE 10000 
#define CHUNKSIZE 1000

#include "test_genapi_INC_MPI_api.c"

int _read_mappedB ( char *test,
		    char * filename,
		    int aid,
		    int rank,
		    int size,
		    _sample_api_commdata *gcomm,
		    int nlocaltasks,
		    int **globalranks
		    ) {

  int rc=SION_SUCCESS;
  sion_int32  fsblksize = 100;
  sion_int64 *chunksizes = NULL;
  int         numfiles    = 3;
  int         sid;
  FILE       *fp;
  int         i,j,c,grank;
  int        *mapping_filenrs=NULL;
  int        *mapping_lranks=NULL;
  char        buffer[BUFSIZE];
  long        sum;
  size_t      bread;


  MPI_Barrier(MPI_COMM_WORLD);

  sid = sion_generic_paropen_mapped(aid,filename, "br", &numfiles, gcomm, rank, size,
				    &nlocaltasks, globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				    &fsblksize, &fp);

  
  ONRANK printf("Test %s: rank=%02d sid=%d after open\n",test,rank,sid);

  if(sid>=0) {

    for (i = 0; i < nlocaltasks; i++) {
      ONRANK printf("Test %s: rank=%02d ltask=%0d globalrank=%02d, chunksize=%02d mapping_filenrs=%02d,mapping_lranks=%02d\n", test,
		       rank, i, (*globalranks)[i], (int) chunksizes[i], mapping_filenrs[i],mapping_lranks[i]);
    }

    for(j=0;j<2;j++) {
      for (i = 0; i < nlocaltasks; i++) {
	grank=(*globalranks)[i];
	sion_seek_fp(sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);
	bread=sion_fread(buffer,1,CHUNKSIZE/2+grank,sid);
	for (c = 0,sum=0; c < bread; c++) sum=sum+buffer[c];
	if(bread>0) {
	  ONRANK printf("Test %s: on rank %d/(%2d --> %2d): bread=%4d blocksum=%4d, data_from=%2d %s\n",test,rank,j,grank, 
			(int) bread, (int) sum, (int) ( (sum/bread) - (int) 'A'), (( (sum/bread) - (int) 'A') == grank)?" --> OK":"--> ERROR (>on rank 0:<)");
	}
      }
    }

    ONRANK printf("Test %s: rank=%02d sid=%d before close\n",test,rank,sid);
    sion_generic_parclose_mapped(sid);

  } else {
    fprintf(stderr, "Test %s: on rank %d: error sid = %d\n",test,rank,sid);
    rc=SION_NOT_SUCCESS;
  }

  MPI_Barrier(MPI_COMM_WORLD);

  SAFE_FREE(chunksizes, NULL);
  SAFE_FREE(mapping_filenrs, NULL);
  SAFE_FREE(mapping_lranks, NULL);

  return(rc);
}

int _read_mappedB_dup ( char *test,
			char * filename,
			int aid,
			int rank,
			int size,
			_sample_api_commdata *gcomm,
			int nlocaltasks,
			int **globalranks
			) {

  int rc=SION_SUCCESS;
  sion_int32  fsblksize = 100;
  sion_int64 *chunksizes = NULL;
  int         numfiles    = 3;
  int         sid, new_sid;
  FILE       *fp;
  int         i,j,c,grank;
  int        *mapping_filenrs=NULL;
  int        *mapping_lranks=NULL;
  char        buffer[BUFSIZE];
  long        sum;
  size_t      bread;


  MPI_Barrier(MPI_COMM_WORLD);

  sid = sion_generic_paropen_mapped(aid,filename, "br", &numfiles, gcomm, rank, size,
				    &nlocaltasks, globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				    &fsblksize, &fp);

  
  ONRANK printf("Test %s: rank=%02d sid=%d after open\n",test,rank,sid);

  if(sid>=0) {

    /* dup sion file pointer  */
    new_sid=sion_dup(sid,SION_DUP_ALL,0,0);
    printf("on rank %d: dup, new_sid       =%d\n",rank,new_sid);
    

    for (i = 0; i < nlocaltasks; i++) {
      ONRANK printf("Test %s: rank=%02d ltask=%0d globalrank=%02d, chunksize=%02d mapping_filenrs=%02d,mapping_lranks=%02d\n", test,
		       rank, i, (*globalranks)[i], (int) chunksizes[i], mapping_filenrs[i],mapping_lranks[i]);
    }

    for(j=0;j<2;j++) {
      for (i = 0; i < nlocaltasks; i++) {
	grank=(*globalranks)[i];
	sion_seek_fp(new_sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);
	bread=sion_fread(buffer,1,CHUNKSIZE/2+grank,new_sid);
	for (c = 0,sum=0; c < bread; c++) sum=sum+buffer[c];
	if(bread>0) {
	  ONRANK printf("Test %s: on rank %d/(%2d --> %2d): bread=%4d blocksum=%4d, data_from=%2d %s\n",test,rank,j,grank, 
			   (int) bread, (int) sum, (int) ( (sum/bread) - (int) 'A'), (( (sum/bread) - (int) 'A') == grank)?" --> OK":"--> ERROR (>on rank 0:<)");
	}
      }
    }

    /* destry dup sion file pointer */
    printf("on rank %d: dedup(%d)\n",rank,new_sid);
    sion_dedup(new_sid);
    
    ONRANK printf("Test %s: rank=%02d new_sid=%d before close\n",test,rank,sid);
    sion_generic_parclose_mapped(sid);

  } else {
    fprintf(stderr, "Test %s: on rank %d: error sid = %d\n",test,rank,sid);
    rc=SION_NOT_SUCCESS;
  }

  MPI_Barrier(MPI_COMM_WORLD);

  SAFE_FREE(chunksizes, NULL);
  SAFE_FREE(mapping_filenrs, NULL);
  SAFE_FREE(mapping_lranks, NULL);

  return(rc);
}

int main(int argc, char **argv)
{
  int  rank, size;
  int aid;
  _sample_api_commdata gcomm;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* -------------------------- */
  /* TEST 0: define new API     */
  /* -------------------------- */
  {
    aid=sion_generic_create_api("SampleAPI");
    ONRANK printf("on rank %d: new API: aid=%d\n",rank,aid);  

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

    fflush(stderr);
    fflush(stdout);
    MPI_Barrier(MPI_COMM_WORLD);
    ONRANK printf("on rank %d: END of TEST 0\n",rank);
   
  }

  /* ------------------------------------------------------------------- */
  /* TEST A: create from 4 task one empty sion file containing 20 tasks  */
  /* ------------------------------------------------------------------- */
  if(1){
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

    for (i = 0; i < nlocaltasks; i++) {
      globalranks[i] = rank*nlocaltasks+i;
      chunksizes[i]  = CHUNKSIZE*2;
      mapping_filenrs[i] = 0; /* one file */
      mapping_lranks[i]  = globalranks[i]; /* one file */
    }

    sid = sion_generic_paropen_mapped(aid,"test_mappedA.out", "bw", &numfiles, &gcomm, rank, size,
				      &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				      &fsblksize, &fp);
    if(sid>=0) {

      sion_generic_parclose_mapped(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }

    SAFE_FREE(chunksizes, NULL);
    SAFE_FREE(globalranks, NULL);
    SAFE_FREE(mapping_filenrs, NULL);
    SAFE_FREE(mapping_lranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST A write mapped\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ---------------------------------------------------------------------------- */
  /* TEST B: create from 4 task multiple sion file containing 20 tasks in 3 files */
  /* ---------------------------------------------------------------------------- */
  {
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

    sid = sion_generic_paropen_mapped(aid,"test_mappedB.out", "bw", &numfiles, &gcomm, rank, size,
				      &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				      &fsblksize, &fp);
    if(sid>=0) {

      for(j=0;j<2;j++) {
        for (i = 0; i < nlocaltasks; i++) {
	  grank=globalranks[i];
	  sion_seek_fp(sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);	
	  for (c = 0; c < BUFSIZE; c++) buffer[c] = 'A' + grank;
	  bwrote=sion_fwrite(buffer,1,CHUNKSIZE/2+grank,sid);
	  bytes_written=sion_get_bytes_written(sid);
	  for (c = 0,sum=0; c < bwrote; c++) sum=sum+buffer[c];
	  if(bwrote>0) {
	    ONRANK printf("on rank %d/(%2d --> %2d): bytes_written=%4d (%4d) blocksum=%4d, data_from=%2d\n",rank,j,grank, 
		   (int) bytes_written, (int) bwrote, (int) sum, (int) ( (sum/bwrote) - (int) 'A'));
	  } else {
	    ONRANK printf("on rank %d/(%2d --> %2d): bytes_written=%4d (%4d) blocksum=%4d\n",rank,j,grank, 
		   (int) bytes_written, (int) bwrote, (int) sum);
	  }
    	}
      }

      sion_generic_parclose_mapped(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }

    SAFE_FREE(chunksizes, NULL);
    SAFE_FREE(globalranks, NULL);
    SAFE_FREE(mapping_filenrs, NULL);
    SAFE_FREE(mapping_lranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST B multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* globalranks for test_mappedB file are in range from 0..19 and distributed as follows: ([grank -> (file,lrank)])
     
     [00000 -> (f000,t000)] [00001 -> (f000,t001)] [00002 -> (f000,t002)]
     [00003 -> (f000,t003)] [00004 -> (f000,t004)] [00005 -> (f000,t005)]
     [00006 -> (f000,t006)] 
     
     [00007 -> (f001,t000)] [00008 -> (f001,t001)] [00009 -> (f001,t002)]
     [00010 -> (f001,t003)] [00011 -> (f001,t004)] [00012 -> (f001,t005)]
     [00013 -> (f001,t006)] 
     
     [00014 -> (f002,t000)] [00015 -> (f002,t001)] [00016 -> (f002,t002)]
     [00017 -> (f002,t003)] [00018 -> (f002,t004)] [00019 -> (f002,t005)]
     
  */



  /* ---------------------------------------------------------------------------- */
  /* TEST C: create from 4 task multiple sion file containing 20 tasks in 5 files */
  /* ---------------------------------------------------------------------------- */
  if(1){
    sion_int32  fsblksize = 100; 
    /* sion_int32  fsblksize = -1; */
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int        *mapping_filenrs;
    int        *mapping_lranks;
    int         numfiles    = 5;
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
      fprintf(stderr,"TEST C: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(sion_int64));
      return(1);
    }
    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST C: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }
    mapping_filenrs = (int *) malloc(nlocaltasks * sizeof(int));
    if (mapping_filenrs == NULL) {
      fprintf(stderr,"TEST C: cannot allocate mapping_filenrs size %lu, aborting ...\n",
	      (unsigned long) mapping_filenrs * sizeof(int));
      return(1);
    }
    mapping_lranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (mapping_lranks == NULL) {
      fprintf(stderr,"TEST C: cannot allocate mapping_lranks size %lu, aborting ...\n",
	      (unsigned long) mapping_lranks * sizeof(int));
      return(1);
    }

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
	  ONRANK printf("Test C: rank=%02d ltask=%0d globalrank=%02d, chunksize=%02d mapping_filenrs=%02d,mapping_lranks=%02d\n",
		 rank,i, globalranks[i], (int) chunksizes[i], mapping_filenrs[i],mapping_lranks[i]);
	}
      }

    }
    MPI_Barrier(MPI_COMM_WORLD);

    sid = sion_generic_paropen_mapped(aid,"test_mappedC.out", "bw", &numfiles, &gcomm, rank, size,
				      &nlocaltasks, &globalranks, &chunksizes, &mapping_filenrs, &mapping_lranks,
				      &fsblksize, &fp);
    if(sid>=0) {

      for(j=0;j<2;j++) {
        for (i = 0; i < nlocaltasks; i++) {
	  grank=globalranks[i];
	  sion_seek_fp(sid,grank,SION_CURRENT_BLK,SION_CURRENT_POS, &fp);	
	  for (c = 0; c < BUFSIZE; c++) buffer[c] = 'A' + grank;
	  bwrote=sion_fwrite(buffer,1,CHUNKSIZE/2+grank,sid);
	  bytes_written=sion_get_bytes_written(sid);
	  for (c = 0,sum=0; c < bwrote; c++) sum=sum+buffer[c];
	  if(bwrote>0) {
	    ONRANK printf("on rank %d/(%2d --> %2d): bytes_written=%4d (%4d) blocksum=%4d, data_from=%2d\n",rank,j,grank, 
		   (int) bytes_written, (int) bwrote, (int) sum, (int) ( (sum/bwrote) - (int) 'A'));
	  } else {
	    ONRANK printf("on rank %d/(%2d --> %2d): bytes_written=%4d (%4d) blocksum=%4d\n",rank,j,grank, 
		   (int) bytes_written, (int) bwrote, (int) sum);
	  }
    	}
      }

      sion_generic_parclose_mapped(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }

    SAFE_FREE(chunksizes, NULL);
    SAFE_FREE(globalranks, NULL);
    SAFE_FREE(mapping_filenrs, NULL);
    SAFE_FREE(mapping_lranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST C multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* globalranks for test_mappedC file are in range from 0..19 and distributed as follows: ([grank -> (file,lrank)])
     
        [00000 -> (f000,t000)] [00001 -> (f000,t001)] [00002 -> (f000,t002)]
        [00003 -> (f000,t003)]
	
	[00004 -> (f001,t000)] [00005 -> (f001,t001)] [00006 -> (f001,t002)]
	[00007 -> (f001,t003)]

	[00008 -> (f002,t000)] [00009 -> (f002,t001)] [00010 -> (f002,t002)]
        [00011 -> (f002,t003)]

	[00012 -> (f003,t000)] [00013 -> (f003,t001)] [00014 -> (f003,t002)]
        [00015 -> (f003,t003)]

	[00016 -> (f004,t000)] [00017 -> (f004,t001)] [00018 -> (f004,t002)]
	[00019 -> (f004,t003)]
     
  */

  /* ------------------------------------------------------------------ */
  /* TEST D: read from 4 task multiple sion file containing 20 tasks    */
  /* ------------------------------------------------------------------ */
  if(1) {
    int        *globalranks = NULL;
    int         nlocaltasks = 5;
    int         i;

    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST D: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }
    /* define access pattern: strided ordering */
    for (i = 0; i < nlocaltasks; i++) {
      globalranks[i] = rank+i*size;
    }
    _read_mappedB("D","test_mappedB.out",aid,rank,size,&gcomm,nlocaltasks,&globalranks);

    _read_mappedB_dup("D","test_mappedB.out",aid,rank,size,&gcomm,nlocaltasks,&globalranks);

    SAFE_FREE(globalranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST D multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ------------------------------------------------------------------------------------------ */
  /* TEST E: read from 4 task multiple sion file containing 20 tasks, multiple open to tasks    */
  /* ------------------------------------------------------------------------------------------ */
  if(1){
    int        *globalranks = NULL;
    int         nlocaltasks = 8;
    int         i,start;

    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST E: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }

    /* define access pattern */
    start=rank*4;
    for (i = 0; i < nlocaltasks; i++) {
      globalranks[i] = start+i;  /* overlapping block ordering */
    }

    _read_mappedB("E","test_mappedB.out",aid,rank,size,&gcomm,nlocaltasks,&globalranks);

    SAFE_FREE(globalranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST E multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ------------------------------------------------------------------------------------------ */
  /* TEST F: read from 4 task multiple sion file containing 20 tasks, pre-computed distribution */
  /* ------------------------------------------------------------------------------------------ */
  if(1){
    int        *globalranks = NULL;
    int         nlocaltasks = -1;

    _read_mappedB("F","test_mappedB.out",aid,rank,size,&gcomm,nlocaltasks,&globalranks);

    SAFE_FREE(globalranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST F multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ----------------------------------------------------------------------------------------------------------- */
  /* TEST G: read from 4 task multiple sion file containing 20 tasks, multiple open to tasks, no globalrank = 0  */
  /* ----------------------------------------------------------------------------------------------------------- */
  if(1){
    int        *globalranks = NULL;
    int         nlocaltasks = 8;

    if(rank==0) nlocaltasks=1;
    if(rank==1) nlocaltasks=1;
    if(rank==2) nlocaltasks=1;
    if(rank==3) nlocaltasks=1;

    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST G: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }

    /* test scenario: 
       - no rank opens lrank 0 of file 0 and 1  
       - no rank opens lranks of file 2 */
    if(rank==0) {
      globalranks[0]=1;
    }
    if(rank==1) {
      globalranks[0]=8;
    }
    if(rank==2) {
      globalranks[0]=11;
    }
    if(rank==3) {
      globalranks[0]=12;
    }

    _read_mappedB("G","test_mappedB.out",aid,rank,size,&gcomm,nlocaltasks,&globalranks);

    SAFE_FREE(globalranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST G multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ----------------------------------------------------------------------------------------------------------- */
  /* TEST H: read from 4 task multiple sion file containing 20 tasks, no task reads data                         */
  /* ----------------------------------------------------------------------------------------------------------- */
  if(1){
    int        *globalranks = NULL;
    int         nlocaltasks = 0;

    _read_mappedB("H","test_mappedB.out",aid,rank,size,&gcomm,nlocaltasks,&globalranks);

    SAFE_FREE(globalranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST H multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ----------------------------------------------------------------------------------------------------------- */
  /* TEST I: read from 4 task multiple sion file containing 20 tasks, multiple open to tasks, no globalrank = 0  */
  /* ----------------------------------------------------------------------------------------------------------- */
  if(1){
    int        *globalranks = NULL;
    int         nlocaltasks = 8;

    if(rank==0) nlocaltasks=1;
    if(rank==1) nlocaltasks=1;
    if(rank==2) nlocaltasks=1;
    if(rank==3) nlocaltasks=1;

    globalranks = (int *) malloc(nlocaltasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST I: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) nlocaltasks * sizeof(int));
      return(1);
    }

    /* test scenario: 
       - no rank opens lrank 0 of file 0 and 1  
       - no rank opens lranks of file 2 */
    if(rank==0) {
      globalranks[0]=1;
    }
    if(rank==1) {
      globalranks[0]=8;
    }
    if(rank==2) {
      globalranks[0]=11;
    }
    if(rank==3) {
      globalranks[0]=12;
    }

    _read_mappedB("I","test_mappedC.out",aid,rank,size,&gcomm,nlocaltasks,&globalranks);

    SAFE_FREE(globalranks, NULL);
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  ONRANK printf("on rank %d: END of TEST I multiple files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  sion_generic_free_api(aid);

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();

  return(0);
}
#undef SAFE_FREE
