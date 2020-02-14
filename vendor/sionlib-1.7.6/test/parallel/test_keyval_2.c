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
  /* TEST A: write a small empty keyval file                */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;

    sid = sion_paropen_mpi("test_keyval.out", "bw,keyval=inline", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {
      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST A (keyval) create\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#undef BUFSIZE
#undef CHUNKSIZE

  /* ---------------------------------------------------------------------- */
  /* TEST B: write a small  file containing multiople key entries           */
  /* ---------------------------------------------------------------------- */
  {
#define BUFSIZE 1000  
#define CHUNKSIZE 200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid, i;
    MPI_Comm    lcomm;
    FILE       *fp;
    const int   cycle_full = 26;
    size_t      bwrote, len;
    uint64_t    key;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=0;

    for (i = 0; i < BUFSIZE; i++) {
      buffer[i] = 'A' + ( i % cycle_full);
    }
    
    sid = sion_paropen_mpi("test_keys.out", "bw,keyval=inline", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {
      
      len=10;
      for(i=0;i<10;i++) {
	key=4711 + rank;
	bwrote=sion_fwrite_key(buffer+i, key, (size_t) 1, (size_t) len, sid);
	bytes_written+=bwrote;
      }

      for(i=0;i<10;i++) {
	key=5611 + i;
	bwrote=sion_fwrite_key(buffer, key, (size_t) 1, (size_t) len+i, sid);
	bytes_written+=bwrote;
      }

      printf("on rank %d: create bytes_written=%d\n",rank,(int) bytes_written);
      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
      
    }
  }
  
  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST B (normal) create\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#undef BUFSIZE
#undef CHUNKSIZE

  /* ------------------------------------------------------ */
  /* TEST B: open empty keyval file                         */
  /* ------------------------------------------------------ */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;

    sid = sion_paropen_mpi("test_keyval.out", "br,keyval=inline", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %d: sid           =%d\n",rank,sid);
    printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    printf("on rank %d: newfname      =%s\n",rank, newfname);

    printf("on rank %d: keyvalmode    =%s\n",rank, sion_keyval_type_to_str(sion_get_keyval_mode(sid)));

    if(sid>=0) {
      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST B read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* ----------------------------------------------- */
  /* TEST C: read keyvals with iterator              */
  /* ----------------------------------------------- */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp, *logfp;
    char        buffer[BUFSIZE];
    size_t      bread, len;
    char        logfilename[256];
    long        sum; 
    int         i,c;
    uint64_t    key;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_paropen_mpi("test_keys.out", "br,keyval=inline", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %d: sid           =%d\n",rank,sid);
    printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      /* iterator reset */
      printf("on rank %d: ITERATOR\n", rank);
      
      sprintf(logfilename, "keys_iter_l_rank%04d_out.log", rank);
      logfp=fopen(logfilename,"w");
      if (logfp == NULL) {
	fprintf(stderr, "cannot open outfile %s , aborting ...\n", logfilename);
	return (1);
      }

      sion_fread_key_iterator_reset(sid);
	
      /* loop over key-value blocks */
      while(sion_fread_key_iterator_next(sid,&key,&len)==SION_SUCCESS) {
	  
	/* printf("on rank %d: key = %ld\n", rank, (long) key); */
	
	bread = sion_fread_key(buffer,  key, 1, len, sid);
	if (bread != len) {
	  printf("on rank %d: end of data for key %d \n",rank, (int) key);
	  break;
	}

	/* log data */
	fprintf(logfp,"on rank %d: key=%6d len=%ld val=",rank, (int) key, (long) len);
	for (c = 0,sum=0; c < len; c++) {
	  fprintf(logfp,"%c",buffer[c]);
	  sum=sum+buffer[c];
	}
	fprintf(logfp,"\n");
      }

      fclose(logfp);
      
      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST D read iterative\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ----------------------------------------------- */
  /* TEST D: read keyvals with seek_key              */
  /* ----------------------------------------------- */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    char        buffer[BUFSIZE];
    size_t      bread, len;
    int         i;
    uint64_t    key;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_paropen_mpi("test_keys.out", "br,keyval=inline", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %d: sid           =%d\n",rank,sid);
    printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      /* seek 4th entry of key 4711+rank at position 0 in key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,4,0); bread = sion_fread_key(buffer,  key, 1, len, sid);
      buffer[len] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek 4th entry of key 4711+rank at position 1 in key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,4,1); bread = sion_fread_key(buffer,  key, 1, len-1, sid);
      buffer[len] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek position 82 of key 4711+rank */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_ABSOLUTE_POS,82); bread = sion_fread_key(buffer,  key, 1, len-2, sid);
      buffer[bread] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek position 82 of key 4711+rank */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_CURRENT_BLOCK,5); bread = sion_fread_key(buffer,  key, 1, len-5, sid);
      buffer[bread] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);


      /* seek position 42 of key 4711+rank, test return value of sion_fread_key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_ABSOLUTE_POS,42); bread = sion_fread_key(buffer,  key, 1, len, sid);
      buffer[bread] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek position 42 of key 4711+rank, test return value of sion_fread_key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_ABSOLUTE_POS,43); bread = sion_fread_key(buffer,  key, 4, 2, sid);
      buffer[bread*4] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) 4*2, (long) bread * 4, buffer);
      

    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST D read iterative\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ----------------------------------------------- */
  /* TEST E: test full scan                          */
  /* ----------------------------------------------- */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    char        buffer[BUFSIZE];
    size_t      bread, len;
    int         i, rc;
    uint64_t    key;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'Q' + rank;

    sid = sion_paropen_mpi("test_keys.out", "br,keyval=inline", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %d: sid           =%d\n",rank,sid);
    printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      rc=sion_key_full_scan(sid);
      printf("on rank %d: key_full_scan returns %d \n",rank, (int) rc);

      /* seek 4th entry of key 4711+rank at position 0 in key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,4,0); bread = sion_fread_key(buffer,  key, 1, len, sid);
      buffer[len] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek 4th entry of key 4711+rank at position 1 in key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,4,1); bread = sion_fread_key(buffer,  key, 1, len-1, sid);
      buffer[len] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek position 82 of key 4711+rank */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_ABSOLUTE_POS,82); bread = sion_fread_key(buffer,  key, 1, len-2, sid);
      buffer[bread] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek position 82 of key 4711+rank */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_CURRENT_BLOCK,5); bread = sion_fread_key(buffer,  key, 1, len-5, sid);
      buffer[bread] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek position 42 of key 4711+rank, test return value of sion_fread_key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_ABSOLUTE_POS,42); bread = sion_fread_key(buffer,  key, 1, len, sid);
      buffer[bread] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) len, (long) bread, buffer);

      /* seek position 42 of key 4711+rank, test return value of sion_fread_key */
      key=4711+rank; len=10;      
      sion_seek_key(sid,key,SION_ABSOLUTE_POS,43); bread = sion_fread_key(buffer,  key, 4, 2, sid);
      buffer[bread*4] = '\0';
      printf("on rank %d: key=%6d len=%ld bread=%ld val=%s\n",rank, (int) key, (long) 4*2, (long) bread * 4, buffer);
      

    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST E read iterative\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* ----------------------------------------------- */
  /* TEST F: test key list iterator                  */
  /* ----------------------------------------------- */
  if(1){
#define BUFSIZE 1000  
#define CHUNKSIZE 200
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    int         i, rc;
    uint64_t    key;
    sion_key_stat_t keystat;

    sid = sion_paropen_mpi("test_keys.out", "br,keyval=inline", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    printf("on rank %d: sid           =%d\n",rank,sid);
    printf("on rank %d: numfiles      =%d\n",rank,numfiles);
    printf("on rank %d: chunksize     =%d\n",rank,(int) chunksize);
    printf("on rank %d: globalrank    =%d\n",rank, globalrank);
    printf("on rank %d: newfname      =%s\n",rank, newfname);

    if(sid>=0) {

      rc=sion_key_full_scan(sid);
      printf("on rank %d: key_full_scan returns %d \n",rank, (int) rc);

      rc=sion_key_list_iterator_reset(sid);
      printf("on rank %d: key_list_iterator_reset returns %d \n",rank, (int) rc);

      i=0;
      while(sion_key_list_iterator_next(sid,&key)==SION_SUCCESS) {
	sion_key_get_stat(sid,key,&keystat);
	printf("on rank %d: key[%02d]=%ld: #blocks=%6d totalsize=%10ld\n",rank, i++, (long) key, (int) keystat.num_blocks, (long) keystat.total_size);
      }
      
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST F read iterative\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();

  return(0);
}
