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
#include <time.h>
#include <math.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>           /* For O_* constants */

#include "sion.h"

int main(int argc, char **argv)
{
  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  /* -------------------------- */
  /* TEST A: write key / value inline globally unique */
  /* -------------------------- */
  if(1){
#define BUFSIZE 100
    sion_int32  fsblksize = 10;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 4;
    int         nfiles = 1;
    int         outsid;
    FILE       *outfp, *logfp;
    int         t,i,j,nk,c;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    size_t      bwrote;
    long        sum;
    uint64_t    key;
    char        logfilename[256];
    const int   cycle_len = 5;
    const int   cycle_full = 26;
    const size_t len = 16;

    chunksizes = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,"TEST A: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(sion_int64));
      return 1;
    }
    globalranks = (int *) malloc(ntasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST A: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(int));
      return 1;
    }

    /* with these chunksizes the tasks 0 and 1 only use one block while */
    /* 2 and 3 use two blocks */
    for(t=0;t<ntasks;t++) {
      /* chunksizes[t] = 32 * (cycle_len + 2 - t); */
      chunksizes[t] = 32 * (3* cycle_len);
      globalranks[t] = t;
    }
    outsid = sion_open("testA.out", "wb,keyval=inline", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &outfp);
    if(outsid>=0) {

      for(t = 0; t < ntasks; t++) {
	sprintf(logfilename, "keysA_rank%04d_in.log", t);
	logfp = fopen(logfilename, "w");
	if (logfp == NULL) {
	  fprintf(stderr, "cannot open outfile %s , aborting ...\n", logfilename);
	  return 1;
	}

	sum = 0;
	sion_seek_fp(outsid,t,SION_CURRENT_BLK,SION_CURRENT_POS,&outfp);
	for(j = 0;  j < cycle_len; j++) {
	  key=4711 + t * cycle_len + j;

	  for(nk = 0;  nk < 3; nk++) {
	  
	    fprintf(logfp,"on rank %d: key=%6d len=%ld val=", t, (int) key, (long) len);
	    for (i = 0; i < len; i++) {
	      buffer[i] = 'A' + ( (key - 4711) % cycle_full );
	    }
	    bwrote=sion_fwrite_key(buffer, key, (size_t) 1, (size_t) len, outsid);
	    for (c = 0; c < bwrote; c++) {
	      sum += buffer[c];
	      fprintf(logfp, "%c", buffer[c]);
	    }
	    fprintf(logfp, "\n");

	  }
	}
	fclose(logfp);

	bytes_written = sion_get_bytes_written(outsid);
	printf("on rank %d: bytes_written=%d blocksum=%ld\n", t,
	       (int)bytes_written, sum);
      }

      sion_close(outsid);
    } else {
      fprintf(stderr,"TEST A: sion_open returned %d\n",outsid);
    }
    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);

  }

  /* ------------------------------------------------------------------ */
  /* TEST B: read key / value inline in the same order they are written */
  /* ------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 100
    sion_int32  fsblksize = 0;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 0;
    int         nfiles = 0;
    int         insid;
    FILE       *infp, *logfp;
    int         t,j,i,c;
    char        buffer[BUFSIZE];
    size_t      bread;
    long        sum;
    uint64_t    key;
    char        logfilename[256];
    const int   cycle_len = 5;
    const size_t len = 16;
 
    insid = sion_open("testA.out", "rb,keyval=inline", &ntasks, &nfiles,
		      &chunksizes, &fsblksize, &globalranks, &infp);

    if(insid>=0) {

      for(t = 0; t < ntasks; t++) {
	sprintf(logfilename, "keysA_rank%04d_out.log", t);
	logfp = fopen(logfilename, "w");
	if (logfp == NULL) {
	  fprintf(stderr, "cannot open outfile %s , aborting ...\n", logfilename);
	  return 1;
	}

	sum = 0;
	printf("on rank %d: seek to task\n",t);
	sion_seek_fp(insid, t, 0, 0, &infp);
	for(j = 0;  j < cycle_len; j++) {
	  key=4711 + t * cycle_len + j;

	  for (i = 0; i < 15; i++) {

	    /* find key */
	    bread = sion_fread_key(buffer, key, 1, len, insid);
	  
	    if (bread != len) {
	      printf("on rank %d: end of data for key %d \n",t, (int) key);
	      break;
	    }

	    fprintf(logfp,"on rank %d: key=%6d len=%ld val=", t, (int) key, (long) len);
	    for (c = 0; c < bread; c++) {
	      sum = sum + buffer[c];
	      fprintf(logfp, "%c", buffer[c]);
	    }
	    fprintf(logfp, "\n");
	  }
	}

	printf("on rank %d: blocksum=%ld\n", t, sum);
	fclose(logfp);
      }

      sion_close(insid);
    } else {
      fprintf(stderr,"TEST B: sion_open returned %d\n",insid);
    }

    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);

  }


  /* -------------------------- */
  /* TEST C: write key / value inline, only unique per task  */
  /* -------------------------- */
  if(1){
#define BUFSIZE 100
    sion_int32  fsblksize = 10;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 4;
    int         nfiles = 1;
    int         outsid;
    FILE       *outfp, *logfp;
    int         t,i,j,nk,c;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=-1;
    size_t      bwrote;
    long        sum;
    uint64_t    key;
    char        logfilename[256];
    const int   cycle_len = 5;
    const int   cycle_full = 26;
    const size_t len = 16;

    chunksizes = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,"TEST C: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(sion_int64));
      return 1;
    }
    globalranks = (int *) malloc(ntasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST C: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(int));
      return 1;
    }

    /* with these chunksizes the tasks 0 and 1 only use one block while */
    /* 2 and 3 use two blocks */
    for(t=0;t<ntasks;t++) {
      chunksizes[t] = 32 * (cycle_len + 2 - t);
      globalranks[t] = t;
    }
    outsid = sion_open("testC.out", "wb,keyval=inline", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &outfp);
    if(outsid>=0) {

      for(t = 0; t < ntasks; t++) {
	sprintf(logfilename, "keysC_rank%04d_in.log", t);
	logfp = fopen(logfilename, "w");
	if (logfp == NULL) {
	  fprintf(stderr, "cannot open outfile %s , aborting ...\n", logfilename);
	  return 1;
	}

	sum = 0;
	sion_seek_fp(outsid,t,SION_CURRENT_BLK,SION_CURRENT_POS,&outfp);
	for(j = 0;  j < cycle_len; j++) {
	  key=4711 + j;

	  for(nk = 0;  nk < 3; nk++) {
	    
	    fprintf(logfp,"on rank %d: key=%6d len=%ld val=", t, (int) key, (long) len);
	    for (i = 0; i < len; i++) {
	      buffer[i] = 'A' + ( (key - 4711 + t * cycle_len) % cycle_full );
	    }
	    bwrote=sion_fwrite_key(buffer, key, (size_t) 1, (size_t) len, outsid);
	    for (c = 0; c < bwrote; c++) {
	      sum += buffer[c];
	      fprintf(logfp, "%c", buffer[c]);
	    }
	    fprintf(logfp, "\n");

	  }
	}
	fclose(logfp);

	bytes_written = sion_get_bytes_written(outsid);
	printf("on rank %d: bytes_written=%d blocksum=%ld\n", t,
	       (int)bytes_written, sum);
      }

      sion_close(outsid);
    } else {
      fprintf(stderr,"TEST C: sion_open returned %d\n",outsid);
    }
    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);

  }

  /* ------------------------------------------------------------------ */
  /* TEST D: read key / value inline in the same order they are written */
  /* ------------------------------------------------------------------ */
  if(1){
#define BUFSIZE 100
    sion_int32  fsblksize = 0;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 0;
    int         nfiles = 0;
    int         insid;
    FILE       *infp, *logfp;
    int         t,j,i,c;
    char        buffer[BUFSIZE];
    size_t      bread;
    long        sum;
    uint64_t    key;
    char        logfilename[256];
    const int   cycle_len = 5;
    const size_t len = 16;
 
    insid = sion_open("testC.out", "rb,keyval=inline", &ntasks, &nfiles,
		      &chunksizes, &fsblksize, &globalranks, &infp);

    if(insid>=0) {

      for(t = 0; t < ntasks; t++) {
	sprintf(logfilename, "keysC_rank%04d_out.log", t);
	logfp = fopen(logfilename, "w");
	if (logfp == NULL) {
	  fprintf(stderr, "cannot open outfile %s , aborting ...\n", logfilename);
	  return 1;
	}

	sum = 0;
	sion_seek_fp(insid, t, 0, 0, &infp);

	for(j = 0;  j < cycle_len; j++) {
	  key=4711 + (j) % cycle_len;

	  for (i = 0; i < 15; i++) {
	    
	    /* find key */
	    bread = sion_fread_key(buffer, key, 1, len, insid);
	    
	    if (bread != len) {
	      printf("on rank %d: end of data for key %d \n",t, (int) key);
	      break;
	    }

	    fprintf(logfp,"on rank %d: key=%6d len=%ld val=", t, (int) key, (long) len);
	    
	    for (c = 0; c < bread; c++) {
	      sum = sum + buffer[c];
	      fprintf(logfp, "%c", buffer[c]);
	    }
	    fprintf(logfp, "\n");
	  }
	}
     
	printf("on rank %d: blocksum=%ld\n", t, sum);
	fclose(logfp);
      }
      
      sion_close(insid);
    } else {
      fprintf(stderr,"TEST D: sion_open returned %d\n",insid);
    }
    
    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);
    
  }


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */

  return 0;

}
