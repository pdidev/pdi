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

#include "sion.h"

int main(int argc, char **argv)
{
  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  /* -------------------------- */
  /* TEST A: test with wrong parameters */
  /* -------------------------- */
  {
    sion_int32  fsblksize = 10;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 4;
    int         nfiles = 1;
    int         outsid;
    FILE       *outfp;

    outsid = sion_open("testA.out", "wb", &ntasks, &nfiles, NULL, &fsblksize, &globalranks, &outfp);
    if(outsid>=0) {
      sion_close(outsid);
    } else {
      fprintf(stderr,"TEST A: open returned %d\n",outsid);
    }

    chunksizes=NULL;
    outsid = sion_open("testA.out", "wb", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &outfp);
    if(outsid>=0) {
      sion_close(outsid);
    } else {
      fprintf(stderr,"TEST A: open returned %d\n",outsid);
    }
    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);
  }

  /* -------------------------- */
  /* TEST B: create an empty file */
  /* -------------------------- */
  {
    sion_int32  fsblksize = 10;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 4;
    int         nfiles = 1;
    int         outsid;
    FILE       *outfp;
    int         t;

    chunksizes = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,"TEST B: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(sion_int64));
      return(1);
    }
    globalranks = (int *) malloc(ntasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST B: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(int));
      return(1);
    }
    for(t=0;t<ntasks;t++) {
      chunksizes[t]=1000+t*200;
      globalranks[t]=t;
    }
    outsid = sion_open("testB.out", "wb", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &outfp);
    if(outsid>=0) {
      sion_close(outsid);
    } else {
      fprintf(stderr,"TEST B: open returned %d\n",outsid);
    }
    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);
  }

  /* ------------------------------------- */
  /* TEST C: create an file with some data */
  /* ------------------------------------- */
  {
#define BUFSIZE 10000  
    sion_int32  fsblksize = 10;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 4;
    int         nfiles = 1;
    int         outsid;
    FILE       *outfp;
    int         t,i,j,c;
    char        buffer[BUFSIZE];
    size_t      bwrote;
    long        sum; 

    chunksizes = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,"TEST C: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(sion_int64));
      return(1);
    }
    globalranks = (int *) malloc(ntasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST C: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(int));
      return(1);
    }
    for(t=0;t<ntasks;t++) {
      chunksizes[t]=1000+t*200;
      globalranks[t]=t;
    }
    outsid = sion_open("testC.out", "wb", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &outfp);
    if(outsid>=0) {
            
      for(j=0;j<2;j++) {
	
	for(t=0;t<ntasks;t++) {
	  for (i = 0; i < chunksizes[t]; i++) buffer[i] = 'A' + t;
	  sion_seek_fp(outsid,t,SION_CURRENT_BLK,SION_CURRENT_POS,&outfp);
          /* write nothing to check 0 as input */
	  bwrote=sion_fwrite(buffer,0,chunksizes[t]-9,outsid);
	  bwrote=sion_fwrite(buffer,1,0,outsid);
          /* write something */
	  bwrote=sion_fwrite(buffer,1,chunksizes[t]-9,outsid);
	  for (c = 0,sum=0; c < bwrote; c++) sum=sum+buffer[c];
	  printf("on rank %d: bytes_written=%d blocksum=%ld\n",t,(int)bwrote,sum);
	}
	
      }
      
      sion_close(outsid);
    } else {
      fprintf(stderr,"TEST C: open returned %d\n",outsid);
    }
    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);

  }

  /* ------------------------------------- */
  /* TEST C: read an file with some data   */
  /* ------------------------------------- */
  {
#define BUFSIZE 10000  
    sion_int32  fsblksize = 10;
    sion_int64 *chunksizes = NULL;
    int        *globalranks = NULL;
    int         ntasks = 4;
    int         nfiles = 1;
    int         outsid;
    FILE       *outfp;
    int         t,i,j,c;
    char        buffer[BUFSIZE];
    size_t      bread;
    long        sum; 

    chunksizes = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,"TEST C: cannot allocate chunksizes of size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(sion_int64));
      return(1);
    }
    globalranks = (int *) malloc(ntasks * sizeof(int));
    if (globalranks== NULL) {
      fprintf(stderr,"TEST C: cannot allocate globalranks size %lu, aborting ...\n",
	      (unsigned long) ntasks * sizeof(int));
      return(1);
    }
    for(t=0;t<ntasks;t++) {
      chunksizes[t]=0;
      globalranks[t]=0;
    }
    outsid = sion_open("testC.out", "rb", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &outfp);
    if(outsid>=0) {

      for(j=0;j<2;j++) {
	for(t=0;t<ntasks;t++) {
	  for (i = 0; i < chunksizes[t]; i++) buffer[i] = '-';
	  sion_seek_fp(outsid,t,SION_CURRENT_BLK,SION_CURRENT_POS,&outfp);
          /* read nothing to check 0 as input */
	  bread=sion_fread(buffer,0,chunksizes[t]-9,outsid);
	  bread=sion_fread(buffer,1,0,outsid);

	  bread=sion_fread(buffer,1,chunksizes[t]-9,outsid);
	  for (c = 0,sum=0; c < bread; c++) sum=sum+buffer[c];
	  printf("TESTC read: rank=%d chunksizes=%ld  bread=%d blocksum=%ld\n",t, (long) chunksizes[t], (int) bread, sum);
	}
      }

      sion_close(outsid);
    } else {
      fprintf(stderr,"TEST C: open returned %d\n",outsid);
    }
    if(chunksizes) free(chunksizes);
    if(globalranks) free(globalranks);

  }

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  
  return(0);
  
}
