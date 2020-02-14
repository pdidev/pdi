/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2009                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*
 * \file sion_pepc.c
 *
 * \brief example for a serial converter tool to convert  data between 
 *  PEPC particle data and binary data in sionfile 
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "sion.h"

#define FILENAME_LENGTH 1024
#define MAXNULLCNT 1000

#define MB *1024*1024

void usage(char *name) {
  fprintf(stderr, "Usage: %s options <infile> <outsionfn>\n\nwith the following optional options (default values in parenthesis):\n\n",name);

  fprintf(stderr, "  [-Q <fsblksize>]       filessystem blocksize for new sion file in MB    (default from input file)\n");
  fprintf(stderr, "  [-q <fsblksize>]       filessystem blocksize for new sion file in bytes \n");
  fprintf(stderr, "  [-S <fsblksize>]       local data size on each rank in MB               (default from input file)\n");
  fprintf(stderr, "  [-s <fsblksize>]       local data size on each rank in bytes\n");
  fprintf(stderr, "  [-n <ntasks>]          number of tasks, when generating sion file\n");
  fprintf(stderr, "  [-g]                   generate sion file from ascii parts dump \n");
  fprintf(stderr, "  [-v]                   verbose mode \n");
  exit(1);
}


int generatesionfilefromascii (
			       char        *infilename,
			       char        *outfilename,
			       int          opt_ntasks,
			       long         opt_nparts,
			       int          opt_fsblocksize,
			       int          opt_verbose
			       );

int generateasciifromsionfile (
			       char        *infilename,
			       char        *outfilename,
			       int          opt_verbose
			       );

int main(int argc, char **argv)
{
  char infilename[FILENAME_LENGTH];
  char outfilename[FILENAME_LENGTH];
  int i;

  /* options */
  int        opt_verbose=0;
  sion_int64 opt_localsize=-1;
  int        opt_fsblocksize=-1;
  int        opt_ntasks=-1;
  long       opt_nparts=-1;
  int        opt_generatesionfilefromascii=0;
  int        opt_generateasciifromsionfile=0;

  /* for file infomation */

  /* parse command line */
  i=1;
  if (argc < 3)
    usage(argv[0]);

  while( i < argc ) {
    if( argv[i][0] == '-' ) {
      switch( argv[i][1] ) {
	case 's':
	  opt_localsize = atoi(argv[++i]);
	  break;
	case 'S':
	  opt_localsize = atoi(argv[++i]) MB;
	  break;
	case 'q':
	  opt_fsblocksize = atoi(argv[++i]);
	  break;
	case 'Q':
	  opt_fsblocksize = atoi(argv[++i]) MB;
	  break;
	case 'n':
	  opt_ntasks = atoi(argv[++i]);
	  break;
	case 'p':
	  opt_nparts = atol(argv[++i]);
	  break;
      case 'g':
	opt_generatesionfilefromascii++;
	break;
      case 'a':
	opt_generateasciifromsionfile++;
	break;
      case 'v':
	opt_verbose++;
	break;
      default:
	usage(argv[0]);
      }
    }
    i++;
  }

  strcpy(infilename,argv[argc-2]);
  strcpy(outfilename,argv[argc-1]);

  printf("siondump: infilename:                %-30s\n",infilename);
  printf("siondump: outfilename:               %-30s\n",outfilename);
  printf("siondump: ntasks:                    %-30d\n",opt_ntasks);
  printf("siondump: nparts:                    %-30ld\n",opt_nparts);


  if(opt_generatesionfilefromascii)   {
    printf("siondump: generate sionfile from ascii:               %-30d\n",opt_generatesionfilefromascii);
    generatesionfilefromascii(infilename,outfilename,opt_ntasks,opt_nparts,opt_fsblocksize,opt_verbose);
  }

  if(opt_generateasciifromsionfile)   {
    printf("siondump: generate ascii from sionfile:               %-30d\n",opt_generateasciifromsionfile);
    generateasciifromsionfile(infilename,outfilename,opt_verbose);
  }



  return(0);
}


int generatesionfilefromascii (
			       char        *infilename,
			       char        *outfilename,
			       int          opt_ntasks,
			       long         opt_nparts,
			       int          opt_fsblocksize,
			       int          opt_verbose
			       ) {
  
  sion_int64 *chunksizes=NULL;
  int        *globalranks=NULL;
  int         ntasks, nfiles;

  FILE *fp,*infp;
  int rank, nullcount, rc;
  int sid,size,blocks;
  long  numparts,nextparts,np,gnp; 
  long  partpertask=0;
  long  partpertask_remain=0;
  double *localbuffer=NULL;
  double doubleval[14];
  sion_int32 fsblksize;
  long wrote,left;
  

  /* scan Input file for number of particles */
  if(opt_nparts<=0) {
    printf("siondump: scanning: %s ...\n",infilename);
    numparts=0;
    infp=fopen(infilename,"r");
    while(!feof(infp)) {
      rc=fscanf(infp,"%le %le %le %le %le %le %le %le %le %le %le %le %le %le",
		&doubleval[0],&doubleval[1],&doubleval[2],&doubleval[3],&doubleval[4],&doubleval[5],
		&doubleval[6],&doubleval[7],&doubleval[8],&doubleval[9],&doubleval[10],
		&doubleval[11],&doubleval[12],&doubleval[13]);
      numparts++;
      if(numparts%1000000==0) {
	printf("siondump:              ... found %d particles ... rc=%d (%le %le %le)\n",
               numparts,rc,doubleval[0],doubleval[1],doubleval[2]);
      }
    }
    fclose(infp);
    printf("siondump:              ... found %d particles\n",numparts);
  } else {
    numparts=opt_nparts;
    printf("siondump:              %d particles available\n",numparts);
  }

  /* check parameters */
  if(numparts<=0) {
    return(_sion_errorprint(-1,_SION_ERROR_ABORT,"number of tasks not defined (-n %d), aborting ...\n",
			    opt_ntasks));
  }
  if(opt_ntasks<=0) {
    return(_sion_errorprint(-1,_SION_ERROR_ABORT,"number of tasks not defined (-n %d), aborting ...\n",
			    opt_ntasks));
  }

  /* calculate parts per task */
  ntasks=opt_ntasks;
  partpertask=(long) ( (double) numparts / (double) ntasks) ;
  partpertask_remain=numparts-partpertask*ntasks;
  
  printf("siondump: partpertask:               %-30ld\n",partpertask);
  printf("siondump: partpertask_remain:        %-30ld\n",partpertask_remain);

  /* allocate fields */
  chunksizes = (sion_int64 *) malloc(ntasks*sizeof(sion_int64));
  if (chunksizes==NULL) {
    return(_sion_errorprint(-1,_SION_ERROR_ABORT,"cannot allocate memory of size %lu (chunksizes), aborting ...\n",
			    (unsigned long) ntasks*sizeof(sion_int64)));
  }
  globalranks = (int *) malloc(ntasks*sizeof(int));
  if (globalranks==NULL) {
    return(_sion_errorprint(-1,_SION_ERROR_ABORT,"cannot allocate memory of size %lu (globalranks), aborting ...\n",
			    (unsigned long) ntasks*sizeof(int)));
  }

  /* initalize fields and other paramters of sion_open */
  for(rank=0;rank<ntasks;rank++) {
    chunksizes[rank]=14*sizeof(double)*(partpertask + ((rank<partpertask_remain)?1:0));
    globalranks[rank]=rank;
  }
  fsblksize=1;
  if(opt_fsblocksize!=-1)  fsblksize=opt_fsblocksize;

  /* open sion file */
  sid=sion_open(outfilename,"wb",&ntasks,&nfiles,&chunksizes,&fsblksize,&globalranks,&fp);
  printf("siondump: sion file opened, sid:     %d\n",sid);
 
  printf("siondump: opening:                   %s ...\n",infilename);
  infp=fopen(infilename,"r");
  if (infp==NULL) {
    return(_sion_errorprint(-1,_SION_ERROR_ABORT,"cannot open %s, aborting ...\n",infilename));
  }

  rank=0;np=0;gnp=0;
  nextparts=partpertask + ((rank<partpertask_remain)?1:0);
  sion_seek(sid,rank,SION_CURRENT_BLK,SION_CURRENT_POS);
  printf("siondump: ->rank:  %5d  %10d particles (%10.4f MB)\n",rank,nextparts,chunksizes[rank]/1024.0/1024.0);

  while( (!feof(infp)) && (gnp<numparts) ) {
    rc=fscanf(infp,"%le %le %le %le %le %le %le %le %le %le %le %le %le %le",
	      &doubleval[0],&doubleval[1],&doubleval[2],&doubleval[3],&doubleval[4],&doubleval[5],
	      &doubleval[6],&doubleval[7],&doubleval[8],&doubleval[9],&doubleval[10],
	      &doubleval[11],&doubleval[12],&doubleval[13]);
    left=sizeof(double)*14;
    while(left>0) {
      wrote=fwrite(doubleval,1,left,fp); /* !!! */
      left-=wrote;
    }

/*     printf("siondump: ->  wrote:  %15ld bytes\n",wrote);  */
    
    np++;gnp++;

    if(gnp%100000==0) {
      printf("siondump:              ... found %8d particles ... rc=%d (%le %le %le)\n",gnp,rc,doubleval[0],doubleval[1],doubleval[2]);
    }

    if ( (np>=nextparts) && (gnp<numparts) && ((rank+1)<ntasks) ) {
      rank++;np=0;
      nextparts=partpertask + ((rank<partpertask_remain)?1:0); 
      sion_seek(sid,rank,SION_CURRENT_BLK,SION_CURRENT_POS);
      printf("siondump: ->rank:  %5d  %10d particles (%10.4f MB)\n",rank,nextparts,chunksizes[rank]/1024.0/1024.0);
    }
  }
  fclose(infp);
  printf("siondump:              ... wrote %d particles\n",gnp);
  sion_seek(sid,0,SION_CURRENT_BLK,SION_CURRENT_POS);
  sion_close(sid);
  printf("siondump:              close sionfile sid=%d\n",sid);

}


int generateasciifromsionfile (
			       char        *infilename,
			       char        *outfilename,
			       int          opt_verbose
			       ) {
  
  sion_int64 *chunksizes=NULL;
  int        *globalranks=NULL;
  int         ntasks, nfiles, blknum;

  FILE *fp,*outfp;
  int rank, nullcount, rc;
  int sid,size,blocks;
  long  numparts,nextparts,np,gnp; 
  long  partpertask=0;
  long  partpertask_remain=0;
  double *localbuffer=NULL;
  double doubleval[14];
  sion_int32 fsblksize;
  long wrote,left;
  sion_int64  globalskip, bsumread,bread;
  sion_int64  start_of_varheader;
  sion_int64 *sion_chunksizes;
  sion_int64 *sion_globalranks;
  sion_int64 *sion_blockcount;
  sion_int64 *sion_blocksizes;

  chunksizes=NULL;globalranks=NULL; /* will be allocated by sion_open */

  sid=sion_open(infilename,"rb",&ntasks,&nfiles,&chunksizes,&fsblksize,&globalranks,&fp);

  printf("sionpepc: sid:                       %d\n",sid);
  printf("sionpepc: filename:                  %-30s\n",infilename);
  printf("sionpepc: ntasks:                    %d\n",ntasks);
  printf("sionpepc: fsblksize:                 %lu bytes (%6.2f MB)\n",(unsigned long)fsblksize,fsblksize/1024.0/1024.0);
  printf("sionpepc: current endianness:        %s\n",(sion_get_endianness())?"big":"little");
  printf("sionpepc: file endianness:           %s\n",(sion_get_endianness(sid))?"big":"little");

  sion_get_locations(sid,&size,&blocks,&globalskip,&start_of_varheader,&sion_chunksizes,&sion_globalranks,
                     &sion_blockcount,&sion_blocksizes);

  printf("sionpepc: max number of blocks:      %d\n",blocks);

 
  printf("siondump: opening:                   %s ...\n",outfilename);
  outfp=fopen(outfilename,"w");
  if (outfp==NULL) {
    return(_sion_errorprint(-1,_SION_ERROR_ABORT,"cannot open %s, aborting ...\n",outfilename));
  }
  gnp=0;
  
  for(rank=0;rank<size;rank++) {

    /* set position in outfile to current rank */
    printf("sionpepc: ->rank:                    %d\n",rank);

    for(blknum=0;blknum<sion_blockcount[rank];blknum++) {

      /* seek position of block */
      sion_seek(sid,rank,blknum,0);

      /* read data from block */
      left=sion_blocksizes[size*blknum+rank];
      bsumread=0;nullcount=0;

      while(left>0) {

        bread=fread(doubleval,1,14*sizeof(double),fp);

        fprintf(outfp,"  %.5le  %.5le  %.5le  %.5le  %.5le  %.5le  %.5le   %.5le  %.5le  %.5le  %.5le   %.5le %8ld %8ld\n",
                doubleval[0],doubleval[1],doubleval[2],doubleval[3],doubleval[4],doubleval[5],
                doubleval[6],doubleval[7],doubleval[8],doubleval[9],doubleval[10],
                doubleval[11],(long) doubleval[12],(long) doubleval[13]);
          
        if(bread==0) nullcount++; else nullcount=0;
        left-=bread;
        bsumread+=bread;
        if (nullcount>MAXNULLCNT) { fprintf(stderr, "timeout on read data , aborting ...\n"); exit(0);}

        if(gnp%100000==0) {
          printf("siondump:              ... found %8d particles ... rc=%d (%le %le %le)\n",gnp,bread,
                 doubleval[0],doubleval[1],doubleval[2]);
        }

        gnp++;
      }

    }

  }

  sion_close(sid);
  fclose(outfp);

}
