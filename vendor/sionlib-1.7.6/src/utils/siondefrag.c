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

#include "sion.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"

#define FILENAME_LENGTH 1024
#define FILE_MODE_LENGTH 1024
#define MAXNULLCNT 1000

#define MB *1024*1024

static void usage(char *name);


int main(int argc, char **argv)
{
  char      infilename[FILENAME_LENGTH];
  char      outfilename[FILENAME_LENGTH];
  char      file_mode[FILE_MODE_LENGTH];
  char      endianness[FILE_MODE_LENGTH];
  char      keyvalue[FILE_MODE_LENGTH];

  int       i, rank, blknum;
  int       nullcount;
  sion_int64 max_chunksize = 0;
  char     *localbuffer = NULL;
  long      localbuffer_size=-1;
  sion_int64 left, bread, bsumread, bwrote, bsumwrote;
  sion_int64 *chunksizes = NULL;
  int      *globalranks = NULL;

  /* options */
  int       verbose = 0;
  /* sion_int64 opt_localsize = -1; */
  sion_int32 opt_fsblocksize = -1;
  sion_int32 opt_nfiles = 1;

  /* for file infomation */
  int       ntasks, nfiles, onfiles;
  sion_int32 fsblksize;
  int       sid, outsid, size, blocks;
  sion_int64 globalskip;
  sion_int64 start_of_varheader;
  sion_int64 *sion_chunksizes;
  sion_int64 *sion_globalranks;
  sion_int64 *sion_blockcount;
  sion_int64 *sion_blocksizes;
  sion_int64 *siondefrag_sum_bytes_per_task;
  sion_int64 siondefrag_sum_bytes;
  sion_int64 siondefrag_filesize;
  sion_int64 siondefrag_sum_chunksizes;
  _sion_filedesc *sion_filedesc;
  uint64_t    key;
  size_t      len;


  /* parse command line */
  i = 1;
  if (argc < 3)
    usage(argv[0]);

  while (i < argc) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
        /*
	  case 's':
	  opt_localsize = atoi(argv[++i]);
	  break;
	  case 'S':
	  opt_localsize = atoi(argv[++i]) MB;
	  break;
        */
      case 'n':
        opt_nfiles = atoi(argv[++i]);
        break;
      case 'q':
        opt_fsblocksize = atoi(argv[++i]);
        break;
      case 'Q':
        opt_fsblocksize = atoi(argv[++i]) MB;
        break;
      case 'v':
        verbose++;
        break;
      case 'V':
        fprintf(stderr, "SIONlib utility %s (Version %d.%dp%d, fileformat version %d)\n", argv[0],
                SION_MAIN_VERSION,SION_SUB_VERSION,
                SION_VERSION_PATCHLEVEL,SION_FILEFORMAT_VERSION);
        exit(1);
      case 'h':
        usage(argv[0]);
        break;
      default:
        usage(argv[0]);
      }
    }
    i++;
  }

  strcpy(infilename, argv[argc - 2]);
  strcpy(outfilename, argv[argc - 1]);

  printf("siondefrag: infilename:                %-30s\n", infilename);
  printf("siondefrag: outfilename:               %-30s\n", outfilename);

  chunksizes = NULL;
  globalranks = NULL;           /* will be allocated by sion_open */

  sid = sion_open(infilename, "rb,posix", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, NULL);

  /* get the sion file structure */
  if ((_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }


  printf("siondefrag: sid:                       %d\n", sid);
  printf("siondefrag: filename:                  %-30s\n", infilename);
  printf("siondefrag: nfiles:                    %d\n", nfiles);
  printf("siondefrag: fsblksize:                 %lu bytes (%6.2f MB)\n", (unsigned long) fsblksize, fsblksize / 1024.0 / 1024.0);
  printf("siondefrag: keyval:                    %d (%s)\n", (int) sion_filedesc->keyvalmode, sion_keyval_type_to_str(sion_filedesc->keyvalmode));
  printf("siondefrag: current endianness:        %s\n", (sion_get_endianness())? "big" : "little");
  printf("siondefrag: file endianness:           %s\n", (sion_get_file_endianness(sid)) ? "big" : "little");

  sion_get_locations(sid, &size, &blocks, &globalskip, &start_of_varheader, &sion_chunksizes, &sion_globalranks, &sion_blockcount,
                     &sion_blocksizes);

  printf("siondefrag: max number of blocks:      %d\n", blocks);

  /* analysis */
  siondefrag_sum_bytes_per_task = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
  for (rank = 0; rank < size; rank++)
    siondefrag_sum_bytes_per_task[rank] = 0;
  for (rank = 0; rank < size; rank++) {
    for (blknum = 0; blknum < sion_blockcount[rank]; blknum++) {
      siondefrag_sum_bytes_per_task[rank] += sion_blocksizes[size * blknum + rank];
    }
  }
  siondefrag_sum_chunksizes = 0;
  siondefrag_sum_bytes = 0;
  for (rank = 0; rank < size; rank++) {
    siondefrag_sum_bytes += siondefrag_sum_bytes_per_task[rank];
    siondefrag_sum_chunksizes+= sion_chunksizes[rank];
  }
  printf("siondefrag: datasize in file (aggr.):  %lld bytes  (%6.2f MB)\n", siondefrag_sum_bytes, siondefrag_sum_bytes / 1024.0 / 1024.0);
  siondefrag_filesize = start_of_varheader + (blocks + 1) * rank * sizeof(sion_int64);
  printf("siondefrag: start_of_varheader:        %lld bytes  (%6.2f MB)\n", start_of_varheader, start_of_varheader / 1024.0 / 1024.0);
  printf("siondefrag: size of chunks:            %lld bytes  (%6.2f MB)\n", siondefrag_sum_chunksizes, siondefrag_sum_chunksizes / 1024.0 / 1024.0);
  if (siondefrag_filesize > 0) {
    printf("siondefrag: file usage:                %8.6f%%\n", (double) siondefrag_sum_bytes / (double) siondefrag_sum_chunksizes * 100.0);
  }

  /* scan for block with max. number of bytes  */
  for (rank = 0; rank < size; rank++) {
    if (sion_chunksizes[rank] > max_chunksize)
      max_chunksize = sion_chunksizes[rank];
    for (blknum = 0; blknum < sion_blockcount[rank]; blknum++) {
      if (sion_blocksizes[size * blknum + rank] > max_chunksize)
        max_chunksize = sion_blocksizes[size * blknum + rank];
    }
  }

  for (rank = 0; rank < size; rank++)
    chunksizes[rank] = (sion_int64) siondefrag_sum_bytes_per_task[rank];

  if (opt_fsblocksize != -1)
    fsblksize = opt_fsblocksize;

  onfiles = opt_nfiles;


  /* preserve endianness */
  if (sion_get_file_endianness(sid)) {
    sprintf(endianness, "endianness=big");
  } else {
    sprintf(endianness, "endianness=little");
  }
  if(sion_filedesc->keyvalmode==SION_KEYVAL_NONE) {
    sprintf(keyvalue, "keyval=none");
  } else if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    sprintf(keyvalue, "keyval=inline");
  } else {
    fprintf(stderr, "unknown keyvalue mode ..., aborting\n");
    free(siondefrag_sum_bytes_per_task);
    return (1);
  }



  if(sion_filedesc->keyvalmode==SION_KEYVAL_NONE) {
    /* standard copy with read/write */

    printf("siondefrag: max chunksize:             %lld\n", max_chunksize);
    localbuffer = (char *) malloc(max_chunksize * sizeof(char));
    if (localbuffer == NULL) {
      free(siondefrag_sum_bytes_per_task);
      fprintf(stderr, "cannot allocate localbuffer of size %lld , aborting ...\n", max_chunksize * sizeof(char));
      return (1);
    }


    sprintf(file_mode, "wb,posix,%s",endianness);
    outsid = sion_open(outfilename, file_mode, &ntasks, &onfiles, &chunksizes, &fsblksize, &globalranks, NULL);
    
    printf("siondefrag: outsid:                    %d\n", outsid);
    printf("siondefrag: fsblksize outfile:         %lu\n", (unsigned long) fsblksize);
    
    for (rank = 0; rank < size; rank++) {

      /* set position in outfile to current rank */
      if (verbose)
	printf("siondefrag: ->rank:                    %d\n", rank);
      if (!verbose)
	if (rank % 16 == 0) {
	  printf("[%d]", rank);
	  fflush(stdout);
	}
      sion_seek(outsid, rank, SION_CURRENT_BLK, SION_CURRENT_POS);

      if (verbose)
	printf("siondefrag:                            copy now %lld blocks on rank=%d \n", sion_blockcount[rank], rank);
      for (blknum = 0; blknum < sion_blockcount[rank]; blknum++) {

	/* seek position of block */
	sion_seek(sid, rank, blknum, 0);

	/* read data from block */
	left = sion_blocksizes[size * blknum + rank];
	if (verbose)
	  printf("siondefrag:                               copy now block #%d with %lld bytes \n", blknum, left);
	bsumread = 0;
	nullcount = 0;
	while (left > 0) {
	  bread = sion_fread(localbuffer + bsumread, 1, left, sid);
	  if (bread == 0)
	    nullcount++;
	  else
	    nullcount = 0;
	  left -= bread;
	  bsumread += bread;
	  /*         printf("siondefrag:                            %lld read left=%lld \n", bread, left); */
	  if (nullcount > MAXNULLCNT) {
	    fprintf(stderr, "timeout on read data , aborting ...\n");
	    exit(0);
	  }
	}

	/* write data to outfile */
	left = sion_blocksizes[size * blknum + rank];
	bsumwrote = 0;
	while (left > 0) {
	  bwrote = sion_fwrite(localbuffer + bsumwrote, 1, left, outsid);
	  if (bwrote == 0)
	    nullcount++;
	  else
	    nullcount = 0;
	  left -= bwrote;
	  bsumwrote += bwrote;
	  /*      printf("siondefrag:                            %ld wrote left=%ld position=%lld\n",bwrote,left,_sion_get_position(outfp)); */
	  if (nullcount > MAXNULLCNT) {
	    fprintf(stderr, "timeout on write data , aborting ...\n");
	    exit(0);
	  }
	}
      }

    }
    if (!verbose) printf("\n");
    sion_close(outsid);
    free(localbuffer);

  } else {
    /* key-value copy with read/write */

    sprintf(file_mode, "wb,posix,%s,%s",endianness,keyvalue);
    outsid = sion_open(outfilename, file_mode, &ntasks, &onfiles, &chunksizes, &fsblksize, &globalranks, NULL);
    
    for (rank = 0; rank < size; rank++) {

      if (verbose)
	printf("siondefrag: ->rank:                    %d\n", rank);
      if (!verbose)
	if (rank % 16 == 0) {
	  printf("[%d]", rank);
	  fflush(stdout);
	}

      /* set position in outfile to current rank */
      sion_seek(outsid, rank, SION_CURRENT_BLK, SION_CURRENT_POS);
    
      /* seek position in infile */
      sion_seek(sid, rank, SION_CURRENT_BLK, SION_CURRENT_POS);

      /* reset iterator over keys */
      sion_fread_key_iterator_reset(sid);
      
      /* loop over key-value blocks */
      while(sion_fread_key_iterator_next(sid,&key,&len)==SION_SUCCESS) {

	/* allocate buffer space */
	if((long) localbuffer_size< (long) len) {
	  if(localbuffer_size>0) {
	    free(localbuffer);
	  }
	  localbuffer = (char *) malloc(len * sizeof(char));
	  if (localbuffer == NULL) {
	    fprintf(stderr, "cannot allocate localbuffer of size %zu , aborting ...\n", len * sizeof(char));
            free(siondefrag_sum_bytes_per_task);
	    return (1);
	  }
	  localbuffer_size=len;
	}

	if (verbose)
	  printf("siondefrag:                               copy now key[%12ld] with %zu bytes \n", (long) key, len);
	
	/* read data of key */
	bread = sion_fread_key(localbuffer, key, 1, len, sid);
	/* bread=len; */
	if (bread != len) {
	  fprintf(stderr, "cannot read data of key %ld with len %zu, aborting ...\n",(long) key, (size_t) len * sizeof(char));
          free(siondefrag_sum_bytes_per_task);
	    return (1);
	}

	/* write data of key */
	bwrote = sion_fwrite_key(localbuffer, key, 1, len, outsid);
	/* bread=len; */
	if (bwrote != len) {
	  fprintf(stderr, "cannot write data of key %ld with len %zu, aborting ...\n",(long) key, (size_t) len * sizeof(char));
          free(siondefrag_sum_bytes_per_task);
	    return (1);
	}
	
      }

    }

    

    sion_close(outsid);
    if (!verbose) printf("\n");
  }

  free(siondefrag_sum_bytes_per_task);

  sion_close(sid);


  return (0);
}

void usage(char *name)
{
  fprintf(stderr, "Usage: %s options <insionfn> <outfn>\n\n", name);

  fprintf(stderr, "Creates a new SIONlib file <outfn> from an existing <insionfn>. The\n");
  fprintf(stderr, "resulting file has only one chunk. This can be used to make SIONlib\n");
  fprintf(stderr, "files less sparse (e.g. by using -q 1 which effectively removes file\n");
  fprintf(stderr, "system block alignment).\n\n");

  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  [-Q <fsblksize>]   filessystem blocksize for new sion file in MB\n");
  fprintf(stderr, "                     (default from input file)\n");
  fprintf(stderr, "  [-q <fsblksize>]   filessystem blocksize for new sion file in bytes\n");
  fprintf(stderr, "  [-v]               verbose mode\n");
  fprintf(stderr, "  [-V]               show version of SIONlib\n");
  fprintf(stderr, "  [-h]               show this help\n");
  exit(1);
}
