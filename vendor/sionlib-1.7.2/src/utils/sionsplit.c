/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
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
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_file.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"

#define FILENAME_LENGTH 1024

static void usage(char *name);

int main(int argc, char **argv)
{
  _sion_fileptr  *outfp; 
  char      infilename[FILENAME_LENGTH];
  char      outfilename[FILENAME_LENGTH];
  char      prefix[FILENAME_LENGTH];
  char      fnmask[FILENAME_LENGTH];
  char      fnkmask[FILENAME_LENGTH];

  int       i, rank, blknum;
  char     *localbuffer;
  sion_int64 chunksize = 0;
  sion_int64 left, bread, bsumread, bwrote;

  /* options */
  int       digits = 5;
  int       verbose = 0;
  int       useglobalranks = 0;

  /* for file infomation */
  int       sid, ntasks, nfiles, maxblocks;
  sion_int32 fsblksize;
  sion_int64 globalskip;
  sion_int64 start_of_varheader;
  sion_int64 *sion_localsizes;
  sion_int64 *sion_globalranks;
  sion_int64 *sion_blockcount;
  sion_int64 *sion_blocksizes;
  sion_int64 *sionsplit_sum_bytes_per_task;
  sion_int64 sionsplit_sum_bytes;
  sion_int64 sionsplit_filesize;

  _sion_filedesc *sion_filedesc;
  uint64_t    key;


  /* parse command line */
  i = 1;
  if (argc < 3)
    usage(argv[0]);

  while (i < argc) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
      case 'd':
        digits = atoi(argv[++i]);
        break;
      case 'g':
        useglobalranks=1;
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
  strcpy(prefix, argv[argc - 1]);

  printf("sionsplit: filename:                  %-30s\n", infilename);
  printf("sionsplit: prefix:                    %-30s\n", prefix);

  sid = sion_open(infilename, "rb,posix", &ntasks, &nfiles, NULL, &fsblksize, NULL, NULL);

  /* get the sion file structure */
  if ((_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  printf("sionsplit: sid:                       %d\n", sid);
  printf("sionsplit: filename:                  %-30s\n", infilename);
  printf("sionsplit: number of tasks:           %d\n", ntasks);
  printf("sionsplit: number of files:           %d\n", nfiles);
  printf("sionsplit: number of digits:          %d\n", digits);
  printf("sionsplit: use global ranks:          %d\n", useglobalranks);
  printf("sionsplit: keyval:                    %d (%s)\n", (int) sion_filedesc->keyvalmode, sion_keyval_type_to_str(sion_filedesc->keyvalmode));
  printf("sionsplit: current endianness:        %s\n", (sion_get_endianness())? "big" : "little");
  printf("sionsplit: file endianness:           %s\n", (sion_get_file_endianness(sid)) ? "big" : "little");
  printf("sionsplit: fsblksize:                 %lu bytes (%6.2f MB)\n", (unsigned long) fsblksize, fsblksize / 1024.0 / 1024.0);

  sion_get_locations(sid, &ntasks, &maxblocks, &globalskip, &start_of_varheader, &sion_localsizes, &sion_globalranks, &sion_blockcount,
                     &sion_blocksizes);

  printf("sionsplit: max number of chunks:      %d\n", maxblocks);

  /* analysis */
  sionsplit_sum_bytes_per_task = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
  for (rank = 0; rank < ntasks; rank++)
    sionsplit_sum_bytes_per_task[rank] = 0;
  for (rank = 0; rank < ntasks; rank++) {
    for (blknum = 0; blknum < maxblocks; blknum++) {
      sionsplit_sum_bytes_per_task[rank] += sion_blocksizes[ntasks * blknum + rank];
    }
  }

  sionsplit_sum_bytes = 0;
  for (rank = 0; rank < ntasks; rank++)
    sionsplit_sum_bytes += sionsplit_sum_bytes_per_task[rank];

  printf("sionsplit: datasize in file (aggr.):  %lld bytes  (%6.2f MB)\n", sionsplit_sum_bytes, sionsplit_sum_bytes / 1024.0 / 1024.0);

  sionsplit_filesize = start_of_varheader + (maxblocks + 1) * rank * sizeof(sion_int64);

  printf("sionsplit: start_of_varheader:        %lld bytes  (%6.2f MB)\n", start_of_varheader, start_of_varheader / 1024.0 / 1024.0);
  printf("sionsplit: size of file:              %lld bytes  (%6.2f MB)\n", sionsplit_filesize, sionsplit_filesize / 1024.0 / 1024.0);

  if (sionsplit_filesize > 0) {
    printf("sionsplit: file usage:                %8.6f%%\n", (double) sionsplit_sum_bytes / (double) sionsplit_filesize * 100.0);
  }
  chunksize = 0;
  
  sprintf(fnmask, "%s%%0%dd", prefix, digits);
  printf("sionsplit: filename mask:             %-s\n", fnmask);

  sprintf(fnkmask, "%s%%0%dd_%%012ld", prefix, digits);
  printf("sionsplit: filename key mask:         %-s\n", fnkmask);

  if(sion_filedesc->keyvalmode==SION_KEYVAL_NONE) {
    /* standard copy with read/write */

    for (rank = 0; rank < ntasks; rank++) {
      if (chunksize<sion_localsizes[rank]) chunksize=sion_localsizes[rank];
      if (chunksize<sion_blocksizes[rank]) chunksize=sion_blocksizes[rank];
    }
    printf("sionsplit: max chunksize:             %lld\n", chunksize);
    localbuffer = (char *) malloc(chunksize * sizeof(char));
    if (localbuffer == NULL) {
      fprintf(stderr, "cannot allocate localbuffer of size %lld , aborting ...\n", chunksize * sizeof(char));
      free(sionsplit_sum_bytes_per_task);
      return (1);
    }
    
  
    for (rank = 0; rank < ntasks; rank++) {
      if(useglobalranks) {
	sprintf(outfilename, fnmask, (int) sion_globalranks[rank]);
      } else {
	sprintf(outfilename, fnmask, rank);
      }
      
      
      printf("sionsplit: generating file:           %-s\n", outfilename);
      outfp = _sion_file_open(outfilename,SION_FILE_FLAG_ANSI|SION_FILE_FLAG_WRITE|SION_FILE_FLAG_CREATE,0);
      if (outfp == NULL) {
	fprintf(stderr, "cannot open outfile %s , aborting ...\n", outfilename);
        free(sionsplit_sum_bytes_per_task);
        free(localbuffer);
	return (1);
      }

      for (blknum = 0; blknum < sion_blockcount[rank]; blknum++) {
	
	/* seek position of block */
	sion_seek(sid, rank, blknum, 0);
	DPRINTFP((1, "sionsplit", 0, "after sion_seek sid=%d rank=%d blknum=%d fileposition=%lld\n", sid, rank, blknum, sion_get_position(sid)));
	
	/* read data from block */
	left = sion_blocksizes[ntasks * blknum + rank];
	bsumread = 0;
	while (left > 0) {
	  DPRINTFP((8, "sionsplit", 0, "will read %lld bytes localbuffer+%lld\n", left, bsumread));
	  bread = sion_fread(localbuffer + bsumread, 1, left, sid);
	  left -= bread;
	  bsumread += bread;
	  printf("sionsplit:                            %lld read left=%lld \n", bread, left);
	}

	/* write data to outfile */
	left = sion_blocksizes[ntasks * blknum + rank];
	bwrote = _sion_file_write(localbuffer, left, outfp);
	printf("sionsplit:                            %lld wrote of left=%lld \n", bwrote, left);
	
      }
      
      _sion_file_close(outfp);

    }

    free(localbuffer);

  } else {
    
    /* key-value copy with read/write */

    printf("sionsplit: buffer size:             %d\n", fsblksize);
    localbuffer = (char *) malloc(fsblksize * sizeof(char));
    if (localbuffer == NULL) {
      fprintf(stderr, "cannot allocate localbuffer of size %lld , aborting ...\n", chunksize * sizeof(char));
      free(sionsplit_sum_bytes_per_task);
      return (1);
    }

    for (rank = 0; rank < ntasks; rank++) {

      if (verbose)
	printf("siondefrag: ->rank:                    %d\n", rank);
      if (!verbose)
	if (rank % 16 == 0) {
	  printf("[%d]", rank);
	  fflush(stdout);
	}

      /* search rank in input file */
      sion_seek(sid, rank, SION_CURRENT_BLK, SION_CURRENT_POS);
      
      sion_key_full_scan(sid);
      
      /* reset iterator over keys */
      sion_key_list_iterator_reset(sid);
      
      /* loop over key-value blocks */
      while(sion_key_list_iterator_next(sid,&key)==SION_SUCCESS) {

	if(useglobalranks) {
	  sprintf(outfilename, fnkmask, (int) sion_globalranks[rank],(long) key);
	} else {
	  sprintf(outfilename, fnkmask, rank,(long) key);
	}

	printf("sionsplit: generating file:           '%-s'\n", outfilename);
	outfp = _sion_file_open(outfilename,SION_FILE_FLAG_ANSI|SION_FILE_FLAG_WRITE|SION_FILE_FLAG_CREATE,0);
	if (outfp == NULL) {
	  fprintf(stderr, "cannot open outfile %s , aborting ...\n", outfilename);
          free(localbuffer);
          free(sionsplit_sum_bytes_per_task);
	  return (1);
	}
	
	while( (bread=sion_fread_key(localbuffer,key,1,fsblksize,sid))>0 ) {
	  if(bread>0) {
	    bwrote = _sion_file_write(localbuffer, bread, outfp);
	  }
	  if (verbose)
	    printf("sionsplit:                               extracting now data of key[%12ld] (%lld bytes) (%lld bytes written)\n", 
		   (long) key, bread, bwrote);
	}
	
	_sion_file_close(outfp);
	
      }

    }
    free(localbuffer);
  }

  free(sionsplit_sum_bytes_per_task);
  sion_close(sid);

  return (0);
}

void usage(char *name)
{
  fprintf(stderr, "Usage: %s options <sionfn> <prefix>\n\n", name);

  fprintf(stderr, "Split SIONlib file <sionfn> into separate files. For each task a file is\n");
  fprintf(stderr, "created with the name <prefix><digits>\n\n");

  fprintf(stderr, "Example: %s data.sion data/file_\n", name);
  fprintf(stderr, "  creates files data/file_00000, data/file_00001, ...\n\n");

  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  [-v]         verbose mode\n");
  fprintf(stderr, "  [-g]         use global rank for numbering files\n");
  fprintf(stderr, "  [-d <num>]   number of digits for filename generation (default 5)\n");
  fprintf(stderr, "  [-V]         show version of SIONlib\n");
  fprintf(stderr, "  [-h]         show this help\n");
  exit(1);
}
