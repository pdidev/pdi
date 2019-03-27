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
#include "sion_file.h"

#define FILENAME_LENGTH 1024

static void usage(char *name);

int main(int argc, char **argv)
{
  _sion_fileptr  *outfp;
  char      infilename[FILENAME_LENGTH];
  char      outfilename[FILENAME_LENGTH];

  int       i, rank, blk, startrank, endrank, startblk, endblk;
  char     *localbuffer;
  sion_int64 chunksize = 0;
  sion_int64 left, bread, bsumread, bwrote;

  /* options */
  int       verbose = 0;
  int       use_outfile = 0;
  int       tasknum = -1;
  int       blknum = -1;

  /* for file infomation */
  int       sid, ntasks, nfiles, maxblocks;
  sion_int32 fsblksize;
  sion_int64 globalskip;
  sion_int64 start_of_varheader;
  sion_int64 *sion_localsizes;
  sion_int64 *sion_globalranks;
  sion_int64 *sion_blockcount;
  sion_int64 *sion_blocksizes;
  sion_int64 *sioncat_sum_bytes_per_task;
  sion_int64 sioncat_sum_bytes;
  sion_int64 sioncat_filesize;


  /* parse command line */
  i = 1;
  if (argc < 2)
    usage(argv[0]);

  while (i < argc) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
      case 'o':
        use_outfile = 1;
        strcpy(outfilename,argv[++i]);
        break;
      case 't':
        tasknum=atoi(argv[++i]);
        break;
      case 'b':
        blknum=atoi(argv[++i]);
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

  strcpy(infilename, argv[argc - 1]);

  if(verbose) fprintf(stderr,"sioncat: filename:                  %-30s\n", infilename);
  if(verbose) fprintf(stderr,"sioncat: outfile:                   %-30s\n", outfilename);

  sid = sion_open(infilename, "rb,posix", &nfiles, &ntasks, NULL, &fsblksize, NULL, NULL);

  if(verbose) fprintf(stderr,"sioncat: sid:                       %d\n", sid);
  if(verbose) fprintf(stderr,"sioncat: filename:                  %-30s\n", infilename);
  if(verbose) fprintf(stderr,"sioncat: number of tasks:           %d\n", ntasks);
  if(verbose) fprintf(stderr,"sioncat: write data of task:        %d\n", tasknum);
  if(verbose) fprintf(stderr,"sioncat: write data of block:       %d\n", blknum);
  if(verbose) fprintf(stderr,"sioncat: current endianness:        %s\n", (sion_get_endianness())? "big" : "little");
  if(verbose) fprintf(stderr,"sioncat: file endianness:           %s\n", (sion_get_file_endianness(sid)) ? "big" : "little");
  if(verbose) fprintf(stderr,"sioncat: fsblksize:                 %lu bytes (%6.2f MB)\n", (unsigned long) fsblksize, fsblksize / 1024.0 / 1024.0);

  sion_get_locations(sid, &ntasks, &maxblocks, &globalskip, &start_of_varheader, &sion_localsizes, &sion_globalranks, &sion_blockcount,
                     &sion_blocksizes);

  if(verbose) fprintf(stderr,"sioncat: max number of chunks:      %d\n", maxblocks);

  /* analysis */
  sioncat_sum_bytes_per_task = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
  for (rank = 0; rank < ntasks; rank++)
    sioncat_sum_bytes_per_task[rank] = 0;
  for (rank = 0; rank < ntasks; rank++) {
    for (blk = 0; blk < maxblocks; blk++) {
      sioncat_sum_bytes_per_task[rank] += sion_blocksizes[ntasks * blk + rank];
    }
  }

  sioncat_sum_bytes = 0;
  for (rank = 0; rank < ntasks; rank++)
    sioncat_sum_bytes += sioncat_sum_bytes_per_task[rank];

  if(verbose) fprintf(stderr,"sioncat: datasize in file (aggr.):  %lld bytes  (%6.2f MB)\n", sioncat_sum_bytes, sioncat_sum_bytes / 1024.0 / 1024.0);

  sioncat_filesize = start_of_varheader + (maxblocks + 1) * rank * sizeof(sion_int64);

  if(verbose) fprintf(stderr,"sioncat: start_of_varheader:        %lld bytes  (%6.2f MB)\n", start_of_varheader, start_of_varheader / 1024.0 / 1024.0);
  if(verbose) fprintf(stderr,"sioncat: size of file:              %lld bytes  (%6.2f MB)\n", sioncat_filesize, sioncat_filesize / 1024.0 / 1024.0);

  if (sioncat_filesize > 0) {
    if(verbose) fprintf(stderr,"sioncat: file usage:                %8.6f%%\n", (double) sioncat_sum_bytes / (double) sioncat_filesize * 100.0);
  }
  chunksize = 0;

  for (rank = 0; rank < ntasks; rank++) {
    if (chunksize<sion_localsizes[rank]) chunksize=sion_localsizes[rank];
    if (chunksize<sion_blocksizes[rank]) chunksize=sion_blocksizes[rank];
  }
  if(verbose) fprintf(stderr,"sioncat: max chunksize:             %lld\n", chunksize);
  localbuffer = (char *) malloc(chunksize * sizeof(char));
  if (localbuffer == NULL) {
    fprintf(stderr, "cannot allocate localbuffer of size %lld , aborting ...\n", chunksize * sizeof(char));
    free(sioncat_sum_bytes_per_task);
    return (1);
  }


  if(use_outfile) {
    outfp = _sion_file_open(outfilename,SION_FILE_FLAG_ANSI|SION_FILE_FLAG_WRITE|SION_FILE_FLAG_CREATE,0);
    if (outfp == NULL) {
      fprintf(stderr, "cannot open outfile %s , aborting ...\n", outfilename);
      free(localbuffer);
      free(sioncat_sum_bytes_per_task);
      return (1);
    }
  }
  if(tasknum>=0) {
    if (tasknum>=ntasks) {
      fprintf(stderr, "task number %d out of range %d .. %d , aborting ...\n", tasknum,0,ntasks-1);
      free(localbuffer);
      free(sioncat_sum_bytes_per_task);
      return (1);
    }
    startrank=tasknum;
    endrank=tasknum;
  } else {
    startrank=0;
    endrank=ntasks-1;
  }

  for (rank = startrank; rank <= endrank; rank++) {

    if(verbose) fprintf(stderr, "sioncat: processing task:           %6d\n", rank);

    if(blknum>=0) {
      if (blknum>=sion_blockcount[rank]) {
        fprintf(stderr, "blk number %d out of range %d .. %d , aborting ...\n", blknum,0,(int) sion_blockcount[rank]-1);
        free(sioncat_sum_bytes_per_task);
        return (1);
      }
      startblk=blknum;
      endblk=blknum;
    } else {
      startblk=0;
      endblk=sion_blockcount[rank]-1;
    }

    for (blk = startblk; blk <= endblk; blk++) {

      /* seek position of block */
      sion_seek(sid, rank, blk, 0);
      DPRINTFP((1, "sioncat", 0, "after sion_seek sid=%d rank=%d blknum=%d fileposition=%lld\n",
                sid, rank, blk, sion_get_position(sid)));

      /* read data from block */
      left = sion_blocksizes[ntasks * blk + rank];
      bsumread = 0;
      while (left > 0) {
        DPRINTFP((8, "sioncat", 0, "will read %lld bytes localbuffer+%lld\n", left, bsumread));
        bread = sion_fread(localbuffer + bsumread, 1, left, sid);
        left -= bread;
        bsumread += bread;
        if(verbose) fprintf(stderr,"sioncat:                            %lld read left=%lld \n", bread, left);
      }

      /* write data to outfile */
      left = sion_blocksizes[ntasks * blk + rank];
      if(use_outfile) {
        _sion_file_write(localbuffer, left, outfp);
      } else {
        bwrote = fwrite(localbuffer, 1, left, stdout);
        if (bwrote != left) {
          fprintf(stderr, "problems writing data of size %d on stdout (rc=%d) , aborting ...\n", (int) left, (int) bwrote);
          free(sioncat_sum_bytes_per_task);
          free(localbuffer);
          return (1);
        }
      }

    }


  }

  if(use_outfile) {
    _sion_file_close(outfp);
  }

  free(sioncat_sum_bytes_per_task);
  free(localbuffer);
  sion_close(sid);

  return (0);
}

void usage(char *name)
{
  fprintf(stderr, "Usage: %s options <sionfn> \n\n", name);

  fprintf(stderr, "%s <sionfn> extracts data from SIONlib file <sionfn>. With -t the\n", name);
  fprintf(stderr, "output can be restricted to a single task and with -b to a single\n");
  fprintf(stderr, "block. %s does not extract any SIONlib meta data.\n\n", name);

  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  [-v]             verbose mode \n");
  fprintf(stderr, "  [-t <tasknum>]   write only data of task <tasknum>\n");
  fprintf(stderr, "  [-b <blknum>]    write only data of block <blknum>\n");
  fprintf(stderr, "  [-o <outfile>]   file data will be written to, if not specified stdout\n");
  fprintf(stderr, "                   is used\n");
  fprintf(stderr, "  [-V]             show version of SIONlib\n");
  fprintf(stderr, "  [-h]             show this help\n");
  exit(1);
}
