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
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"

#define FILENAME_LENGTH 1024

static void usage(char *name);

int main(int argc, char **argv)
{
  FILE     *fp;
  char      infilename[FILENAME_LENGTH], fname[FILENAME_LENGTH];
  int       i, f, rank, blknum;

  /* options */
  int       printall = 0;
  int       printmap = 0;
  int       printMap = 0;
  int       verbose = 0;
  int       longspec = 0;
  int       printisize = 0;
  int       printkeysstat = 0;
  int       printkeyslist = 0;

  /* for file infomation */
  int       sid, size, blocks, gblknum, ntasks;
  sion_int32 fsblksize;
  sion_int64 globalskip;
  sion_int64 start_of_varheader;
  sion_int64 *sion_chunksizes;
  sion_int64 *sion_globalranks;
  sion_int64 *sion_blockcount;
  sion_int64 *sion_blocksizes;
  sion_int64 *sion_currentpos;
  sion_int64 *sion_currentblocknr;
  sion_int64 *siondump_sum_bytes_per_task;
  sion_int64 siondump_sum_bytes;
  sion_int64 siondump_sum_chunksizes;
  sion_int64 siondump_sum_all_fsblocks;
  sion_int64 siondump_sum_full_fsblocks;
  sion_int64 siondump_sum_part_fsblocks;
  sion_int64 siondump_sum_bytes_in_part_fsblocks,blksize;
  sion_int64 numfullchunks,numemptychunks,numpartlychunks;
  sion_int64 fullchunkssize,emptychunkssize,partlychunkssize;
  _sion_filedesc *sion_filedesc;
  int         mapping_size, nfiles;
  sion_int32 *mapping;

  /* parse command line */
  i = 1;
  if (argc < 2)
    usage(argv[0]);

  while (i < argc) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
      case 'a':
        printall = 1;
        break;
      case 'm':
        printmap = 1;
        break;
      case 'M':
        printMap = 1;
        break;
      case 'v':
        verbose++;
        break;
      case 'l':
        longspec++;
        break;
      case 'k':
        printkeysstat++;
        break;
      case 'K':
        printkeyslist++;
        break;
      case 'S':
        printisize++;
        break;
      case 'h':
        usage(argv[0]);
        break;
      case 'V':
        fprintf(stderr, "SIONlib utility %s (Version %d.%dp%d, fileformat version %d)\n", argv[0],
                SION_MAIN_VERSION,SION_SUB_VERSION,
                SION_VERSION_PATCHLEVEL,SION_FILEFORMAT_VERSION);
        exit(1);
        break;
      default:
        usage(argv[0]);
      }
    }
    i++;
  }

  strcpy(infilename, argv[argc - 1]);

  sid = sion_open(infilename, "rb,posix", &ntasks, &nfiles, NULL, &fsblksize, NULL, &fp);

  if (sid<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"could not open file, aborting %d ...\n", sid));
  }

  /* get the sion file structure */
  if ((_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  printf("siondump: sid:                       %d\n", sid);
  /* printf("siondump: filename:                  %-30s\n",infilename); */
  printf("siondump: filename:                  %-30s\n", sion_filedesc->fname);
  printf("siondump: prefix:                    %-30s\n", sion_filedesc->prefix);
  printf("siondump: number of tasks:           %d\n", ntasks);
  printf("siondump: current endianness:        %s\n", (sion_get_endianness())? "big" : "little");
  printf("siondump: file endianness:           %s\n", (sion_get_file_endianness(sid)) ? "big" : "little");
  printf("siondump: filesystem blocksize:      %lu bytes (%6.2f MB)\n", (unsigned long) fsblksize, fsblksize / 1024.0 / 1024.0);
  printf("siondump: file set:                  %s\n", (nfiles > 1) ? "Yes" : "No");
  printf("siondump: file number:               %d\n", sion_filedesc->filenumber);
  printf("siondump: number of files:           %d\n", sion_filedesc->nfiles);
  printf("siondump: flag1:                     %lld (%d+%d)\n", sion_filedesc->flag1,
         (int) ((sion_int64) sion_filedesc->flag1>>32),
         (int) ((sion_int64) sion_filedesc->flag1 & ~((sion_int64) 1<<32) ) );
  printf("siondump: flag2:                     %lld\n", sion_filedesc->flag2);
  printf("siondump: keyval:                    %d (%s)\n", (int) sion_filedesc->keyvalmode, sion_keyval_type_to_str(sion_filedesc->keyvalmode));
  printf("siondump: file format version:       %d\n", sion_filedesc->fileversion);
  printf("siondump: file SIONlib version:      %d.%dp%d (current %d.%dp%d)\n",
         (int) sion_filedesc->filesionversion/1000,sion_filedesc->filesionversion%1000,sion_filedesc->filesionpatchlevel,
         SION_MAIN_VERSION,SION_SUB_VERSION,SION_VERSION_PATCHLEVEL);

  sion_get_locations(sid, &size, &blocks, &globalskip, &start_of_varheader, &sion_chunksizes, &sion_globalranks,
                     &sion_blockcount, &sion_blocksizes);
  /* ToDo: data has to be extracted from multifiles */
  /* printf("siondump: end_of_header:             %10lld bytes  (%6.2f MB)\n", sion_filedesc->end_of_header, sion_filedesc->end_of_header / 1024.0 / 1024.0); */
  /* printf("siondump: start_of_data:             %10lld bytes  (%6.2f MB)\n", sion_filedesc->start_of_data, sion_filedesc->start_of_data / 1024.0 / 1024.0); */
  /* printf("siondump: start_of_varheader:        %10lld bytes  (%6.2f MB)\n", start_of_varheader, start_of_varheader / 1024.0 / 1024.0); */
  printf("siondump: number of blocks:           %d\n", blocks);
  if(printisize) {
    int num_bytes, num_fds;
    sion_get_sizeof(sid,&num_bytes, &num_fds);
    printf("siondump: sizeof internal struct:    %d bytes %d file descriptors\n", num_bytes, num_fds);
  }

  /* analysis */
  siondump_sum_full_fsblocks          = 0;
  siondump_sum_part_fsblocks          = 0;
  siondump_sum_bytes_in_part_fsblocks = 0;

  numfullchunks=numemptychunks=numpartlychunks=0;
  fullchunkssize=emptychunkssize=partlychunkssize=0;

  siondump_sum_bytes_per_task = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
  for (rank = 0; rank < size; rank++) siondump_sum_bytes_per_task[rank] = 0;

  for (rank = 0; rank < size; rank++) {
    for (blknum = 0; blknum < sion_blockcount[rank]; blknum++) {
      blksize=sion_blocksizes[size * blknum + rank];
      siondump_sum_bytes_per_task[rank] += blksize;

      if(blksize==sion_chunksizes[rank]) {
        numfullchunks++;fullchunkssize+=blksize;
      } else {
        if(blksize>0) {
          numpartlychunks++;partlychunkssize+=blksize;
        } else {
          numemptychunks++;emptychunkssize+=sion_chunksizes[rank];
        }
      }

      siondump_sum_full_fsblocks += (int) ( (sion_int64) blksize / (sion_int64) fsblksize );
      if(blksize - fsblksize * ((sion_int64) (blksize/fsblksize)) > 0) {
        siondump_sum_bytes_in_part_fsblocks += blksize - fsblksize * ((int) (blksize/fsblksize));
        siondump_sum_part_fsblocks++;
      }
    }

    numemptychunks+=(blocks-sion_blockcount[rank]);
    emptychunkssize+=(blocks-sion_blockcount[rank])*sion_chunksizes[rank];
  }
  siondump_sum_all_fsblocks = siondump_sum_full_fsblocks + siondump_sum_part_fsblocks;

  siondump_sum_chunksizes = 0;
  siondump_sum_bytes = 0;
  for (rank = 0; rank < size; rank++) {
    siondump_sum_bytes += siondump_sum_bytes_per_task[rank];
    siondump_sum_chunksizes+= sion_chunksizes[rank]*sion_blockcount[rank];
  }

  printf("siondump: size of chunks:                        %12lld bytes  (%8.2f MB)\n",
         siondump_sum_chunksizes,
         siondump_sum_chunksizes / 1024.0 / 1024.0);

  printf("siondump: datasize in file (aggr.):              %12lld bytes  (%8.2f MB)\n",
         siondump_sum_bytes,
         siondump_sum_bytes / 1024.0 / 1024.0);

  printf("siondump: size of full chunks:                   %12lld bytes  (%8.2f MB) # %6lld\n",
         fullchunkssize,
         fullchunkssize / 1024.0 / 1024.0, numfullchunks);

  printf("siondump: size of partly filled chunks:          %12lld bytes  (%8.2f MB) # %6lld\n",
         partlychunkssize,
         partlychunkssize / 1024.0 / 1024.0, numpartlychunks);

  printf("siondump: size of empty chunks:                  %12lld bytes  (%8.2f MB) # %6lld\n",
         emptychunkssize,
         emptychunkssize / 1024.0 / 1024.0,numemptychunks);

  printf("siondump: overhead compared to task-local file:  %12lld bytes  (%8.2f MB)\n",
         siondump_sum_chunksizes-siondump_sum_bytes,
         (siondump_sum_chunksizes-siondump_sum_bytes) / 1024.0 / 1024.0);

  if (siondump_sum_chunksizes > 0) {
    printf("siondump: file usage (user data/chunk size):   %14.6f%%\n",
           (double) siondump_sum_bytes / (double) siondump_sum_chunksizes * 100.0);
  }


  printf("siondump: fsblocks, size of used blocks:         %12lld bytes  (%8.2f MB, %ld blocks)\n",
         siondump_sum_all_fsblocks * (sion_int64) fsblksize,
         siondump_sum_all_fsblocks * (sion_int64) fsblksize / 1024.0 / 1024.0,(long) siondump_sum_all_fsblocks);

  printf("siondump: fsblocks, size of fully used blocks:   %12lld bytes  (%8.2f MB, %ld blocks)\n",
         siondump_sum_full_fsblocks*fsblksize,
         siondump_sum_full_fsblocks*fsblksize / 1024.0 / 1024.0,
         (long) siondump_sum_full_fsblocks);

  printf("siondump: fsblocks, size of partly used blocks:  %12lld bytes of %12lld bytes (%6.2f MB of %6.2f MB, %ld blocks)\n",
         siondump_sum_bytes_in_part_fsblocks,
         siondump_sum_part_fsblocks*fsblksize,
         siondump_sum_bytes_in_part_fsblocks / 1024.0 / 1024.0,
         siondump_sum_part_fsblocks*fsblksize / 1024.0 / 1024.0,
         (long) siondump_sum_part_fsblocks);

  if (siondump_sum_chunksizes > 0) {
    printf("siondump: fsblocks, used fsblocks/chunk size:   %14.6f%%\n",
         (double) siondump_sum_all_fsblocks*fsblksize / (double) siondump_sum_chunksizes * 100.0);
  }

  if ((siondump_sum_all_fsblocks*fsblksize) > 0) {
    printf("siondump: fsblocks, user data/used blocks:      %14.6f%%\n",
           (double) siondump_sum_bytes / (double) (siondump_sum_all_fsblocks*fsblksize) * 100.0);
  }

  /* detailed print of block sizes */
  if (printall) {

    printf("\n");
    printf
      ("-----------------------------------------------------------------------------------------------------------------------------------------------------------\n");
    for (gblknum = 0; gblknum < blocks; gblknum += 10) {
      printf("%70s", " ");
      for (blknum = gblknum + 0; ((blknum < gblknum + 10) && (blknum < blocks)); blknum++) {
        printf("BLK%03d    ", blknum);
      }
      printf("\n");
      for (rank = 0; rank < size; rank++) {
        if(!longspec) {
          printf("Task %02d: size=%8.2f MB glblrnk=%4lld #blks=%2lld lsz=%8.2f MB:  ", rank, siondump_sum_bytes_per_task[rank] / 1024.0 / 1024.0,
                 sion_globalranks[rank], sion_blockcount[rank], sion_chunksizes[rank] / 1024.0 / 1024.0);
          for (blknum = gblknum + 0; ((blknum < gblknum + 10) && (blknum < blocks)); blknum++) {
            printf(" %6.2f MB ", (long) sion_blocksizes[size * blknum + rank] / 1024.0 / 1024.0);
          }
        } else {
          printf("Task %02d: size=%10ld B glblrnk=%4lld #blks=%2lld lsz=%10ld B: ", rank, (long) siondump_sum_bytes_per_task[rank],
                 sion_globalranks[rank], sion_blockcount[rank], (long) sion_chunksizes[rank]);
          for (blknum = gblknum + 0; ((blknum < gblknum + 10) && (blknum < blocks)); blknum++) {
            printf("%9ld ", (long) sion_blocksizes[size * blknum + rank]);
          }
        }
        printf("\n");
      }
      printf("\n");
    }
    printf
      ("-----------------------------------------------------------------------------------------------------------------------------------------------------------\n");
  }

  /* detailed list of keys */
  if (printkeysstat) {

    uint64_t    key;
    sion_key_stat_t keystat;
    int         i;

    if((sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) || (sion_filedesc->keyvalmode!=SION_KEYVAL_NONE)) {

    printf
      ("-----------------------------------------------------------------------------------------------------------------------------------------------------------\n");
      for (rank = 0; rank < size; rank++) {
	sion_seek(sid,rank,SION_CURRENT_CHUNK,SION_CURRENT_POS);

	sion_key_full_scan(sid);
	sion_key_list_iterator_reset(sid);
	i=0;
	while(sion_key_list_iterator_next(sid,&key)==SION_SUCCESS) {
	  sion_key_get_stat(sid,key,&keystat);
	  printf("Task %02d: key[%02d]=%8ld: #blocks=%6d totalsize=%10ld\n",rank, i++, (long) key, (int) keystat.num_blocks, (long) keystat.total_size);
	}
      }
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
    printf
      ("-----------------------------------------------------------------------------------------------------------------------------------------------------------\n");
  }

  /* detailed list of keys */
  if (printkeyslist) {

    uint64_t    key;
    size_t value_size;      
    int         i;

    if((sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) || (sion_filedesc->keyvalmode!=SION_KEYVAL_NONE)) {

    printf
      ("-----------------------------------------------------------------------------------------------------------------------------------------------------------\n");
      for (rank = 0; rank < size; rank++) {
	sion_seek(sid,rank,SION_CURRENT_CHUNK,SION_CURRENT_POS);
	sion_fread_key_iterator_reset(sid);
	i=0;
	printf("Task %02d:",rank);
	while(sion_fread_key_iterator_next(sid,&key,&value_size)==SION_SUCCESS) {
	  if (((i%5)==0) && (i>0)) printf("\n       :");
	  printf(" [%02d:k=<%ld>,size=%ld]",i++, (long) key, (long) value_size);
	}
	printf("\n");
      }
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);
    }
    printf
      ("-----------------------------------------------------------------------------------------------------------------------------------------------------------\n");
  }


  if (sion_filedesc->nfiles > 1) {
    printf("------------------------------------------------------------\n");
    printf("siondump: number of files:           %d\n", sion_filedesc->nfiles);
    for (i = 0; i < sion_filedesc->nfiles; ++i) {
      if(i>0) sprintf(fname, "%s.%06d", sion_filedesc->prefix, i);
      else    sprintf(fname, "%s", sion_filedesc->prefix);
      printf("siondump: file %3d:                    %s \n", i + 1, fname);
    }
    printf("------------------------------------------------------------\n");

    if(printmap) {
      sion_get_mapping(sid,&mapping_size,&mapping,&nfiles);
      printf("  mapping: ");
      for (i = 0; i < mapping_size; ++i) {
        printf("[%05d -> (f%03d,t%03d)] ", i, mapping[i*2+0],mapping[i*2+1]);
        if(i%16==15) printf("\n        ");
      }
      printf("\n");
      printf("------------------------------------------------------------\n");
    }
    if(printMap) {
      sion_get_mapping(sid,&mapping_size,&mapping,&nfiles);
      for (f = 0; f < nfiles; ++f) {
	printf("  file[%03d]: ",f);
	for (i = 0; i < mapping_size; ++i) {
	  if(mapping[i*2+0]==f) {
	    printf("%2d:%05d ", mapping[i*2+1], i);
	  }
	}
	printf("\n");
      }
      printf("------------------------------------------------------------\n");
    }
  }

  sion_get_current_locations(sid, &size, &sion_currentpos, &sion_currentblocknr);

  sion_close(sid);

  free(siondump_sum_bytes_per_task);

  return (0);
}

void usage(char *name)
{
  fprintf(stderr, "Usage: %s options <sionfn>\n\n", name);

  fprintf(stderr, "Dump meta data information of SIONlib file <sionfn>.\n\n");

  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  [-a]   print all information about all blocks\n");
  fprintf(stderr, "  [-m]   print all mapping information\n");
  fprintf(stderr, "  [-M]   print all mapping information (table) \n");
  fprintf(stderr, "  [-l]   print all sizes in number of bytes\n");
  fprintf(stderr, "  [-k]   print key-value statistic for each task\n");
  fprintf(stderr, "  [-K]   print key-value list for each task\n");
  fprintf(stderr, "  [-S]   print size of internal data structure in memory\n");
  fprintf(stderr, "  [-V]   show version number of SIONlib\n");
  fprintf(stderr, "  [-v]   verbose mode\n");
  fprintf(stderr, "  [-h]   show this help\n");
  exit(1);
}
