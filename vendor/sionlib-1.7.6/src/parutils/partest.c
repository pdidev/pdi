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

#include "sion_debug.h"
#include "sion_printts.h"
#include "sion.h"
#include "partest.h"
#include "partest_util.h"
#include "partest_split_comm.h"


/*!
 * \file
 * \brief Benchmark/Test Tool
 * \author Wolfgang Frings
 * \author Ventsislav Petkov
 */




int main(int argc, char **argv)
{
  int       rank, size, rc = 0;
  char     *localbuffer ;
  time_t    t;
  sion_int64 commwork_size64 = 1;

  /* communicators */
  _test_communicators communicators;

  /* options */
  _test_options options;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  /*                                                                      */ DPRINTFTS(rank, "after MPI_Init");

  /*   srand ( time(NULL)*rank ); */

  /*   printf("starting partest %02d of %02d\n", rank,size); */

  init_options(&options);



  if (rank == 0) {
#ifdef _SION_AIX
    parse_options_std(argc, argv, &options);
    if(rc==0) {
      usage(argv[0]);
    }
#else
    rc=parse_options_long(argc, argv, &options);
    if(rc==0) {
      usage_long(argv[0]);
    }
#endif
  }
  MPI_Bcast(&rc, 1, MPI_INT, 0, MPI_COMM_WORLD);
  if(rc==0) {
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  distribute_options_mpi(&options);

  /* adjust communicators */
  communicators.all = MPI_COMM_WORLD;
  MPI_Comm_size(MPI_COMM_WORLD, &communicators.all_size);
  MPI_Comm_rank(MPI_COMM_WORLD, &communicators.all_rank);
  split_communicator(&communicators, options.bluegene, options.bluegene_np, options.bluegene_sort, options.numfiles, options.read_task_offset, options.verbose);

  /* determine global and local size of data to be written and read */
  commwork_size64 = (sion_int64) communicators.work_size;
  if (options.globalsize > 0) {
    options.totalsize = (sion_int64) options.globalsize / commwork_size64;
  }
  else {
    /* commwork_size64 = 1; */
    options.globalsize = options.totalsize * commwork_size64;
  }

  if((options.totalsize>options.bufsize) || (options.read_task_offset>0) || (options.do_write==0) ) {
    options.suppress_checksum=1;
  }

  if(options.fsblksize<0) options.fsblksize=-1; 

  /* chunksize set to localsize if -1 */
  if(options.chunksize<0) options.chunksize=options.totalsize; 

  if ( (communicators.work_size>0) && (communicators.work_rank==0) ) {
    time(&t);
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
    fprintf(stderr, "SION parallel file I/O benchmark 'partest': start at %s", ctime(&t));
    fprintf(stderr, "partest Number of tasks that will use the file tasks:       running on %d tasks\n", size);
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
#ifndef     CHECKSUM
    fprintf(stderr, "partest parameter:   CHECKSUM DISABLED!\n\n");
#else
    if(options.suppress_checksum) {
      fprintf(stderr, "partest parameter:   CHECKSUM not possible, DISABLED!\n\n");
    }
#endif
    fprintf(stderr, "partest parameter:   (-f)    datafile                 = %s\n", options.filename);
    fprintf(stderr, "partest parameter:   (-n)    number of files          = %d\n", options.numfiles);
    fprintf(stderr, "partest parameter:   (-F)    random factor            = %13.4f\n", options.factor);
    fprintf(stderr, "partest parameter:   (-X)    remove files after test  = %d\n", options.unlink_files);
    fprintf(stderr, "partest parameter:   (-b/-B) local buffer size / task = %15lld bytes %10.3f MB\n", options.bufsize, options.bufsize / (1.0 MB));
    fprintf(stderr, "partest parameter:   (-g/-G) global total data size   = %15lld bytes %10.3f GB\n", options.globalsize, options.globalsize / (1024.0 MB));
    fprintf(stderr, "partest parameter:   (-s/-S) total data size / task   = %15lld bytes %10.3f MB\n", options.totalsize, options.totalsize / (1.0 MB));
    fprintf(stderr, "partest parameter:   (-r/-R) sion chunk size          = %15lld bytes %10.3f MB\n", options.chunksize, options.chunksize / (1.0 MB));
    fprintf(stderr, "partest parameter:   (-Q)    fs block size            = %15d bytes %10.3f MB\n", options.fsblksize, options.fsblksize / (1.0 MB));
    if (options.type == 0)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (sion, collective read)\n", options.type);
    if (options.type == 1)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (sion, independant read)\n", options.type);
    if (options.type == 2)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (mpi-io)\n", options.type);
    if (options.type == 3)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (single files)\n", options.type);
    fprintf(stderr, "partest parameter:   (-j)    serialize_blocknum       = %d\n", options.serialize_blocknum);
    fprintf(stderr, "partest parameter:   (-Z)    read task offset         = %d\n", options.read_task_offset);
    fprintf(stderr, "partest parameter:   (-o)    start offset bytes       = %d\n", options.startoffset);
    fprintf(stderr, "partest parameter:   (-v)    verbose                  = %d\n", options.verbose);
    fprintf(stderr, "partest parameter:   (-d)    debug                    = %d\n", options.debug);
    fprintf(stderr, "partest parameter:   (-D)    Debug                    = %d\n", options.Debug);
    fprintf(stderr, "partest parameter:   (-M)    collective write         = %d\n", options.collectivewrite);
    fprintf(stderr, "partest parameter:   (-m)    collective read          = %d\n", options.collectiveread);
    fprintf(stderr, "partest parameter:   (-c)    MSA-aware collective I/O = %d\n", options.collmsa);


    fprintf(stderr, "partest parameter:   (-w)    MPI-IO, IBM, Large Block IO = %d\n", options.mpiio_lb);
    fprintf(stderr, "partest parameter:   (-W)    MPI-IO, IBM, IO bufsize     = %d KB\n", options.mpiio_bs);
    fprintf(stderr, "partest parameter:   (-x)    MPI-IO, IBM, sparse access  = %d\n", options.mpiio_sa);
    fprintf(stderr, "partest parameter:   (-C)    suppress_checksum           = %d\n", options.suppress_checksum);
    fprintf(stderr, "partest parameter:   (-O)    do_write                    = %d\n", options.do_write);
    fprintf(stderr, "partest parameter:   (-I)    do_read                     = %d\n", options.do_read);
    fprintf(stderr, "partest parameter:   (-L)    use_posix                   = %d\n", options.use_posix);

    fprintf(stderr, "partest parameter:   (-P)    Blue Gene, I/O nodes        = %d\n", options.bluegene);
    fprintf(stderr, "partest parameter:   (-p)    Blue Gene: task/IO-nodes    = %d\n", options.bluegene_np);
    fprintf(stderr, "partest parameter:   (-Y)    Blue Gene: task sort        = %d\n", options.bluegene_sort);

    fprintf(stderr, "partest parameter:   (  )    commwork_size64             = %lld\n", commwork_size64);

    fprintf(stderr, "partest versioninfo:         SION_MAIN_VERSION           = %d\n", SION_MAIN_VERSION);
    fprintf(stderr, "partest versioninfo:         SION_SUB_VERSION            = %d\n", SION_SUB_VERSION);
    fprintf(stderr, "partest versioninfo:         SION_VERSION_PATCHLEVEL     = %d\n", SION_VERSION_PATCHLEVEL);
    fprintf(stderr, "partest versioninfo:         SION_FILEFORMAT_VERSION     = %d\n", SION_FILEFORMAT_VERSION);
    fprintf(stderr, "partest versioninfo:         SION_GIT_REVISION           = %s\n", GIT_REV);

    
  }

  barrier_after_start(MPI_COMM_WORLD);

  if ( (communicators.work_size>0) && (communicators.work_rank==0) ) {
    fprintf(stderr, "partest parameter:   (  )    comm(all)                   = %d of %d\n", communicators.all_rank, communicators.all_size);
    fprintf(stderr, "partest parameter:   (  )    comm(work)                  = %d of %d\n", communicators.work_rank, communicators.work_size);
    fprintf(stderr, "partest parameter:   (  )    comm(local)                 = %d of %d\n", communicators.local_rank, communicators.local_size);
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
  }

  barrier_after_start(MPI_COMM_WORLD);
  /*                                                                      */ DPRINTFTS(rank, "after pstart");
  /* Init the local buffer that will be written to the file */
  localbuffer = (char *) malloc(options.bufsize);

  srand(time(NULL)*communicators.work_rank);
  /* for (i = 0; i < (options.bufsize / sizeof(int)); i++) */
  /*   localbuffer[i] = (char) rand() % 256; */

/*   memset (localbuffer, 'a'+rank%26, bufsize);  */
  memset (localbuffer, 'a'+rank%26, options.bufsize); 
  barrier_after_malloc(MPI_COMM_WORLD);
  /*                                                                      */ DPRINTFTS(rank, "after malloc");

  /* random factor handling */
  if(options.factor>0.0) {
    if((options.collectivewrite) || (options.collectiveread)) {
      if(options.bufsize<options.totalsize*(1+options.factor)) {
	fprintf(stderr, "partest: ERROR deadlock possible if collective read/write and random factor used, and buffer is too small aborting\n");
	MPI_Abort(MPI_COMM_WORLD,0);
	exit(0);
      }
    }

    options.totalsize += ((sion_int64) (options.factor * (sion_int64) options.totalsize * (sion_int64) rand() / (sion_int64) RAND_MAX));
    options.chunksize += ((sion_int64) (options.factor * (sion_int64) options.totalsize * (sion_int64) rand() / (sion_int64) RAND_MAX));
    fprintf(stderr, "partest parameter:   (  )    new totalsize[%4d]          = %lld\n", communicators.work_rank, options.totalsize);
  }

  

  /*                                                                      */ DPRINTFTS(rank, "before scall2");
  if ( (communicators.work_size>0) && (communicators.work_rank==0) ) {
    fprintf(stderr, "partest parameter:   (  )    new totalsize                = %lld\n", options.totalsize);
  }
  barrier_after_malloc(MPI_COMM_WORLD);
  if (options.type == 0) {
    options.collectiveopenforread = 1;
    test_paropen_multi_mpi(options.filename, localbuffer, &communicators, &options);

  }
  else if (options.type == 1) {
    options.collectiveopenforread = 0;
    test_paropen_multi_mpi(options.filename, localbuffer, &communicators, &options);
  }
  else if (options.type == 2) {
    options.collectiveopenforread = 1;
    test_mpiio_multi_mpi(options.filename, localbuffer, &communicators, &options);
  }
  else if (options.type == 3) {
    options.collectiveopenforread = 1;
    test_single_mpi(options.filename, localbuffer, &communicators, &options);
  }

  /*                                                                      */ DPRINTFTS(rank, "before MPI_Finalize");
  barrier_after_malloc(MPI_COMM_WORLD);

  if ( (communicators.work_size>0) && (communicators.work_rank==0) ) {
    time(&t);
    fprintf(stderr, "SION parallel file I/O benchmark 'partest': end at %s\n", ctime(&t));
  }

  MPI_Finalize();
  return (0);
}

