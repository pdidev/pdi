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
#include "ompi_partest.h"
#include "partest_opts.h"

#include <omp.h>


#ifdef _SION_BGP
/* #include <mpix.h> */
#include <common/bgp_personality.h>
#include <common/bgp_personality_inlines.h>
#endif

#ifdef _SION_AIX
#include <unistd.h>
#endif

#ifdef _SION_LINUX
#include <unistd.h>
#endif

/*!
 * \file
 * \brief Benchmark/Test Tool
 * \author Wolfgang Frings
 * \author Ventsislav Petkov
 * \author David Montoya
 */



int barrier_after_start(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier
  return (1);
}

int barrier_after_malloc(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier

  return (1);
}

int barrier_after_open(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier
	return (1);
}

int barrier_after_write(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier
	return (1);
}

int barrier_after_read(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier
	return (1);
}

int barrier_after_close(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier
	return (1);
}

int barrier_before_unlink(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier
	return (1);
}

int barrier_after_unlink(MPI_Comm comm)
{
	#pragma omp master
		{
			MPI_Barrier(comm);
		}
	#pragma omp barrier
	return (1);
}

static char * __collective_print_pointer;

int collective_print_gather(char *cbuffer, MPI_Comm comm )
{
  int       rank, size, p;
  char     *lbuffer;
  int       num_threads = omp_get_num_threads();

	#pragma omp barrier

	#pragma omp master
	  {
		  __collective_print_pointer = (char *) malloc(MAXCHARLEN * num_threads);
	  }

	#pragma omp barrier

	 memcpy(__collective_print_pointer+(MAXCHARLEN * omp_get_thread_num()),cbuffer,MAXCHARLEN);

    #pragma omp barrier

	#pragma omp master
	{
	  MPI_Comm_size(comm, &size);
	  MPI_Comm_rank(comm, &rank);
	  if(rank==0) lbuffer = (char *) malloc(MAXCHARLEN * num_threads * size);
	  else        lbuffer = NULL;

	  MPI_Gather(__collective_print_pointer, MAXCHARLEN*num_threads, MPI_CHAR, lbuffer, MAXCHARLEN*num_threads, MPI_CHAR, 0, comm);


	  if (rank == 0) {

		for (p = 0; p < (size*num_threads); p++) {

		  fprintf(stderr, "%s", lbuffer + p * MAXCHARLEN);
		}
	  }

	  if(rank==0) free(lbuffer);

	  free(__collective_print_pointer);
	}
#pragma omp barrier

  return (1);
}


/*!\brief an ad hoc solution to the OpenMP reduction when outside the definition scope of the parallel block
 *
 * @param[in] *syncdata					in data
 * @param[out] *out						result
 * @param[in] *op						operator (supported: MPI_SUM/MPI_MAX)
 * @param[in] *dtype					data type (supported: _PARTEST_SION_INT32,_PARTEST_SION_INT64 (default),_PARTEST_DOUBLE)
 *
 */

static void * __thread_sync_pointer;

void reduce_omp(void *syncdata, void * out, MPI_Op op, int dtype)
{
	int thread_num = omp_get_thread_num();
	int num_threads = omp_get_num_threads();
	{
		#pragma omp barrier
	}
	#pragma omp master
	{
	  switch (dtype) {
		  case _PARTEST_SION_INT32:
			  {
				  __thread_sync_pointer = malloc(sizeof(sion_int32)*num_threads);
			  }
			break;
		  case _PARTEST_SION_INT64:
			  {
				  __thread_sync_pointer = malloc(sizeof(sion_int64)*num_threads);
			  }
			break;
		  case _PARTEST_DOUBLE:
			  {
				  __thread_sync_pointer = malloc(sizeof(double)*num_threads);
			  }
			break;
		  default:
			  {
				  __thread_sync_pointer = malloc(sizeof(sion_int64)*num_threads);
			  }
			break;
	  }
	}
	{
		#pragma omp barrier
	}
	switch (dtype) {
		case _PARTEST_SION_INT32:
			  {
				((sion_int32 *)__thread_sync_pointer)[thread_num] = * (sion_int32*)syncdata;
			  }
			break;
		case _PARTEST_SION_INT64:
			  {
				((sion_int64 *)__thread_sync_pointer)[thread_num] = *((sion_int64*)syncdata);
			  }
			break;
		case _PARTEST_DOUBLE:
			  {
				((double *)__thread_sync_pointer)[thread_num] = *(double*)syncdata;
			  }
			break;
		default:
			  {
				((sion_int64 *)__thread_sync_pointer)[thread_num] = *(sion_int64*)syncdata;
			  }
			break;
	}
	{
		#pragma omp barrier
	}

	#pragma omp master
		{
		int i;
		switch (dtype) {
			case _PARTEST_SION_INT32:
				  {
					if(op == MPI_SUM){
						*((sion_int32 *) out) = 0;
						for(i=0;i<num_threads;i++){
							*((sion_int32 *) out) += ((sion_int32 *)__thread_sync_pointer)[i];
						}
					}else if(op == MPI_MAX){
						*((sion_int32 *) out) = ((sion_int32 *)__thread_sync_pointer)[0];
						for(i=1;i<num_threads;i++){
							if(((sion_int32 *)__thread_sync_pointer)[i] > *((sion_int32 *) out)) *((sion_int32 *) out) = ((sion_int32 *)__thread_sync_pointer)[i];
						}
					}
				  }
				break;
			case _PARTEST_SION_INT64:
				  {
						if(op == MPI_SUM){
							*((sion_int64 *) out) = 0;
							for(i=0;i<num_threads;i++){
								*((sion_int64 *) out) += ((sion_int64 *)__thread_sync_pointer)[i];
							}
						}else if(op == MPI_MAX){
							*((sion_int64 *) out) = ((sion_int64 *)__thread_sync_pointer)[0];
							for(i=1;i<num_threads;i++){
								if(((sion_int64 *)__thread_sync_pointer)[i] > *((sion_int64 *) out)) *((sion_int64 *) out) = ((sion_int64 *)__thread_sync_pointer)[i];
							}
						}
				  }
				break;
			case _PARTEST_DOUBLE:
				  {
						if(op == MPI_SUM){
							*((double *) out) = 0;
							for(i=0;i<num_threads;i++){
								*((double *) out) += ((double *)__thread_sync_pointer)[i];
							}
						}else if(op == MPI_MAX){
							*((double *) out) = ((double *)__thread_sync_pointer)[0];
							for(i=1;i<num_threads;i++){
								if(((double *)__thread_sync_pointer)[i] > *((double *) out)) *((double *) out) = ((double *)__thread_sync_pointer)[i];
							}
						}
				  }
				break;
			default:
				  {
						if(op == MPI_SUM){
							*((sion_int64 *) out) = 0;
							for(i=0;i<num_threads;i++){
								*((sion_int64 *) out) += ((sion_int64 *)__thread_sync_pointer)[i];
							}
						}else if(op == MPI_MAX){
							*((sion_int64 *) out) = ((sion_int64 *)__thread_sync_pointer)[0];
							for(i=1;i<num_threads;i++){
								if(((sion_int64 *)__thread_sync_pointer)[i] > *((sion_int64 *) out)) *((sion_int64 *) out) = ((sion_int64 *)__thread_sync_pointer)[i];
							}
						}
				  }
				break;
		}
		free(__thread_sync_pointer);
	}
#pragma omp barrier
}

int split_communicator(_test_communicators * communicators, int bluegene, int bluegene_np, int numfiles, int read_task_offset, int verbose);


int main(int argc, char **argv)
{
  int       rank, size, rc = 0;
  char     *localbuffer;
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
  split_communicator(&communicators, options.bluegene, options.bluegene_np, options.numfiles, options.read_task_offset, options.verbose);

  /* determine global and local size of data to be written and read */
  sion_int64 num_threads = (sion_int64) omp_get_max_threads();
  MPI_Allreduce(&num_threads, &commwork_size64, 1, SION_MPI_INT64, MPI_SUM, communicators.work);

  if (options.globalsize > 0) {
    options.totalsize = (sion_int64) options.globalsize / commwork_size64;
  }
  else {
    options.globalsize = options.totalsize * commwork_size64;
  }

  if((options.totalsize>options.bufsize) || (options.read_task_offset>0)  || (options.do_write==0)) {
    options.suppress_checksum=1;
  }

  if(options.fsblksize<0) options.fsblksize=-1;

  if ( (communicators.work_size>0) && (communicators.work_rank==0) ) {
    time(&t);
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
    fprintf(stderr, "SION parallel file I/O benchmark 'ompi_partest': start at %s", ctime(&t));
    fprintf(stderr, "partest Number of MPI tasks that will use the file tasks:       running on %d tasks\n", size);
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
    fprintf(stderr, "partest parameter:   (-b/-B) local buffer size / sion task = %15lld bytes %10.3f MB\n", options.bufsize, options.bufsize / (1.0 MB));
    fprintf(stderr, "partest parameter:   (-g/-G) global total data size   = %15lld bytes %10.3f GB\n", options.globalsize, options.globalsize / (1024.0 MB));
    fprintf(stderr, "partest parameter:   (-s/-S) total data size / sion task   = %15lld bytes %10.3f MB\n", options.totalsize, options.totalsize / (1.0 MB));
    fprintf(stderr, "partest parameter:   (-r/-R) sion chunk size          = %15lld bytes %10.3f MB\n", options.chunksize, options.chunksize / (1.0 MB));
    fprintf(stderr, "partest parameter:   (-Q)    fs block size            = %15d bytes %10.3f MB\n", options.fsblksize, options.fsblksize / (1.0 MB));
    if (options.type == 0)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (sion OMPI, collective read)\n", options.type);
    if (options.type == 1)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (sion OMPI, independant read)\n", options.type);
    if (options.type == 2)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (sion OMP, collective read)\n", options.type);
    if (options.type == 3)
      fprintf(stderr, "partest parameter:   (-T)    test type                = %d (sion OMP, independant read)\n", options.type);
    fprintf(stderr, "partest parameter:   (-j)    serialize_blocknum       = %d\n", options.serialize_blocknum);
    fprintf(stderr, "partest parameter:   (-Z)    read task offset         = %d\n", options.read_task_offset);
    fprintf(stderr, "partest parameter:   (-o)    start offset bytes       = %d\n", options.startoffset);
    fprintf(stderr, "partest parameter:   (-v)    verbose                  = %d\n", options.verbose);
    fprintf(stderr, "partest parameter:   (-d)    debug                    = %d\n", options.debug);
    fprintf(stderr, "partest parameter:   (-D)    Debug                    = %d\n", options.Debug);
    fprintf(stderr, "partest parameter:   (-M)    collective write         = %d\n", options.collectivewrite);
    fprintf(stderr, "partest parameter:   (-m)    collective read          = %d\n", options.collectiveread);


    fprintf(stderr, "partest parameter:   (-P)    Blue Gene, I/O nodes     = %d\n", options.bluegene);
    fprintf(stderr, "partest parameter:   ()      Blue Gene: tasks/IO-node = %d\n", options.bluegene_np);
    fprintf(stderr, "partest parameter:   (-w)    MPI-IO, IBM, Large Block IO = %d\n", options.mpiio_lb);
    fprintf(stderr, "partest parameter:   (-W)    MPI-IO, IBM, IO bufsize     = %d KB\n", options.mpiio_bs);
    fprintf(stderr, "partest parameter:   (-x)    MPI-IO, IBM, sparse access  = %d\n", options.mpiio_sa);
    fprintf(stderr, "partest parameter:   (  )    OpenMP number of threads    = %d\n", omp_get_max_threads());
    fprintf(stderr, "partest parameter:   (  )    commwork_size64             = %lld\n", commwork_size64);
    fprintf(stderr, "partest parameter:   (  )    suppress_checksum           = %d\n", options.suppress_checksum);
    fprintf(stderr, "partest parameter:   (  )    do_write                    = %d\n", options.do_write);
    fprintf(stderr, "partest parameter:   (  )    do_read                     = %d\n", options.do_read);
    fprintf(stderr, "partest parameter:   (  )    use_posix                   = %d\n", options.use_posix);

  }

  barrier_after_start(MPI_COMM_WORLD);

  if ( (communicators.work_size>0) && (communicators.work_rank==0) ) {
    fprintf(stderr, "partest parameter:   (  )    comm(all)                   = %d of %d\n", communicators.all_rank, communicators.all_size);
    fprintf(stderr, "partest parameter:   (  )    comm(work)                  = %d of %d\n", communicators.work_rank, communicators.work_size);
    fprintf(stderr, "partest parameter:   (  )    comm(local)                 = %d of %d\n", communicators.local_rank, communicators.local_size);
    fprintf(stderr, "------------------------------------------------------------------------------------------\n");
  }
	#pragma omp parallel private(localbuffer,t)
	  {
		 barrier_after_start(MPI_COMM_WORLD);
		  char l_filename[MAXCHARLEN];
		  strcpy(l_filename,options.filename);

		  _test_communicators local_communicators = communicators;
		  _test_options local_options = options;
		  /*                                                                      */ DPRINTFTS(rank, "after pstart");
		  /* Init the local buffer that will be written to the file */
		  localbuffer = (char *) malloc(options.bufsize);

		  srand(time(NULL)*local_communicators.work_rank*omp_get_thread_num());
		  /* for (i = 0; i < (options.bufsize / sizeof(int)); i++) */
		  /*   localbuffer[i] = (char) rand() % 256; */

		/*   memset (localbuffer, 'a'+rank%26, bufsize);  */
		  memset (localbuffer, 'a'+rank%26, local_options.bufsize);
		  barrier_after_malloc(MPI_COMM_WORLD);
		  /*                                                                      */ DPRINTFTS(rank, "after malloc");

		  /* random factor handling */

		  if(local_options.factor>0.0) {
			if((local_options.collectivewrite) || (local_options.collectiveread)) {
			  if(local_options.bufsize<local_options.totalsize*(1+local_options.factor)) {
				  #pragma omp master
				  {
					  fprintf(stderr, "partest: ERROR deadlock possible if collective read/write and random factor used, and buffer is too small aborting\n");
					  MPI_Abort(MPI_COMM_WORLD,0);
				  }
#pragma omp barrier
				exit(0);
			  }
			}

			local_options.totalsize += ((sion_int64) (local_options.factor * (sion_int64) local_options.totalsize * (sion_int64) rand() / (sion_int64) RAND_MAX));
			local_options.chunksize += ((sion_int64) (local_options.factor * (sion_int64) local_options.totalsize * (sion_int64) rand() / (sion_int64) RAND_MAX));
			fprintf(stderr, "partest parameter:   (  )    new totalsize[%4d,t%4d]          = %lld\n", local_communicators.work_rank,omp_get_thread_num(),local_options.totalsize);
		  }



		  /*                                                                      */ DPRINTFTS(rank, "before scall2");
		  #pragma omp master
		  {
			  if ( (local_communicators.work_size>0) && (local_communicators.work_rank==0) ) {
				fprintf(stderr, "partest parameter:   (  )    new totalsize                = %lld\n", local_options.totalsize);
			  }
		  }
#pragma omp barrier
		  barrier_after_malloc(MPI_COMM_WORLD);
		  if (local_options.type == 0) {
			  local_options.collectiveopenforread = 1;
			  test_paropen_multi_ompi(l_filename, localbuffer, &local_communicators, &local_options);

		  }else if(local_options.type == 1) {
			  local_options.collectiveopenforread = 0;
			  test_paropen_multi_ompi(l_filename, localbuffer, &local_communicators, &local_options);
		  }else if(local_options.type == 2) {
			  local_options.collectiveopenforread = 1;
			  test_paropen_omp(l_filename, localbuffer, &local_communicators, &local_options);
		  }else if(local_options.type == 3) {
			  local_options.collectiveopenforread = 0;
			  test_paropen_omp(l_filename, localbuffer, &local_communicators, &local_options);
		  }

		  /*                                                                      */ DPRINTFTS(rank, "before MPI_Finalize");
		  barrier_after_malloc(MPI_COMM_WORLD);
		  #pragma omp master
		  {
			  if ( (local_communicators.work_size>0) && (local_communicators.work_rank==0) ) {
				  time(&t);
				  fprintf(stderr, "SION parallel file I/O benchmark 'ompi_partest': end at %s\n", ctime(&t));
			  }
		  }
#pragma omp barrier
	  }

	 MPI_Finalize();
  return (0);
}


int split_communicator(_test_communicators * communicators, int bluegene, int bluegene_np, int numfiles, int read_task_offset, int verbose)
{
  int       proc_per_file;

  communicators->work_size = communicators->work_rank = -2;
  communicators->local_size = communicators->local_rank = -2;



#ifdef _SION_BGP
  if (bluegene) {               /* order MPI-tasks by I/O-node */
    _BGP_Personality_t personality;
    MPI_Comm  commSame, commDiff;
    int       sizeSame, sizeDiff;
    int       rankSame, rankDiff;
    char      location[128];
    unsigned  procid, x, y, z, t;
    char      cbuffer[MAXCHARLEN];

    /* get location information */
    Kernel_GetPersonality(&personality, sizeof(personality));
    BGP_Personality_getLocationString(&personality, location);
    procid = Kernel_PhysicalProcessorID();
    MPIX_rank2torus(communicators->all_rank, &x, &y, &z, &t);

    /* task of communicator working with different I/O-nodes */
    MPIX_Pset_diff_comm_create(&commDiff);
    MPI_Comm_size(commDiff, &sizeDiff);
    MPI_Comm_rank(commDiff, &rankDiff);
    communicators->ionode_number = rankDiff;

    /* communicator consists of all task working with the same I/O-node */
    MPIX_Pset_same_comm_create(&commSame);
    MPI_Comm_size(commSame, &sizeSame);
    MPI_Comm_rank(commSame, &rankSame);

    /*  if -p not specified all proc will write! */
    if (bluegene_np == 0) {
      bluegene_np = sizeSame;
    }

    /*  Get a communicator with all writing tasks => new global communicator */
    MPI_Comm_split(communicators->all, (rankSame < bluegene_np), communicators->all_rank, &communicators->work);
    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
    if (rankSame >= bluegene_np) {
      /* not working task */
      communicators->work_size = communicators->work_rank = -1;
      communicators->local_size = communicators->local_rank = -1;
    }

    /*  If only one file will be used => dont split further */
    /*  if numfile > 1 sion will generate correct local communicator */
    if (numfiles >= 1) {
      communicators->local = communicators->work;
    }
    else if(numfiles<0) {
      if(numfiles==-1) {
	/*  Split the common communicator for each IO node to get a local comm with only the writing tasks for this IO Node */
	MPI_Comm_split(commSame, (rankSame < bluegene_np), communicators->all_rank, &communicators->local);
      } else {
	/*  local communicator contains only one task per IO-node */
	/* bluegene_np has to be 512 */
	communicators->local=commDiff;
      }
    } 
    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);

    /* determine filenumber */
    if (numfiles < 1) {
      /* one file per I/O-node */
      if(numfiles==-1)  communicators->file_number = rankDiff;
      else              communicators->file_number = rankSame;
    }
    else {
      communicators->file_number = -1;
    }

    /* print log message about location, ...  */
    sprintf(cbuffer, "");
    if (rankSame < bluegene_np) {
      if (verbose) {
        sprintf(cbuffer, "BGP[%05d] diff_comm: %4d of %4d  same_comm: %5d of %5d file_comm: %5d of %5d %s phys_xyzt(%ud,%ud,%ud,%ud)\n",
                communicators->all_rank, rankDiff + 1, sizeDiff, rankSame + 1, sizeSame, communicators->local_rank + 1, communicators->local_size,
                location, x, y, z, t);
      }
    }
    collective_print_gather(cbuffer, communicators->work);

  }
#endif

#ifdef _SION_AIX
  /* no communicator adjustment */
#endif

  /* initial set of communicators */
  if (communicators->work_size == -2) {
    /* all task do work */
    communicators->work = communicators->all;
    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
  }
  /* local communicators  */
  if (communicators->local_size == -2) {
    if (numfiles == 1) {
      communicators->local = communicators->work;
    }
    /* set a default distribution on files, will be computed again by sion_open */
    if (numfiles < 1) {
      numfiles = communicators->work_size / 2;
      if (numfiles == 0)
        numfiles = 1;
    }
    proc_per_file = communicators->work_size / numfiles;

    /* remaining tasks are write/read to/from the last file */
    if (communicators->work_rank >= (numfiles * proc_per_file)) {
      communicators->file_number = numfiles - 1;
    }
    else {
      communicators->file_number = communicators->work_rank / proc_per_file;
    }

    MPI_Comm_split(communicators->work, communicators->file_number, communicators->all_rank, &communicators->local);

    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);

    communicators->ionode_number = communicators->file_number;

  }

#ifdef _SION_LINUX
  if (verbose) {
    char      location[256];
    gethostname(location, 256);
    char      cbuffer[MAXCHARLEN];
    sprintf(cbuffer, "LINUX[%03d] diff_comm: %4d of %4d  same_comm: %4d of %4d file_comm: %4d of %4d %s\n",
            communicators->all_rank, communicators->all_rank, communicators->all_size,
            communicators->work_rank, communicators->work_size, communicators->local_rank, communicators->local_size, location);
    collective_print_gather(cbuffer, communicators->all);
  }

#endif

#ifdef _SION_AIX
  if (verbose) {
    char      location[256];
    gethostname(location, 256);
    int       sizeSame = 0, sizeDiff = 0;
    char      cbuffer[MAXCHARLEN];
    sprintf(cbuffer, "AIX[%03d] diff_comm: %4d of %4d  same_comm: %4d of %4d file_comm: %4d of %4d %s\n",
            communicators->all_rank, communicators->all_rank, communicators->all_size,
            communicators->work_rank, communicators->work_size, communicators->local_rank, communicators->local_size, location);
    collective_print_gather(cbuffer, communicators->all);
  }

#endif

  /* shift working tasks  */
  if (communicators->work_size != -1) {
    /* only if task in communicator work */
    int newtasknr;
    newtasknr=(communicators->work_rank+read_task_offset)%communicators->work_size;
    MPI_Comm_split(communicators->work, 0, newtasknr, &communicators->workread);

    MPI_Comm_size(communicators->workread, &communicators->workread_size);
    MPI_Comm_rank(communicators->workread, &communicators->workread_rank);
   /* printf("WF: %d %d %% %d-> %d (%d %d)\n",
	   communicators->work_rank,read_task_offset,
	   communicators->work_size,newtasknr,
	   communicators->workread_rank,communicators->workread_size);*/
   
  } else {
    /* this rtask will not be used for reading */
    communicators->workread_size = communicators->workread_rank = -1;
    communicators->local_size = communicators->local_rank = -1;
  }

  return(1);
}
