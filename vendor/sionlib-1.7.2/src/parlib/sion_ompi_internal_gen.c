/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

/*!
 * \file
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "mpi.h"
#include "omp.h"

#include <sys/time.h>

#include <sys/types.h>
#include <fcntl.h>

#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"

#include "sion_ompi_internal_gen.h"

#ifdef SION_OMPI

#include "sion_ompi.h"

int _sion_gen_info_from_gcomm_ompi(int numFiles, MPI_Comm gComm, int *filenumber, int *lrank, int *lsize)
{
  int       gtasks, gRank;
  int       rc = SION_SUCCESS;
  int       task_per_file;


  MPI_Comm_size(gComm, &gtasks);
  MPI_Comm_rank(gComm, &gRank);
  DPRINTFP((1, "_sion_get_info_from_splitted_comm_mpi", gRank, "enter: gcomm: %d of %d, numfiles=%d\n", 
	    gRank,gtasks,numFiles));

  if (gtasks < numFiles) {
    return(_sion_errorprint_ompi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
				 "_sion_gen_info_from_gcomm_mpi: Number of files(%d) cannot be bigger the the number of tasks(%d), aborting...\n", 
				numFiles, gtasks));
  }

  task_per_file = gtasks / numFiles;

  /* remaining tasks are added to last communicator */
  if (gRank >= (numFiles * task_per_file)) {
    *filenumber = numFiles - 1;
  }
  else {
    *filenumber = gRank / task_per_file;
  }

  if(*filenumber == numFiles - 1) {
    *lrank=gRank-(numFiles - 1)*task_per_file;
    *lsize=gtasks-(numFiles - 1)*task_per_file;
  } else {
    *lrank=gRank%task_per_file;
    *lsize=task_per_file;
  }
  
  DPRINTFP((1, "_sion_get_info_from_splitted_comm_mpi", gRank, "Global communicator divided in %d local communicators (%d: %d of %d)\n", 
	    numFiles,*filenumber,*lrank,*lsize));

  return (rc);
}

int _sion_get_info_from_splitted_comm_ompi(MPI_Comm gComm, MPI_Comm lComm, int *numComm, int *CommNumber, int *lrank, int *lsize) {
  int       gSize, gRank, lSize, lRank;
  int      *allRanks=NULL, *allSizes=NULL, i, ntasks;
  int       ncomms, color;
  int       rc = SION_SUCCESS;
  MPI_Status status;

  MPI_Comm_size(gComm, &gSize);
  MPI_Comm_rank(gComm, &gRank);

  if (lComm != MPI_COMM_NULL) {
    MPI_Comm_size(lComm, &lSize);
    MPI_Comm_rank(lComm, &lRank);
  }
  else {
    lSize = gSize;
    lRank = gRank;
  }

  DPRINTFP((32, "_sion_get_info_from_splitted_comm_mpi", gRank, "lRank: %d\n", lRank));

  if (gRank == 0) {

    allRanks = (int *) malloc(gSize * sizeof(int));
    allSizes = (int *) malloc(gSize * sizeof(int));
    if ((allRanks == NULL) || (allSizes == NULL)) {
      free(allRanks);
      free(allSizes);
      return(_sion_errorprint_ompi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_get_info_from_splitted_comm_mpi: Cannot allocate temp arrays of size %lu\n", (unsigned long) gSize * sizeof(int)));
    }
  }

  /*  Gather all local ranks to task 0 for counting.. */
  MPI_Gather(&lRank, 1, MPI_INT, allRanks, 1, MPI_INT, 0, gComm);
  MPI_Gather(&lSize, 1, MPI_INT, allSizes, 1, MPI_INT, 0, gComm);

  ncomms = 0;
  ntasks = 0;
  if (gRank == 0) {
    for (i = 0; i < gSize; i++) {
      if (allRanks[i] == 0) {
        /*  Use the current number of the communicator for the file suffix!! */
        if (i != 0) {
          MPI_Send(&ncomms, 1, MPI_INT, i, i, gComm);
          DPRINTFP((32, "sion_paropen_comms_mpi", gRank, "Sent ncomms=%05d to %d with TAG %d\n", ncomms, i, i));
        }
        else {
          /* it's my self */
          color = ncomms;
        }
        ncomms++;
        ntasks += allSizes[i];
      }
    }
  }
  /*  Recv the num of communicator from global 0 =>used for the suffix */
  if ((lRank == 0) && (gRank != 0)) {
    MPI_Recv(&color, 1, MPI_INT, 0, gRank, gComm, &status);
    DPRINTFP((32, "sion_paropen_comms_mpi", gRank, "Received ncomms=%05d from %d with TAG %d\n", color, status.MPI_SOURCE, status.MPI_TAG));

  }

  MPI_Bcast(&ncomms, 1, MPI_INT, 0, gComm);
  MPI_Bcast(&ntasks, 1, MPI_INT, 0, gComm);


  DPRINTFP((32, "_sion_get_info_from_splitted_comm_mpi", gRank, "#Comms=%05d #Tasks=%05d #TotalTasks=%05d\n", ncomms, ntasks, gSize));

  if (lComm != MPI_COMM_NULL) {
    MPI_Bcast(&color, 1, MPI_INT, 0, lComm);
  }

  if (gRank == 0) {
    if(allRanks) free(allRanks);
    if(allSizes) free(allSizes);
  }
  /* return parameters */
  *numComm    = ncomms;
  *CommNumber = color;
  *lrank = lRank;
  *lsize = lSize;

  return (rc);
}

int _sion_errorprint_ompi(int rc, int level, const char *format, ...)
{
  int       rank=-1;
  va_list ap;

  int thread = omp_get_thread_num();
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
 
  va_start(ap, format);
  rc=__sion_errorprint_vargs(rc, level, rank, thread, format, ap);
  va_end(ap);

  return (rc);
}


/* Help Functions */

int _sion_map_rank_ompi_to_mpi(int ompi_rank, int num_threads) {
  int mpi_rank = 0;

  mpi_rank = (int) (ompi_rank / num_threads);

  return mpi_rank;
}

int _sion_map_rank_ompi_to_thread_num(int ompi_rank, int num_threads) {
  int thread_num = 0;

  thread_num = (int) (ompi_rank % num_threads);

  return thread_num;
}


int _sion_map_rank_mpi_to_ompi(int mpi_rank, int num_threads, int thread_num) {
  int ompi_rank = 0;

  ompi_rank = mpi_rank * num_threads + thread_num;

  return ompi_rank;
}

int _sion_get_size_ompi(int mpi_size, int num_threads) {
  int ompi_size = 0;

  ompi_size = mpi_size * num_threads;

  return ompi_size;
}


/* end of ifdef OMPI */
#endif
