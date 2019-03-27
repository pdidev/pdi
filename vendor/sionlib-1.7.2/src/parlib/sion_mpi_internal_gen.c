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

#include "sion_mpi_internal_gen.h"


#ifdef SION_MPI


int _sion_errorprint_mpi(int rc, int level, const char *format, ...)
{
  int     rank=-1, thread=-1;
  va_list ap;

#ifdef SION_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
#endif

  va_start(ap, format);
  rc=__sion_errorprint_vargs(rc, level, rank, thread, format, ap);
  va_end(ap);
  
  return (rc);
}


/* Help Functions */

/*!\brief Splits a Communicator in numfiles different communicators
 *
 * @param[in]    numFiles     number of files
 * @param[in]    gComm        global communicator
 * @param[out]   filenumber   file number
 * @param[out]   lrank        local rank
 * @param[out]   lsize        local size
 * 
 */

int _sion_gen_info_from_gcomm_mpi(int numFiles, MPI_Comm gComm, int *filenumber, int *lrank, int *lsize)
{
  int       gtasks, gRank;
  int       rc = SION_SUCCESS;
  int       task_per_file;


  MPI_Comm_size(gComm, &gtasks);
  MPI_Comm_rank(gComm, &gRank);

  if (gtasks < numFiles) {
    return(_sion_errorprint_mpi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
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
  
  DPRINTFP((1, "_sion_get_info_from_splitted_comm_mpi", gRank, "Global communicator divided in %d local communicators (%f: %d of %d)\n", 
	    numFiles,*filenumber,*lrank,*lsize));

  return (rc);
}

int _sion_get_info_from_splitted_comm_mpi(MPI_Comm gComm, MPI_Comm lComm, int *numComm, int *CommNumber, int *lrank, int *lsize) {
  int       gSize, gRank, lSize, lRank;
  int      *allRanks=NULL, *allSizes=NULL, i, ntasks;
  int       ncomms, color = 0;
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
      return(_sion_errorprint_mpi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_get_info_from_splitted_comm_mpi: Cannot allocate temp arrays of size %lu\n", (unsigned long) gSize * sizeof(int)));
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

int _sion_get_numfiles_from_file_mpi(char *fname) {

  int       rank, sid;
  FILE     *fileptr;
  int       numfiles = 0;
  sion_int64 chunksize;
  sion_int32 fsblksize;

  rank = 0;
  sid = _sion_open_rank(fname, "br", &chunksize, &fsblksize, &rank, &fileptr);

  numfiles = sion_get_number_of_files(sid);

  _sion_close_sid(sid);

  DPRINTFP((1, "_sion_get_numfiles_from_file_mpi", _SION_DEFAULT_RANK, "sion file %s has %d files\n", fname, numfiles));

  return (numfiles);
}

int _sion_get_filenumber_from_files_mpi(char *fname, MPI_Comm gComm, int *numfiles, int *filenumber, int *lRank) {

  int       sid, gSize, gRank, ntasks, nfiles;
  int       rc = SION_SUCCESS;
  FILE     *fileptr;
  sion_int64 *chunksizes;
  sion_int32 fsblksize;
  int      *globalranks;
  int         mapping_size = 0;
  sion_int32 *mapping = NULL;
  sion_int32 lpos[2];

  MPI_Comm_size(gComm, &gSize);
  MPI_Comm_rank(gComm, &gRank);

  DPRINTFP((1, "_sion_get_filenumber_from_files_mpi", gRank, "before open\n"));
  if(gRank == 0) {
    /* open and get mapping of sion file */
    chunksizes=NULL;globalranks=NULL;
    sid=_sion_open_read(fname,_SION_FMODE_READ|_SION_FMODE_ANSI,_SION_READ_MASTER_ONLY_OF_MULTI_FILES,
			&ntasks,&nfiles,&chunksizes,&fsblksize,&globalranks,&fileptr);
    /* sid = sion_open(fname, "br", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &fileptr); */
    if(sid>=0) {
      DPRINTFP((1, "_sion_get_filenumber_from_files_mpi", gRank, "after open\n"));
      rc=sion_get_mapping(sid,&mapping_size,&mapping,numfiles);
      DPRINTFP((1, "_sion_get_filenumber_from_files_mpi", gRank, "sion file %d files rc=%d\n", *numfiles, rc));
    } else {
      *numfiles=-1;
    }
  }
  
  /* each task has to know if more than file was used in sion file */
  MPI_Bcast(numfiles, 1, MPI_INT, 0, gComm);

  if((gRank == 0) && (*numfiles>1)) {
    if(mapping_size!=gSize) {
      return(_sion_errorprint_mpi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_get_filenumber_from_files_mpi: Incorrect sum of ntasks of files %d <> %d\n", mapping_size, gSize));
    }
  }

  if(*numfiles<0) {
    return(_sion_errorprint_mpi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_get_filenumber_from_files_mpi: could not get numfiles from sion file\n"));
  }

  if(*numfiles>1) {
    DPRINTFP((1, "_sion_get_filenumber_from_files_mpi", gRank, "before scatter\n"));
    MPI_Scatter(mapping, 2, SION_MPI_INT32, lpos, 2, SION_MPI_INT32, 0, gComm);
    *filenumber=lpos[0];
    *lRank     =lpos[1];
    DPRINTFP((1, "_sion_get_filenumber_from_files_mpi", gRank, "after scatter\n"));
  } else {
    *filenumber=0;
    *lRank     =gRank;
    DPRINTFP((1, "_sion_get_filenumber_from_files_mpi", gRank, "only one file -> filenumber=%d lRank=%d\n",*filenumber,*lRank));
  }

  if(gRank == 0) {
    /* frees also mapping vector */
    if (sid>=0) _sion_close_sid(sid);
  }

  DPRINTFP((1, "_sion_get_filenumber_from_files_mpi", gRank, "close rc=%d\n",rc));
    return(rc);
}

/* end of ifdef MPI */
#endif
