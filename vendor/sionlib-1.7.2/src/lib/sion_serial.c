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
 *
 * \ref api_page
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <sys/time.h>

#include "sion_debug.h"
#include "sion_filedesc.h"
#include "sion_internal.h"
#include "sion_printts.h"

#include "sion_const.h"
#include "sion_common.h"
#include "sion_serial.h"


/*!
 * @brief Open a sion file in serial mode.
 *
 * See \ref sion_open_description.
 *
 * @param[in]       fname         name of file, should be equal on all tasks
 * @param[in]       file_mode     like the type parameter of fopen. See \ref file_mode_description .
 * @param[in,out]   ntasks        number of tasks used to write this file
 * @param[in,out]   nfiles        number of physical files
 * @param[in,out]   chunksizes    chunksize for each task
 * @param[in,out]   fsblksize     blocksize of filesystem, must be equal on all processors
 * @param[in]       globalranks   rank numbers for which the file should be opened;
 *                                will be stored in sion file, useful if comm is not MPI_COMM_WORLD
 *                                typical: globalrank = rank in MPI_COMM_WORLD
 * @param[out]      fileptr       file pointer for this task
 *
 * @retval          sid           sion file handle or -1 if error occured
 */
int sion_open(char *fname, const char *file_mode, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr)
{

  int     sid=0;

  DPRINTFP((1, "sion_open", 0, "enter open of file %s in %s mode\n", fname, file_mode));
  
  sid=_sion_open(fname,file_mode,ntasks,nfiles,chunksizes,fsblksize,globalranks,fileptr);

  return (sid);

}

/*!
 * @brief Open a sion file for a specific rank
 *
 * See \ref sion_open_rank_description
 *
 * @param[in]           fname           name of file, should be equal on all tasks
 * @param[in,out]       file_mode       like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in,out]       chunksize       chunksize for this task
 * @param[in,out]       fsblksize       blocksize of filesystem, must be equal on all processors
 * @param[in]           rank            rank number for which the file should be open;
 *                                      will be stored in sion file, usefull if comm is not MPI_COMM_WORLD
 *                                      typical: globalrank= rank in MPI_COMM_WORLD
 * @param[out]          fileptr         file pointer for this task
 *
 * @retval              sid             sion file handle or -1 if error occured
 */
int sion_open_rank(char *fname, const char *file_mode, sion_int64 *chunksize, sion_int32 *fsblksize, int *rank, FILE **fileptr)
{
  int             sid;

  /*                                                                      */ DPRINTFTS(*rank, "before open rank");
  DPRINTFP((1, "sion_open_rank", *rank, "enter open of file %s in %s mode\n", fname, file_mode));
  sid=_sion_open_rank(fname, file_mode, chunksize, fsblksize, rank, fileptr);
  DPRINTFP((1, "sion_open_rank", 0, "leave open of file %s in %s mode sid=%d\n", fname, file_mode,sid));
  /*                                                                      */ DPRINTFTS(*rank, "after open rank");

  return (sid);

}

/*!
 * @brief Close a sion file.
 *
 * See \ref sion_close_description .
 *
 *  @param[in]  sid     sion file handle
 *
 *  @return     SION_SUCCESS if close is ok
 */
int sion_close(int sid)
{

  int       rc = 0;
  DPRINTFP((1, "sion_close", -1, "enter close   sid=%d\n", sid));
  rc=_sion_close_sid(sid);
  DPRINTFP((1, "sion_close", -1, "leave close   sid=%d\n", sid));

  sion_dclose();

  return (rc);
}
