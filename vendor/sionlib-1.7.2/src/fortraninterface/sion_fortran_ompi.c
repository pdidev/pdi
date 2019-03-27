/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*!
 * @file sion_fortran_ompi.c
 *
 * @brief Fortran API
 *
 * @author Ventsislav Petkov
 * @author David Montoya
 */

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpi.h"

#include "sion.h"
#include "sion_debug.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_ompi.h"
#include "sion_fortran_ompi.h"

/*!
 * @brief Fortran wrapper function that calls sion_paropen_mpi for 1 file
 *
 * @param[in]           fname           name of file, should equal on all tasks
 * @param[in]           file_mode       like the type parameter of fopen ("rb", "wb")
 * @param[in]           numFiles        number of files to open
 * @param[in]           fgComm          global MPI communicator, which contains all tasks writing to the files
 *                                      typical: MPI_COMM_WORLD
 * @param[in,out]       flComm          new local MPI communicator, which contains all tasks writing to the current file
 * @param[in,out]       chunksize       chunksize for this task
 * @param[in,out]       fsblksize       filesystem blocksize, must be equal on all processors
 * @param[in,out]       globalrank      any global unique id for this task
 *                                      will be stored in sion file, usefull if comm is not MPI_COMM_WORLD
 *                                      typical: globalrank= rank in MPI_COMM_WORLD
 *
 * @param[in]           fname_len       (internal) length of the fname string *
 * @param[in]           file_mode_len   (internal) length of the file_mode string
 * @param[in]           newfname_len    (internal) length of the newfname string
 *
 * @param[out]          newfname        filename of the new file
 * @param[out]          sid             sion file handle or -1 if error occured
 */
void fsion_paropen_ompi_c(char       *fname,
                          char       *file_mode,
                          int        *numFiles,
                          MPI_Fint   * fgComm,
                          MPI_Fint   * flComm,
                          sion_int64 *chunksize, 
		          sion_int32 *fsblksize, 
		          int        *globalrank, 
		          char       *newfname,
		          int        *sid, 
		          int        fname_len, 
		          int        file_mode_len,
		          int        newfname_len) 
{
  char     *fname_tmp,*fmode_tmp;
  MPI_Comm  cgComm,clComm;

  fname_tmp = (char *) malloc((size_t) ((fname_len + 1) * sizeof(char)));
  fmode_tmp = (char *) malloc((size_t) ((file_mode_len + 1) * sizeof(char)));

  /* Copy the strings to the new buffers and pad with nulls */
  strncpy(fname_tmp, fname, fname_len);
  strncpy(fmode_tmp, file_mode, file_mode_len);

  fname_tmp[fname_len] = '\0';
  fmode_tmp[file_mode_len] = '\0';

  cgComm = MPI_Comm_f2c(*fgComm);
  clComm = MPI_Comm_f2c(*flComm); 

#ifdef SION_DEBUG_UNUSED
#pragma omp master
  {
	  if (*globalrank == 0){
		if (cgComm == MPI_COMM_WORLD) fprintf(stderr,"COMM_WORLD selected\n");
		fprintf(stderr,"fsion_paropen_ompi_c: filename_len: %d\n", (int) fname_len);
		fprintf(stderr,"fsion_paropen_ompi_c: filename: %s\n",fname_tmp);

		fprintf(stderr,"fsion_paropen_ompi_c: filemode_len: %d\n", (int) file_mode_len);
		fprintf(stderr,"fsion_paropen_ompi_c: filemode: %s\n",fmode_tmp);

		fprintf(stderr,"fsion_paropen_ompi_c: chunksize: %lld\n",*chunksize);
		fprintf(stderr,"fsion_paropen_ompi_c: fsblksize: %d\n",*fsblksize);
		fprintf(stderr,"fsion_paropen_ompi_c: numFiles:  %d\n",*numFiles);
	  }
  }
#pragma omp barrier
#endif

  (*sid) = sion_paropen_ompi(fname_tmp, fmode_tmp, numFiles, cgComm, &clComm, chunksize, fsblksize, globalrank, NULL, NULL);

  *flComm = MPI_Comm_c2f(clComm);

  /* Free the used memory */
  free(fname_tmp);
  free(fmode_tmp);

}

/*!
 * @brief Fortran procedure to open multiple sion files in parallel.
 *
 * This function opens numFiles files in parallel. It has to be called from
 * each processor at the same time (like a MPI collective operation).
 *
 * @param[in]           fname           name of file, should equal on all tasks
 * @param[in]           file_mode       like the type parameter of fopen ("rb", "wb")
 * @param[in]           numFiles        number of files to open
 * @param[in]           fgComm          global MPI communicator, which contains all tasks writing to the files
 *                                      typical: MPI_COMM_WORLD
 * @param[in,out]       flComm          new local MPI communicator, which contains all tasks writing to the current file
 * @param[in,out]       chunksize       chunksize for this task
 * @param[in,out]       fsblksize       filesystem blocksize, must be equal on all processors
 * @param[in,out]       globalrank      any global unique id for this task
 *                                      will be stored in sion file, usefull if comm is not MPI_COMM_WORLD
 *                                      typical: globalrank= rank in MPI_COMM_WORLD
 *
 * @param[in]           fname_len       (internal) length of the fname string *
 * @param[in]           file_mode_len   (internal) length of the file_mode string
 * @param[in]           newfname_len    (internal) length of the newfname string
 *
 * @param[out]          newfname        filename of the new file
 * @param[out]          sid             sion file handle or -1 if error occured
 */
void fsion_paropen_multi_ompi_c(char       *fname,
                                char       *file_mode,
                                int        *numFiles,
                                MPI_Fint   *fgComm,
                                MPI_Fint   *flComm,
                                sion_int64 *chunksize,
                                sion_int32 *fsblksize,
                                int        *globalrank, 
                                int        *sid, 
                                char       *newfname, 
                                int        fname_len, 
                                int        file_mode_len, 
                                int        newfname_len)
{
  MPI_Comm  cgComm, clComm;

  /* Fortran strings are not NULL-terminated => insert \0 at the end */
  char     *fname_tmp, *fmode_tmp, *newfname_tmp;


  fname_tmp = (char *) malloc((size_t) ((fname_len + 1) * sizeof(char)));
  fmode_tmp = (char *) malloc((size_t) ((file_mode_len + 1) * sizeof(char)));
  newfname_tmp = (char *) malloc((size_t) ((newfname_len + 1) * sizeof(char)));

  /* Copy the strings to the new buffers and pad with nulls */
  strncpy(fname_tmp, fname, fname_len);
  strncpy(fmode_tmp, file_mode, file_mode_len);

  fname_tmp[fname_len] = '\0';
  fmode_tmp[file_mode_len] = '\0';
  newfname_tmp[newfname_len] = '\0';
  strncpy(newfname_tmp, newfname, newfname_len);

  cgComm = MPI_Comm_f2c(*fgComm);

  (*sid) = sion_paropen_ompi(fname_tmp, fmode_tmp, numFiles, cgComm, &clComm, chunksize, fsblksize, globalrank, NULL, &newfname_tmp);

  *flComm = MPI_Comm_c2f(clComm);
  strncpy(newfname, newfname_tmp, newfname_len);

  /* Free the used memory */
  free(fname_tmp);
  free(fmode_tmp);
  free(newfname_tmp);
}

/*!
 * @brief Fortran procedure to close a sion file opened in OpenMP/MPI in parallel.
 *
 * This function closes the sion in parallel on all tasks included in comm.
 * The communicator should be the same used in the parallel open statement.
 *
 * @param[in]   sid     sion file handle
 *
 * @retval      ierr    1 if close is ok
 */
void fsion_parclose_ompi_c(int *sid, 
                           int *ierr)
{
  (*ierr) = sion_parclose_ompi(*sid);
}

