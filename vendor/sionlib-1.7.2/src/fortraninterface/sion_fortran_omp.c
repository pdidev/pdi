/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*!
 * @file sion_fortran_omp.c
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

#include "sion.h"
#include "sion_debug.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_omp.h"
#include "sion_fortran_omp.h"

/*!
 * @brief Fortran wrapper function that calls sion_paropen_omp
 *
 * @param[in]           fname           name of file, should equal on all tasks
 * @param[in]           file_mode       like the type parameter of fopen ("rb", "wb")
 * @param[in,out]       chunksize       chunksize for this task
 * @param[in,out]       fsblksize       filesystem blocksize, must be equal on all processors
 * @param[in,out]       globalrank      any global unique id for this task
 *                                      will be stored in sion file
 *
 * @param[out]          newfname        filename of the new file
 * @param[in]           fname_len       (internal) length of the fname string *
 * @param[in]           file_mode_len   (internal) length of the file_mode string
 * @param[out]          newfname_len    (internal) length of the newfname string
 *
 * @param[out]          sid             sion file handle or -1 if error occured
 */

void fsion_paropen_omp_c(char       *fname,
                         char       *file_mode,
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

  fname_tmp = (char *) malloc((size_t) ((fname_len + 1) * sizeof(char)));
  fmode_tmp = (char *) malloc((size_t) ((file_mode_len + 1) * sizeof(char)));

  /* Copy the strings to the new buffers and pad with nulls */
  strncpy(fname_tmp, fname, fname_len);
  strncpy(fmode_tmp, file_mode, file_mode_len);

  fname_tmp[fname_len] = '\0';
  fmode_tmp[file_mode_len] = '\0';

#ifdef SION_DEBUG_UNUSED
  if (*globalrank == 0){
    fprintf(stderr,"fsion_paropen_omp: filename_len: %d\n", (int) fname_len);
    fprintf(stderr,"fsion_paropen_omp: filename: %s\n",fname_tmp);

    fprintf(stderr,"fsion_paropen_omp: filemode_len: %d\n", (int) file_mode_len);
    fprintf(stderr,"fsion_paropen_omp: filemode: %s\n",fmode_tmp);
    
    fprintf(stderr,"fsion_paropen_omp: chunksize: %lld\n",*chunksize);
    fprintf(stderr,"fsion_paropen_omp: fsblksize: %d\n",*fsblksize);
  }
#endif

  (*sid) = sion_paropen_omp(fname_tmp, fmode_tmp, chunksize, fsblksize, globalrank, NULL, NULL);


  /* Free the used memory */
  free(fname_tmp);
  free(fmode_tmp);

}

/*!
 * @brief Fortran procedure to close a sion file opened with OpenMP in parallel.
 *
 * This function closes the sion in parallel on all tasks included in comm.
 * The communicator should be the same used in the parallel open statement.
 *
 * @param[in]   sid     sion file handle
 *
 * @retval      ierr    1 if close is ok
 */
void fsion_parclose_omp_c(int *sid, 
                          int *ierr)
{
  (*ierr) = sion_parclose_omp(*sid);
}

