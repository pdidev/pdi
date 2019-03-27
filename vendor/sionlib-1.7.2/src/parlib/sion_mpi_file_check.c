/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*
 *
 * \brief Internal Functions(parallel)
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "mpi.h"

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"
#include "sion_flags.h"


#ifdef SION_MPI

#include "sion_generic.h"

#include "sion_mpi.h"
#include "sion_mpi_internal_gen.h"

#include "sion_mpi_cb_gen.h"


struct _sion_file_check_par_args_mpi_struct {
  char      *file_mode;
  int        numfiles;
  MPI_Comm   gComm;
  MPI_Comm   lComm; 
};

/*!\brief 
 *
 * @param  x             desc                   
 *
 * @return  y
 *          
 */
#define DFUNCTION "_sion_paropen_generic_buddy"
sion_file_check_par_args_mpi *sion_file_check_par_args_init_mpi( const char     *file_mode,
								 MPI_Comm        gComm, 
								 int             numfiles, 
								 MPI_Comm        lComm 
								 ) {
  sion_file_check_par_args_mpi *cb_args;

  cb_args = (sion_file_check_par_args_mpi *) malloc(sizeof(sion_file_check_par_args_mpi));
  if (cb_args == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate cb_args structure of size %lu (sion_file_check_par_args), aborting ...\n", 
		     (unsigned long) sizeof(sion_file_check_par_args_mpi));
    return(NULL);
  }

  cb_args->file_mode=strdup(file_mode);
  cb_args->gComm=gComm;
  cb_args->lComm=lComm;
  cb_args->numfiles=numfiles;
  
  return(cb_args);
}
#undef DFUNCTION

#define DFUNCTION "sion_file_check_par_args_free_mpi"
int sion_file_check_par_args_free_mpi( sion_file_check_par_args_mpi *cb_args ) {
  int rc=SION_SUCCESS;
  if(cb_args!=NULL) {
    if(cb_args->file_mode!=NULL) {
      free(cb_args->file_mode);
    }
    free(cb_args);
  }
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "sion_file_check_par_cb_mpi"
int sion_file_check_par_cb_mpi( char *fname,  void *v_args ) {
  sion_file_check_par_args_mpi *cb_args= (sion_file_check_par_args_mpi *) v_args;
  int rc=SION_NOT_SUCCESS;
  int gtasks, gRank, sid;
  sion_int64  chunksize=0;
  sion_int32  fsblksize=-1;
  int         globalrank=-1;


  MPI_Comm_size(cb_args->gComm, &gtasks);
  MPI_Comm_rank(cb_args->gComm, &gRank);

  DPRINTFP((1, DFUNCTION, gRank, "enter parallel check of file %s with mode %s\n", fname,cb_args->file_mode));
  
  _sion_errorprint_set_flag(_SION_ERROR_FLAG_SUPPRESS_MSG);
  sid = sion_paropen_mpi(fname, cb_args->file_mode, &cb_args->numfiles, cb_args->gComm, &cb_args->lComm, &chunksize, &fsblksize, &globalrank, NULL, NULL);
  _sion_errorprint_set_flag(_SION_ERROR_FLAG_NONE);
  if(sid<0) {
    rc=SION_NOT_SUCCESS;
  } else {
    rc=SION_SUCCESS;
    sion_parclose_mpi(sid);
  }

  DPRINTFP((1, DFUNCTION, gRank, "leave parallel check of file %s with mode %s rc=%d\n", fname,cb_args->file_mode,rc));
  return(rc);
}
#undef DFUNCTION

#endif
