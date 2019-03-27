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
 * OpenMP API
 *
 * \author David Montoya
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>

#include <sys/types.h>
#include <fcntl.h>

#include <unistd.h>

#include "omp.h"

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"
#include "sion_flags.h"

#include "sion_generic.h"

#include "sion_omp.h"
#include "sion_omp_internal_gen.h"
#include "sion_omp_cb_gen.h"
#include "sion_lock.h"


#ifdef SION_OMP


sion_int32 _sion_omp_api_aid = -1;
static omp_lock_t _sion_omp_lock_data;

int _sion_omp_user_lock(void * data) {
  int rc=SION_SUCCESS;
  omp_set_lock(&_sion_omp_lock_data);
  return(rc);
}
int _sion_omp_user_unlock(void * data) {
  int rc=SION_SUCCESS;
  omp_unset_lock(&_sion_omp_lock_data);
  return(rc);
}

/*!
 * @brief Open a sion file using OpenMP.
 *
 * This function opens a sion file using OpenMP. It processes the
 * OpenMP specific parts and then passes its arguments on to
 * sion_generic_paropen().
 *
 * @param[in]       fname        name of file, should be equal on all tasks
 * @param[in]       file_mode    like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in,out]   chunksize    maximum size to be written with single write call
 * @param[in,out]   fsblksize    file system block size (-1 for automatic)
 * @param[in,out]   globalrank   global rank of process
 * @param[in,out]   fileptr      file pointer (NULL for not using an external file pointer)
 * @param[out]      newfname     return value for actual file name if using multi files
 *
 * @retval          sid          sion file handle or -1 if error occured
 */
int sion_paropen_omp(const char	*fname,
		     const char	*file_mode,
		     sion_int64	*chunksize,
		     sion_int32	*fsblksize,
		     int	*globalrank,
		     FILE	**fileptr,
		     char	**newfname)
{
  int        sid = SION_ID_UNDEF;
  int        filenumber, num_threads, thread_num;
  int        numFiles=1,gtasks, gRank, lRank, lSize;
  _omp_api_commdata *gen_gcomm;
  _sion_flags_store* flags_store = NULL;

  thread_num = omp_get_thread_num();
  num_threads = omp_get_num_threads();

  #pragma omp master
  {
    _sion_debug_set_query_thread_num_function(omp_get_thread_num);
    _sion_error_set_query_thread_num_function(omp_get_thread_num);
    omp_init_lock(&_sion_omp_lock_data);
    sion_lock_register_lock_callbacks(_sion_omp_user_lock,_sion_omp_user_unlock,&_sion_omp_lock_data);
  }
  {
  #pragma omp barrier
  }

  DPRINTFP((1, "sion_paropen_omp", thread_num, "enter parallel open of file %s\n", fname));
  
  flags_store = _sion_parse_flags(file_mode);
  if ( ! flags_store ) {
    return(_sion_errorprint_omp(SION_ID_NOT_VALID,_SION_ERROR_RETURN,
				"sion_paropen_omp: could not parse file mode in %s, aborting ...\n", file_mode));
  }
  
  if (flags_store->mask&_SION_FMODE_WRITE) {
    /* file mode WRITE */
    *globalrank = thread_num;
  }
  _sion_flags_destroy_store(&flags_store);

  #pragma omp master
  {
    /* register callbacks for generic interface */
    if(_sion_omp_api_aid<0) _sion_omp_api_aid=_sion_register_callbacks_omp();
  }
  
  /* create generic communicator container */
  gen_gcomm = (_omp_api_commdata *) malloc(sizeof(_omp_api_commdata));
  if (gen_gcomm == NULL) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,
			    "cannot allocate omp internal data structure of size %lu (_omp_api_commdata), aborting ...\n", 
			    (unsigned long) sizeof(_omp_api_commdata)));
  }
  gen_gcomm->commset=1;
  gen_gcomm->thread_num=thread_num;
  gen_gcomm->num_threads=num_threads;

  /* sync to ensure that aid is accessible */
   _sion_omp_barrier_cb(gen_gcomm);

  
  lRank=gRank=thread_num;
  lSize=gtasks=num_threads;
  filenumber=0;
  numFiles=1;
  
  DPRINTFP((1, "sion_paropen_omp", gRank, "enter parallel open of %d files (current name %s) in %s mode\n", numFiles, fname, file_mode));
  sid = sion_generic_paropen(_sion_omp_api_aid, fname, file_mode, chunksize, fsblksize, gen_gcomm, gRank, gtasks, 
			     &filenumber, &numFiles, &lRank, &lSize, fileptr, newfname);
  DPRINTFP((1, "sion_paropen_omp", gRank, "leave parallel open of %d files in %s mode #tasks=%d sid=%d\n", numFiles, file_mode, lSize, sid));


   /* test return code from internal open */
  if ( sid == SION_ID_NOT_VALID ) {
    return(_sion_errorprint_omp(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_omp: invalid return code from internal open %d", sid));
  }

  DPRINTFP((1, "sion_paropen_omp", thread_num, "leave parallel open of file %s sid=%d\n", fname, sid));

  return (sid);
}

/*!\brief closes a SION file previously opened in OpenMP mode
 *
 * @param[in] *sid					 SION file id
 *
 */

int sion_parclose_omp(int sid)
{
  int rc = SION_SUCCESS;
  ONLY_DEBUG(int thread_num;)
  _sion_filedesc *sion_filedesc;

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint_omp(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_parclose_omp: invalid sion_filedesc %d", sid));
  }

  ONLY_DEBUG(thread_num = omp_get_thread_num();)

  if(omp_get_num_threads()!=sion_filedesc->ntasks){
	  return(_sion_errorprint_omp(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
				      "sion_parclose_omp: invalid number of OpenMP threads, %d <> %d", 
				      omp_get_num_threads(), sion_filedesc->ntasks));
  }

  DPRINTFP((1, "sion_parclose_omp", thread_num, "enter parallel close of sid %d\n", sid));
  DPRINTFP((1, "sion_parclose_omp", thread_num, "closing %d file(s)\n", sion_filedesc->nfiles));

  rc = sion_generic_parclose(sid);

  DPRINTFP((1, "sion_parclose_omp", thread_num, "leave parallel close of sid %d rc=%d\n", sid, rc));
  return rc;
}

/* end of ifdef OMP */
#endif
