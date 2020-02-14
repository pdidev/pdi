/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*!
 * \file
 *
 * Hybrid (OpenMP + MPI) API
 *
 * \author David Montoya
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
#include "sion_flags.h"

#ifdef SION_OMPI

#include "sion_generic.h"

#include "sion_ompi.h"
#include "sion_ompi_internal_gen.h"
#include "sion_ompi_cb_gen.h"
#include "sion_lock.h"

#include "omp.h"

int _sion_ompi_api_aid = -1;
static omp_lock_t _sion_ompi_lock_data;

static void * __ompi_thread_sync_struct;

int _sion_ompi_user_lock(void * data) {
  int rc=SION_SUCCESS;
  omp_set_lock(&_sion_ompi_lock_data);
  return(rc);
}
int _sion_ompi_user_unlock(void * data) {
  int rc=SION_SUCCESS;
  omp_unset_lock(&_sion_ompi_lock_data);
  return(rc);
}

/*!
 * @brief Open a sion file using OpenMP/MPI.
 *
 * This function opens a sion file using OpenMP/MPI. It processes the
 * OpenMP/MPI specific parts and then passes its arguments on to
 * sion_generic_paropen().
 *
 * @param[in]       fname        name of file, should be equal on all tasks
 * @param[in]       file_mode    like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in,out]   numFiles     number of multi files to use (-1 for automatic choosing from local communicator)
 * @param[in]       gComm        global communicator
 * @param[in]       lComm        local communicator (= gComm if no adaption to I/O nodes is needed)
 * @param[in,out]   chunksize    maximum size to be written with single write call
 * @param[in,out]   fsblksize    file system block size (-1 for automatic)
 * @param[in,out]   globalrank   global rank of process
 * @param[in,out]   fileptr      file pointer (NULL for not using an external file pointer)
 * @param[out]      newfname     return value for actual file name if using multi files
 *
 * @retval         sid          sion file handle or -1 if error occured
 */
#define DFUNCTION "sion_paropen_ompi"
int sion_paropen_ompi(const char*     fname,
                      const char*     file_mode,
                      int*            numFiles,
                      MPI_Comm        gComm,
                      const MPI_Comm* lComm,
                      sion_int64*     chunksize,
                      sion_int32*     fsblksize,
                      int*            globalrank,
                      FILE**          fileptr,
                      char**          newfname)
{

  /* gRank and lRank refer to the MPI process rank in the global and local communicator respectively */
  int       rc, sid = -1;
  int       filenumber, gRank, lRank, lSize, gSize;

  _sion_flags_store* flags_store = NULL;

  _ompi_api_commdata *gen_gcomm;
  _ompi_api_commdata *gen_lcomm=NULL;
	  
  int                 num_threads, thread_num;
  __ompi_thread_sync  *thread_sync;


	  
  thread_num = omp_get_thread_num();

  #pragma omp master
  {

    _sion_debug_set_query_thread_num_function(omp_get_thread_num);
    _sion_error_set_query_thread_num_function(omp_get_thread_num);
    omp_init_lock(&_sion_ompi_lock_data);
    sion_lock_register_lock_callbacks(_sion_ompi_user_lock,_sion_ompi_user_unlock,&_sion_ompi_lock_data);

    MPI_Comm_size(gComm, &gSize);
    MPI_Comm_rank(gComm, &gRank);
    num_threads = omp_get_num_threads();
    
    thread_sync = malloc(sizeof(__ompi_thread_sync));
    if(thread_sync==NULL) (_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_ABORT,"sion_paropen_ompi: cannot allocate struct of size %lu (__ompi_thread_sync), aborting...", 
						 sizeof(__ompi_thread_sync)));

    thread_sync->grank_master_mpi  = gRank;
    thread_sync->gsize_mpi         = gSize;
    thread_sync->grank_master_ompi = _sion_map_rank_mpi_to_ompi(gRank,num_threads,thread_num);
    thread_sync->gsize_ompi        = _sion_get_size_ompi(gSize,num_threads);
    thread_sync->num_threads       = num_threads;
    thread_sync->numFiles           = *numFiles;
    __ompi_thread_sync_struct = thread_sync;
  }
  /* sync to ensure that info in thread_sync is accessible */
  {
#pragma omp barrier
  }
  
  
  /* this is actually not necessary, but it makes for cleaner code by preventing us from doing lots of typecasts */
  thread_sync = (__ompi_thread_sync *) __ompi_thread_sync_struct;
  
  DPRINTFP((1, "sion_paropen_ompi", thread_sync->grank_master_ompi+thread_num, "thread %d enters parallel open of file %s\n", thread_num, fname));
 

  /* check parameters */
  if (lComm == NULL) {
    return(_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_ABORT,"sion_paropen_ompi: No lComm variable given"));
  }
  if (numFiles == NULL) {
    return(_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_ABORT,"sion_paropen_ompi: No numFiles variable given"));
  }
  flags_store = _sion_parse_flags(file_mode);
  if ( ! flags_store ) {
    return(_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: could not parse file mode in %s, aborting ...\n", file_mode));
  }
  if (_sion_flags_get(flags_store, "collmsa")) {
    _sion_flags_destroy_store(&flags_store);
    return _sion_errorprint(SION_ID_NOT_VALID, _SION_ERROR_ABORT, "sion_paropen_omp: MSA aware collective operations not supported with OpenMP API, aborting ...\n");
  }

  /* create generic API */
  #pragma omp master
  {
    /* register callbacks for generic interface */
    if(_sion_ompi_api_aid<0) _sion_ompi_api_aid=_sion_register_callbacks_ompi();
  }

  /* create global generic communicator container on all threads */
  gen_gcomm = (_ompi_api_commdata *) malloc(sizeof(_ompi_api_commdata));
  if (gen_gcomm != NULL) {
    gen_gcomm->commset=0;
    gen_gcomm->local=0;
    gen_gcomm->rank=thread_sync->grank_master_ompi+thread_num;
    gen_gcomm->size=thread_sync->gsize_ompi;
    gen_gcomm->num_threads=thread_sync->num_threads;
    gen_gcomm->thread_num=thread_num;
    gen_gcomm->lcommgroup=NULL;
  } else {
    _sion_flags_destroy_store(&flags_store);
    return(_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,
			  "cannot allocate ompi internal data structure of size %lu (_omp_api_commdata), aborting ...\n", 
				(unsigned long) sizeof(_ompi_api_commdata)));
  } 

  /* store MPI communicator in global generic communicator container on master thread */
#pragma omp master
  {
    gen_gcomm->comm=gComm;
  }



  /* sync to ensure that aid is accessible */
  {
#pragma omp barrier
  }

  if (flags_store->mask&_SION_FMODE_WRITE) {
    /* file mode WRITE */

    /* create generic local communicator container on each thread */
    if (*numFiles <= 0) {
      gen_lcomm = (_ompi_api_commdata *) malloc(sizeof(_ompi_api_commdata));
      if (gen_lcomm != NULL) {
	gen_lcomm->commset=1;
	gen_lcomm->commcreated=0;
    	gen_lcomm->local=1;
	gen_lcomm->num_threads=gen_gcomm->num_threads;
	gen_lcomm->thread_num=thread_num;
	gen_gcomm->lcommgroup=gen_lcomm; /* store pointer in global comm group */
      } else {
        _sion_flags_destroy_store(&flags_store);
	return(_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate ompi internal data structure of size %lu (_ompi_api_commdata), aborting ...\n", 
				     (unsigned long) sizeof(_ompi_api_commdata)));
      }
    }

#pragma omp master
    {

      if (*numFiles <= 0) {
	/* lComm contains local communicator */
	
	rc = _sion_get_info_from_splitted_comm_ompi(gComm, *lComm, numFiles, &filenumber, &lRank, &lSize);
	if(rc != SION_SUCCESS) _sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_ompi: error in _sion_get_info_from_splitted_comm_ompi");
	DPRINTFP((1, DFUNCTION, gRank, "%d local communicators found\n", *numFiles));

	gen_lcomm->comm=*lComm;

      } else {
	/* number of files is given */
	rc = _sion_gen_info_from_gcomm_ompi(*numFiles, gComm, &filenumber, &lRank, &lSize);
	if(rc != SION_SUCCESS) _sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_ompi: error in _sion_gen_info_from_gcomm_ompi");
	DPRINTFP((1, "sion_paropen_ompi", gRank, "Global communicator divided in %d local communicators\n", *numFiles));      
      }
      
      thread_sync->filenumber        = filenumber;
      thread_sync->numFiles          = *numFiles;
      thread_sync->lrank_master_mpi  = lRank;
      thread_sync->lsize_mpi         = lSize;
      thread_sync->lrank_master_ompi = _sion_map_rank_mpi_to_ompi(lRank,num_threads,thread_num);
      thread_sync->lsize_ompi        = _sion_get_size_ompi(lSize,num_threads);
      
    } /* OMP MASTER END */
    
    {
#pragma omp barrier
    }

      
    /* set up parameters of call to generic open (OMPI values) */
    gRank       = thread_sync->grank_master_ompi+thread_num;
    gSize       = thread_sync->gsize_ompi;
    lRank       = thread_sync->lrank_master_ompi+thread_num;
    lSize       = thread_sync->lsize_ompi;
    filenumber  = thread_sync->filenumber;
    *numFiles   = thread_sync->numFiles;

    if (gen_lcomm != NULL) {
      gen_lcomm->rank=thread_sync->lrank_master_ompi+thread_num;
      gen_lcomm->size=thread_sync->lsize_ompi;
    }

  } else if (flags_store->mask&_SION_FMODE_READ) {

      /* file mode READ */
    /* set up parameters of call to generic open (OMPI values) */
    gRank       = thread_sync->grank_master_ompi+thread_num;
    gSize       = thread_sync->gsize_ompi;
    lRank       = -1; 		/* which determined after opening file by sion_generic_paropen */
    lSize       = -1;		/* " */
    filenumber  = -1;		/* " */
    *numFiles   = -1;		/* " */

  }  else {
    _sion_flags_destroy_store(&flags_store);
    return(_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_ompi: unknown file mode"));
  }
  _sion_flags_destroy_store(&flags_store);


  DPRINTFP((1, DFUNCTION, gRank, "enter parallel open of %d files (current name %s) in %s mode\n", *numFiles, fname, file_mode));
  DPRINTFP((2, DFUNCTION, gRank, "enter parallel parameters: grank=%d gsize=%d fnum=%d numfiles=%d lrank=%d lsize=%d chunksize=%d\n", 
	    gRank, gSize,filenumber, *numFiles, lRank, lSize, (int) *chunksize));
  sid = sion_generic_paropen(_sion_ompi_api_aid, fname, file_mode, chunksize, fsblksize, gen_gcomm, 
			     gRank, gSize, &filenumber, numFiles, &lRank, &lSize, 
			     fileptr, newfname);
  DPRINTFP((1, DFUNCTION, gRank, "leave parallel open of %d files in %s mode #tasks=%d sid=%d\n", *numFiles, file_mode, lSize, sid));

  /* test return code from internal open */
  if ( sid == SION_ID_NOT_VALID ) {
    return(_sion_errorprint_ompi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: invalid return code from internal open %d", rc));
  }

  /* return parameter */
  *globalrank=gRank;

  DPRINTFP((1, "sion_paropen_ompi", gRank, "leave parallel open of file %s sid=%d globalrank=%d\n", fname, sid,*globalrank));

  return (sid);
}
#undef DFUNCTION


/*!\brief closes a SION file previously opened in OpenMP/MPI mode
 *
 * @param[in] *sid					 SION file id
 *
 */

int sion_parclose_ompi(int sid)
{
  int rc=0;

  DPRINTFP((1, "sion_parclose_ompi", _SION_DEFAULT_RANK, "enter parallel close of sid %d\n", sid));

  rc = sion_generic_parclose(sid);

  DPRINTFP((1, "sion_parclose_ompi", _SION_DEFAULT_RANK, "leave parallel close of sid %d rc=%d\n", sid, rc));

  return (rc);
}

/* end of ifdef OMPI */
#endif
