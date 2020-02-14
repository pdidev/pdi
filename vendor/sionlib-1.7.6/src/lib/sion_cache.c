/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_cache.h"


#if defined(_SION_LINUX)
#include <sys/mman.h>
#elif defined(_SION_AIX)
#elif defined(_SION_BGP)
#endif


/*!
 * \file
 *
 * This functions control and manage a memory based cache for sion
 * files. Depending on a set of environment variables the cache will work as
 * write-through or as write-back cache. On Unix systems a POSIX shared memory
 * segment will be used, on Blue Gene/P it is persistent memory.
 *
 * A cache of n MB will store the last n MB of data per task in the cache.  If
 * the cache memory is filled, the full cache will be written to disk and the
 * cache will be exhausted. 
 *
 */

/*!\brief Check if environment variables are set to use cache.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if env could be checked
 */
int  _sion_cache_check_env(_sion_filedesc *sion_filedesc) {
  const char *t;
  int rc = SION_SUCCESS;

  t = _sion_getenv("SION_CACHESIZE");
  if(t) {
    sion_filedesc->usecache=1;
    sion_filedesc->cachesize=atoi(t);
    sion_filedesc->flag1 |= _SION_FLAG1_USECACHE;
  }
  DPRINTFP((2, "_sion_cache_check_env", -1, "usecache=%d cachesize=%d flag1=%d\n", sion_filedesc->usecache, sion_filedesc->cachesize, sion_filedesc->flag1));

  return (rc);
}


/*!\brief Allocate and initalize the cache.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_cache_init(_sion_filedesc *sion_filedesc) {
  int rc = SION_SUCCESS;

  /* allocation */
  if(sion_filedesc->usecache) {
    if(sion_filedesc->mode == SION_FILEMODE_WRITE) { 
#if defined(_SION_LINUX)
      rc=_sion_cache_create_linux(sion_filedesc);
#elif defined(_SION_AIX)
#elif defined(_SION_BGP)
#endif
    } else if(sion_filedesc->mode == SION_FILEMODE_READ) {
#if defined(_SION_LINUX)
      rc=_sion_cache_load_linux(sion_filedesc);
#elif defined(_SION_AIX)
#elif defined(_SION_BGP)
#endif
    } else {
      _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_cache_init: unknown SION mode\n");
    }
  }

  return (rc);
}


/*!\brief Deallocate the cache.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_cache_destroy(_sion_filedesc *sion_filedesc) {
  int rc = SION_SUCCESS;

  /* deallocation */
  if(sion_filedesc->usecache) {
#if defined(_SION_LINUX)
    rc=_sion_cache_destroy_linux(sion_filedesc);
#elif defined(_SION_AIX)
#elif defined(_SION_BGP)
#endif
  }

  return (rc);
}

/*!\brief Allocate cache for linux.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_cache_create_linux(_sion_filedesc *sion_filedesc) {
  int rc = SION_SUCCESS;

  return(rc);
}

/*!\brief Load an already allocated cache for linux.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_cache_load_linux(_sion_filedesc *sion_filedesc) {
  int rc = SION_SUCCESS;

  return(rc);
}


/*!\brief Deallocate cache for linux.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return 1 if ok
 */
int  _sion_cache_destroy_linux(_sion_filedesc *sion_filedesc) {
  int rc = SION_SUCCESS;


  return(rc);
}
