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
#include "sion_internal.h"
#include "sion_hints.h"
#include "sion_hints_gpfs.h"


#if defined(_SION_LINUX)
#elif defined(_SION_AIX)
#elif defined(_SION_BGP)
#endif


/* 

   
 */

/*!\brief checks if environment variables are set to use hints
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return 1 if hints should use
 */
int  _sion_hints_check_env(_sion_filedesc *sion_filedesc) {
  const char *t;
  int         rc = SION_SUCCESS;

  t = _sion_getenv("SION_HINTS");
  if(t) {
    if(atoi(t) == 1) {
      sion_filedesc->usehints=1;
    } else {
      sion_filedesc->usehints=0;
    }
  }

#ifdef SION_HINTS_LINUX
    sion_filedesc->hinttype=SION_HINTS_TYPE_LINUX;
#endif
#ifdef SION_HINTS_GPFS
    sion_filedesc->hinttype=SION_HINTS_TYPE_GPFS;
#endif

  t = _sion_getenv("SION_HINT_TYPE_GPFS");
  if(t) {
    if(atoi(t) == 1) {
      sion_filedesc->hinttype=SION_HINTS_TYPE_GPFS;
    }
  }
  t = _sion_getenv("SION_HINT_TYPE_LINUX");
  if(t) {
    if(atoi(t) == 1) {
      sion_filedesc->hinttype=SION_HINTS_TYPE_LINUX;
    }
  }


  DPRINTFP((2, "_sion_hints_check_env", -1, "usehints=%d hinttype=%d\n", sion_filedesc->usehints, sion_filedesc->hinttype));

  return (rc);
}


/*!\brief allocates and initalize the hints
 *
 * @param  sion_filedesc  sion file description struct (_sion_filedesc)
 * @param  access_type    type of access
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_apply_hints(_sion_filedesc *sion_filedesc, int access_type) {
  int rc=SION_SUCCESS;

  DPRINTFP((2, "_sion_apply_hints", -1, "start usehints=%d hinttype=%d access_type=%d\n",
	    sion_filedesc->usehints,sion_filedesc->hinttype,access_type));

  if(!sion_filedesc->usehints) {
    return(rc);
  }

#ifdef SION_HINTS_LINUX
  if(sion_filedesc->hinttype==SION_HINTS_TYPE_LINUX) {
    DPRINTFP((2, "_sion_apply_hints", -1, "apply Linux hints\n"));
    int fd=_sion_file_get_fd(sion_filedesc->fileptr);
    int iswrite=sion_filedesc->mode == SION_FILEMODE_WRITE;
    if(access_type==SION_HINTS_ACCESS_TYPE_METADATABLOCK1) {
      long long startpos = 0;
      long long length   = _sion_get_size_metadatablock1( sion_filedesc );
     
      DPRINTFP((2, "_sion_apply_hints", -1, "apply linux hints for metablock1 (%d,%lld,%lld)\n",fd, startpos, length));
      rc=_sion_apply_hint_linux_access_range(fd, startpos, length, iswrite);
    }
  }
#endif

#ifdef SION_HINTS_GPFS
  if(sion_filedesc->hinttype==SION_HINTS_TYPE_GPFS) {
    int fd=_sion_file_get_fd(sion_filedesc->fileptr);
    int iswrite=sion_filedesc->mode == SION_FILEMODE_WRITE;

    if(access_type==SION_HINTS_ACCESS_TYPE_METADATABLOCK1) {
      long long startpos = 0;
      long long length   = _sion_get_size_metadatablock1( sion_filedesc );
      DPRINTFP((2, "_sion_apply_hints", -1, "apply GPFS hints for metablock1 (%d,%lld,%lld)\n",fd, startpos, length));
      rc=_sion_apply_hint_gpfs_access_range(fd, startpos, length, iswrite);
    }

    if(access_type==SION_HINTS_ACCESS_TYPE_CHUNK) {
      long long startpos = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip;
      long long length   = sion_filedesc->chunksize;
      DPRINTFP((2, "_sion_apply_hints", -1, "apply  GPFS hints (access) for chunk (%d,%lld,%lld)\n",fd, startpos, length));
      rc=_sion_apply_hint_gpfs_access_range(fd, startpos, length, iswrite);
    }

    if(access_type==SION_HINTS_FREE_TYPE_CHUNK) {
      long long startpos = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip;
      long long length   = sion_filedesc->chunksize;
      DPRINTFP((2, "_sion_apply_hints", -1, "apply GPFS hints (free)    for metablock1 (%d,%lld,%lld)\n",fd, startpos, length));
      rc=_sion_apply_hint_gpfs_free_range(fd, startpos, length, iswrite);
    }

  }
#endif
  

  return (rc);
}


