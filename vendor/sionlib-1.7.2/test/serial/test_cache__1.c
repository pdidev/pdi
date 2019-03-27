/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>           /* For O_* constants */

#include "sion.h"
#include "sion_debug.h"
#include "sion_internal.h"
#include "sion_metadata.h"
#include "sion_filedesc.h"
#include "sion_tools.h"
#include "sion_fd.h"
#include "sion_file.h"
#include "sion_cache.h"
#include "sion_printts.h"

int main(int argc, char **argv)
{
  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  /* -------------------------- */
  /* TEST A: test with wrong parameters */
  /* -------------------------- */
  {
    /* sion_int32  fsblksize = 10; */
    /* sion_int64 *chunksizes = NULL; */
    /* int        *globalranks = NULL; */
    /* int         ntasks = 4; */
    /* int         nfiles = 1; */
    /* int         outsid; */
    /* FILE       *outfp; */
    int rc;
    _sion_filedesc *sion_filedesc;

    
    sion_filedesc = _sion_alloc_filedesc();
    if (sion_filedesc == NULL) {
      return(_sion_errorprint(-1,_SION_ERROR_RETURN,"sion_open: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n", (unsigned long) sizeof(sion_filedesc)));
    }
    _sion_init_filedesc(sion_filedesc);

    /* test check function of sion_cache */
    rc=_sion_cache_check_env(sion_filedesc);
    printf("_sion_cache_check_env: returns rc=%d\n",rc);
    printf("_sion_cache_check_env: usecache=  %d\n",sion_filedesc->usecache);
    printf("_sion_cache_check_env: cachesize= %d\n",sion_filedesc->cachesize);

    /* test creation of sion_cache */
    sion_filedesc->mode=SION_FILEMODE_WRITE;
    rc=_sion_cache_init(sion_filedesc);
    printf("_sion_cache_check_env: returns rc=%d\n",rc);
    printf("_sion_cache_check_env: cacheid=   %d\n",sion_filedesc->cacheid);

    /* test re-usage of sion_cache */
    sion_filedesc->mode=SION_FILEMODE_READ;
    rc=_sion_cache_init(sion_filedesc);
    printf("_sion_cache_check_env: returns rc=%d\n",rc);
    printf("_sion_cache_check_env: cacheid=   %d\n",sion_filedesc->cacheid);

  }


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  
  return(0);
  
}
