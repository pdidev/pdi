/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
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
#include "sion_hints_linux.h"



/*!\brief allocates and initalize the hints
 *
 * @param  fd      file descriptor
 * @param  start   start
 * @param  length  length
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_apply_hint_linux_access_range(int fd, long long  start, long long length ) {
  int rc = SION_SUCCESS;


  return (rc);
}


