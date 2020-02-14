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
#include "sion_hints_gpfs.h"

#ifdef SION_HINTS_GPFS
#include <gpfs_fcntl.h>
#endif



/*!\brief applies hint to GPFS accessing a range in the file 
 *
 * @param  fd                       file descriptor
 * @param  start                    start position of range
 * @param  length                   length of range
 * @param  iswrite                  1 for write access, 0 read access
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_apply_hint_gpfs_access_range(int fd, long long  start, long long length, int iswrite ) {
  int rc=SION_SUCCESS;


  return (rc);
}


/*!\brief applies hint to GPFS freeing access to a range in the file 
 *
 * @param  fd                       file descriptor
 * @param  start                    start position of range
 * @param  length                   length of range
 * @param  iswrite                  1 for write access, 0 read access
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_apply_hint_gpfs_free_range(int fd, long long  start, long long length, int iswrite ) {
  int rc=SION_SUCCESS;


  return (rc);
}


