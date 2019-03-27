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

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"

#include "sion_omp_internal_gen.h"

#ifdef SION_OMP

#include "omp.h"

int _sion_errorprint_omp(int rc, int level, const char *format, ...)
{
  int     rank=-1, thread;
  va_list ap;

  thread = omp_get_thread_num();

  va_start(ap, format);
  rc=__sion_errorprint_vargs(rc, level, rank, thread, format, ap);
  va_end(ap);

  return (rc);
}

/* end of ifdef OMP */
#endif
