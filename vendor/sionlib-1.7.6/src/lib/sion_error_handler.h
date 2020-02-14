/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_SION_ERROR_HANDLER_H
#define SION_SION_ERROR_HANDLER_H

#include <stdarg.h>

#include "sion_const.h"

 /* Error types */
#define _SION_ERROR_RETURN         -10001
#define _SION_ERROR_ABORT          -10002
#define _SION_ERROR_WARN           -10003
#define _SION_ERROR_UNKNOWN        -10020


#define _SION_ERROR_FLAG_NONE         0
#define _SION_ERROR_FLAG_SUPPRESS_MSG 1

int _sion_errorprint(int rc, int level, const char *format, ...);
int _sion_errorprint_on_rank(int rc, int level, int rank, const char *format, ...);

int __sion_errorprint_vargs(int rc, int level, int rank, int thread, const char *format, va_list argp);

int _sion_errorprint_set_flag( int flag);

char* __sion_error_level_to_str (unsigned int flag);

int _sion_error_set_query_thread_num_function( int (*get_thread_num)(void) );

#endif
