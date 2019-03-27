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

#ifndef SION_SION_GENERIC_COLL_H
#define SION_SION_GENERIC_COLL_H

#include <stddef.h>

#include "sion_const.h"
#include "sion_filedesc.h"

/* collective I/O */
size_t sion_coll_fwrite(const void *data, size_t size, size_t nitems, int sid);
size_t sion_coll_fread(void *data, size_t size, size_t nitems, int sid);

size_t _sion_coll_fwrite_merge(const void *data, size_t size, size_t nitems, int sid);
int  _sion_coll_check_env(_sion_filedesc *sion_filedesc);

/* callback functions */
int _sion_generic_collective_process_write( const void *data, sion_int64 *spec, int sid );
int _sion_generic_collective_process_read( void *data, sion_int64 *spec, int sid );

#endif
