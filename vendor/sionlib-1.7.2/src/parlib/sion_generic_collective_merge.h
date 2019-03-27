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

#ifndef SION_SION_GENERIC_COLL_MERGE_H
#define SION_SION_GENERIC_COLL_MERGE_H

#include <stddef.h>

/* collective I/O with merge mode */
size_t _sion_coll_fwrite_merge(const void *data, size_t size, size_t nitems, int sid);

#endif
