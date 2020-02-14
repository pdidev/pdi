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
 */

#ifndef SION_SION_INTERNAL_STARTPTR_H
#define SION_SION_INTERNAL_STARTPTR_H

#include "sion_const.h"
#include "sion_filedesc.h"

int _sion_get_size_metadatablock1( _sion_filedesc *sion_filedesc );
int _sion_calculate_startpointers( _sion_filedesc *sion_filedesc );
int _sion_calculate_startpointers_collective( _sion_filedesc *sion_filedesc );
int _sion_calculate_startpointers_collective_msa(_sion_filedesc *fd);
int _sion_calculate_startpointers_collective_merge( _sion_filedesc *sion_filedesc );
int _sion_flush_file( int sid );

#endif
