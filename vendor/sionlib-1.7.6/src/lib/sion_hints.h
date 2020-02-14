/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_SION_HINTS_H
#define SION_SION_HINTS_H

#include "sion_const.h"
#include "sion_filedesc.h"

#define SION_HINTS_ACCESS_TYPE_METADATABLOCK1 101
#define SION_HINTS_ACCESS_TYPE_METADATABLOCK2 102
#define SION_HINTS_ACCESS_TYPE_CHUNK 103
#define SION_HINTS_FREE_TYPE_CHUNK 104

int  _sion_hints_check_env(_sion_filedesc *sion_filedesc);

int  _sion_apply_hints(_sion_filedesc *sion_filedesc, int access_type);

#endif
