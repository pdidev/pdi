/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_SION_HINTS_GPFS_H
#define SION_SION_HINTS_GPFS_H

#include "sion_const.h"

int  _sion_apply_hint_gpfs_access_range(int fd, long long  start, long long length, int iswrite );
int  _sion_apply_hint_gpfs_free_range(int fd, long long  start, long long length, int iswrite );

#endif
