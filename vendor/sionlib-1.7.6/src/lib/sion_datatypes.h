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

#ifndef SION_SION_DATATYPES_H
#define SION_SION_DATATYPES_H

#include "sion_platform.h"

/* SION datatypes */
#if defined(_SION_LINUX)
#define sion_int32 int
#define sion_uint32 unsigned int
#define sion_int64 long long
#define sion_uint64 unsigned long long
#elif defined(_SION_DARWIN)
#define sion_int32 int
#define sion_uint32 unsigned int
#define sion_int64 long long
#define sion_uint64 unsigned long long
#elif defined(_SION_AIX)
#define sion_int32 int
#define sion_uint32 unsigned int
#define sion_int64 long long
#define sion_uint64 unsigned long long
#elif defined(_SION_BGP)
#define sion_int32 int
#define sion_uint32 unsigned int
#define sion_int64 long long
#define sion_uint64 unsigned long long
#elif defined(_SION_BGQ)
#define sion_int32 int
#define sion_uint32 unsigned int
#define sion_int64 long long
#define sion_uint64 unsigned long long
#endif

#endif
