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

#ifndef SION_SION_DATATYPES_MPI_H
#define SION_SION_DATATYPES_MPI_H

#if defined(MPI_VERSION) || defined(SION_MPI) || defined(SION_OMPI)

#if defined(_SION_LINUX)
#define SION_MPI_INT32 MPI_INT
#define SION_MPI_INT64 MPI_LONG_LONG
#elif defined(_SION_DARWIN)
#define SION_MPI_INT32 MPI_INT
#define SION_MPI_INT64 MPI_LONG_LONG
#elif defined(_SION_AIX)
#define SION_MPI_INT32 MPI_INT
#define SION_MPI_INT64 MPI_LONG_LONG
#elif defined(_SION_BGP)
#define SION_MPI_INT32 MPI_INTEGER4
#define SION_MPI_INT64 MPI_INTEGER8
#elif defined(_SION_BGQ)
#define SION_MPI_INT32 MPI_INTEGER4
#define SION_MPI_INT64 MPI_INTEGER8
#endif

#endif

#endif
