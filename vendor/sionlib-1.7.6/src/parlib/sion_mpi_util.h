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

#ifndef SION_SION_UTIL_MPI_H
#define SION_SION_UTIL_MPI_H

#ifdef SION_MPI
#include "mpi.h"

int sion_get_IO_comm_mpi(MPI_Comm *commSame);

#endif

#endif
