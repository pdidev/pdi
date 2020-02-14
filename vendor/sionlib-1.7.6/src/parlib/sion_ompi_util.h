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

#ifndef SION_SION_UTIL_OMPI_H
#define SION_SION_UTIL_OMPI_H

#ifdef SION_OMPI

#include "mpi.h"

int sion_get_IO_comm_ompi(MPI_Comm *commSame);

#endif

#endif
