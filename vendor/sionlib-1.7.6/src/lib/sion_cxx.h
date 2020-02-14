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

#ifndef SION_SION_CXX_H
#define SION_SION_CXX_H

#include "sion.h"

#ifdef __cplusplus

/* Serial and Utility CXX Functions */
//#include "sion_cxx_base.hpp"
#include "sion_cxx_common.hpp"
//#include "sion_cxx_serial.hpp"

#ifdef SION_OMP
/* Parallel CXX Interface: OpenMP  */
#include "sion_cxx_omp.hpp"
#endif

#if defined(MPI_VERSION) || defined(SION_MPI)
/* Parallel CXX Interface: MPI  */
#include "sion_cxx_mpi.hpp"
#endif

#if defined(MPI_VERSION) || defined(SION_OMPI)
/* Parallel CXX Interface: OMPI --> MPI+OpenMP  */
#include "sion_cxx_ompi.hpp"
#endif

#endif

#endif
