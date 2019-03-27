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
 *
 * Main SIONlib header
 */

#ifndef SION_SION_H
#define SION_SION_H

/* SION version information --> see sion_const.h */

/* Constants */
#include "sion_const.h"

/* Serial and utility functions */
#include "sion_common.h"

/* generic interface routines (parallel w/o dependencies to MPI/OMP)  */
#include "sion_generic.h"

/* serial interface routines */
#include "sion_serial.h"

#ifdef SION_OMP
/* Parallel Interface: OpenMP  */
#include "sion_omp.h"
#endif

#ifdef SION_MPI
/* Parallel Interface: MPI  */
#include "sion_mpi.h"
#endif

#ifdef SION_OMPI
/* Parallel Interface: OMPI --> MPI+OpenMP  */
#include "sion_ompi.h"
#endif

/* DIST_INCLUDE_LIB_REP */

#endif
