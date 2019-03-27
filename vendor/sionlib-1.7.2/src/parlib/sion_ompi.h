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
 * \ref api_page
 */

#ifndef SION_SION_OMPI_H
#define SION_SION_OMPI_H

#ifdef SION_OMPI

#include <stdio.h>

#include "mpi.h"

#include "sion_const.h"
#include "sion_datatypes.h"
#include "sion_datatypes_mpi.h"

#ifdef __cplusplus
extern "C" {
#endif

int sion_paropen_ompi(    const char     *fname,
			  const char     *file_mode,
			  int            *numFiles,
			  MPI_Comm        gComm,
			  const MPI_Comm *lComm,
			   sion_int64     *chunksize,
			  sion_int32     *fsblksize,
			  int            *globalrank,
			  FILE          **fileptr,
			  char          **newfname);

int sion_parclose_ompi(   int      sid  );

#ifdef __cplusplus
}
#endif

#endif

#endif
