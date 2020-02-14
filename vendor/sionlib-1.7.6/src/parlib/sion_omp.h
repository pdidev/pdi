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

#ifndef SION_SION_OMP_H
#define SION_SION_OMP_H

#ifdef SION_OMP
/* SIONlib defines functions using FILE* parameter */
#include <stdio.h>

#include "sion_const.h"
#include "sion_datatypes.h"

/* ************************** */
/* Parallel Interface OpenMP  */
/* ************************** */

#ifdef __cplusplus
extern "C" {
#endif
int sion_paropen_omp(    const char    *fname,
			 const char    *file_mode,
			 sion_int64    *chunksize,
			 sion_int32    *fsblksize,
			 int           *globalrank,
			 FILE         **fileptr,
			 char         **newfname);

int sion_parclose_omp(   int      sid  );
#ifdef __cplusplus
}
#endif

#endif

#endif
