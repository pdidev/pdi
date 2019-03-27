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
 */

#ifndef SION_SION_GENERIC_PAR_MAPPED_H
#define SION_SION_GENERIC_PAR_MAPPED_H

#include <stdio.h>

#include "sion_const.h"
#include "sion_datatypes.h"
#include "sion_generic_apidesc.h"

int _sion_paropen_mapped_generic(
				 int sid,
				 char  *fname,
				 sion_int64 file_mode_flags,
				 char  *prefix,
				 int   *numFiles,
				 int   *nlocaltasks,
				 int   **globalranks, 
				 sion_int64  **chunksizes,
				 int   **mapping_filenrs, 
				 int   **mapping_lranks,
				 sion_int32  *fsblksize,
				 int    rank,
				 int    ntasks,
				 int    flag,
				 FILE **fileptr,
				 _sion_generic_gendata *sion_gendata);

int _sion_parclose_mapped_generic(   int    sid,
				     int    rank,
				     int    ntasks,
				     _sion_generic_gendata *sion_gendata );
#endif
