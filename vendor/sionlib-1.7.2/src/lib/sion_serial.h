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
 * Serial interface \ref api_page
 */

#ifndef SION_SION_SERIAL_H
#define SION_SION_SERIAL_H

/* SIONlib defines functions using FILE* parameter */
#include <stdio.h>

#include "sion_const.h"
#include "sion_datatypes.h"

#ifdef __cplusplus
extern "C" {
#endif
  
  /* open/close */
  int sion_open(    char *fname,
                    const char* file_mode,
                    int  *ntasks,
                    int  *nfiles,
		    sion_int64 **chunksizes,
                    sion_int32  *fsblksize,
                    int  **globalranks,
                    FILE **fileptr);

  int sion_open_rank(    char  *fname,
			 const char  *file_mode,
			 sion_int64  *chunksize,
			 sion_int32  *fsblksize,
			 int   *rank,
			 FILE **fileptr);

  int sion_close(int sid);

#ifdef __cplusplus
}
#endif

#endif

