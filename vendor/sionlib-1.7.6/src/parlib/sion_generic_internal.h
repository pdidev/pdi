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

#ifndef SION_SION_GENERIC_INTERNAL_H
#define SION_SION_GENERIC_INTERNAL_H

#include "sion_const.h"
#include "sion_buddy_common.h"
#include "sion_datatypes.h"
#include "sion_filedesc.h"
#include "sion_flags.h"
#include "sion_generic_apidesc.h"

#define _SION_INTERNAL_FLAG_NORMAL        1
#define _SION_INTERNAL_FLAG_BUDDY_NORMAL  2
#define _SION_INTERNAL_FLAG_BUDDY_SEND    4
#define _SION_INTERNAL_FLAG_BUDDY_COLL    8
#define _SION_INTERNAL_FLAG_BUDDY_READ   16
#define _SION_INTERNAL_FLAG_UNKNOWN      32

/* internal interface routines, containing parallel functionality (e.g. meta data management) */
int _sion_paropen_generic_one_file(
				   int sid,
				   char  *fname,
				   _sion_flags_store *flags_store,
				   char  *prefix,
				   int   *numFiles,
				   int   *filenumber, 
				   sion_int64  *chunksize,
				   sion_int32  *fsblksize,
				   int    rank,
				   int    ntasks,
				   int   *globalrank,
				   int    flag,
				   FILE **fileptr,
				   _sion_generic_gendata *sion_gendata,
				   _sion_generic_buddy   *buddy_info);

int _sion_parclose_generic(int sid,
			   int rank,
			   int ntasks,
			   int mapping_size,
			   sion_int32 *mapping,
			   int    flag,
			   _sion_generic_gendata *sion_gendata,
			   _sion_generic_buddy   *buddy_info);

int _sion_parreinit_generic(
			    int sid,
			    sion_int64 chunksize,
			    int rank,
			    int ntasks,
			    _sion_generic_gendata *sion_gendata);

/* internal functions */
int _sion_generic_get_and_distribute_info_from_file(  _sion_generic_gendata *sion_gendata, const char *fname);
int _sion_generic_collect_mapping( _sion_filedesc *sion_filedesc,
				   int            *mapping_size,
				   sion_int32    **mapping );
int _sion_generic_renumber_collmsa(_sion_generic_gendata *comm, _sion_flags_store *flags);
int _sion_generic_is_candidate(_sion_generic_gendata *comm);

#endif
