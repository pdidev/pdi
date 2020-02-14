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
 *
 * Generic API \ref generic_api_page
 */

#ifndef SION_SION_GENERIC_H
#define SION_SION_GENERIC_H

#include <stdio.h>

#include "sion_const.h"
#include "sion_datatypes.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Datatypes used for the collective operations */
int sion_generic_create_api( char *name );
int sion_generic_free_api( int aip );

int sion_generic_register_create_local_commgroup_cb(int aid, int create_lcg_cb(void **, void *, int, int, int, int, int, int));
int sion_generic_register_free_local_commgroup_cb(int aid, int free_lcg_cb(void *));

int sion_generic_register_barrier_cb(int aid, int barrier_cb(void *));
int sion_generic_register_bcastr_cb(int aid, int bcastr_cb(void *,void *, int, int, int));
int sion_generic_register_gatherr_cb(int aid, int gatherr_cb(void *,void *,void *, int, int, int));
int sion_generic_register_gathervr_cb(int aid, int gathervr_cb(void *,void *,void *, int, int *, int, int));
int sion_generic_register_scatterr_cb(int aid, int scatterr_cb(void *,void *,void *, int, int, int));
int sion_generic_register_scattervr_cb(int aid, int scattervr_cb(void *,void *,void *, int, int *, int, int));
int sion_generic_register_get_multi_filename_cb(int aid, char *get_multi_filename_cb(const char *, int));

int sion_generic_register_gather_and_execute_cb(int aid, int gather_execute_cb(const void *, sion_int64*, int, sion_int64, 
									       void *, int, int, int, int,
									       int process_cb(const void *,sion_int64 *, int)));
int sion_generic_register_execute_and_scatter_cb(int aid, int execute_scatter_cb(void *, sion_int64*, int, sion_int64, 
									         void *, int, int, int, int,
									         int process_cb(void *,sion_int64 *, int)));
int sion_generic_register_get_capability_cb(int aid, int get_capability_cb(void *));



int sion_generic_paropen(int         aid,
                         const char* fname,
                         const char* file_mode,
                         sion_int64* chunksize,
                         sion_int32* fsblksize,
                         void*       gcommgroup,
                         int         grank,
                         int         gsize,
                         int*        filenumber,
                         int*        numfiles,
                         const int*  lrank,
                         const int*  lsize,
                         FILE**      fileptr,
                         char**      newfname
                         );

int sion_generic_parclose(int sid);

int sion_generic_paropen_mapped(int         aid,
				char       *fname,
				const char *file_mode,
				int        *numFiles,
				void       *gcommgroup,  
				int         grank,
				int         gsize,
				int         *nlocaltasks,
				int        **globalranks,
				sion_int64 **chunksizes,
				int        **mapping_filenrs, 
				int        **mapping_lranks,
				sion_int32 *fsblksize,
				FILE      **fileptr );

int sion_generic_parclose_mapped(int sid);

int sion_generic_parreinit(int sid, sion_int64 chunksize );

size_t sion_coll_fwrite(const void *data, size_t size, size_t nitems, int sid);
size_t sion_coll_fread( void *data, size_t size, size_t nitems, int sid);

#ifdef __cplusplus
}
#endif

#endif
