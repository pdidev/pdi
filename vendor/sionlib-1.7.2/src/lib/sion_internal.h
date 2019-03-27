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

#ifndef SION_SION_INTERNAL_H
#define SION_SION_INTERNAL_H

#include <stdio.h>

#include "sion_const.h"
#include "sion_common.h"
#include "sion_datatypes.h"
#include "sion_filedesc.h"

#define _SION_READ_MASTER_ONLY_OF_MULTI_FILES         0
#define _SION_READ_ALL_OF_MULTI_FILES                 1

#define _SION_SAFE_FREE(ptr, null) {if (ptr) {free(ptr); ptr = null;}}

/* internal functions for serial open/close, will also be used by parallel APIs */
int _sion_open(    const char *fname,
		   const char* file_mode,
		   int  *ntasks,
		   int  *nfiles,
		   sion_int64 **chunksizes,
		   sion_int32  *fsblksize,
		   int  **globalranks,
		   FILE **fileptr);

int _sion_open_rank(    const char  *fname,
			const char  *file_mode,
			sion_int64  *chunksize,
			sion_int32  *fsblksize,
			int   *rank,
			FILE **fileptr);

int _sion_open_write(const char *fname, sion_int64 file_mode_flags, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr);
int _sion_open_read(const char *fname, sion_int64 file_mode_flags, int read_all, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr);
int _sion_open_read_single(const char *fname, sion_int64 file_mode_flags, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr, _sion_filedesc *sion_filedesc);
int _sion_open_read_master(const char *fname, sion_int64 file_mode_flags, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr, _sion_filedesc *sion_filedesc);

int _sion_close(_sion_filedesc *sion_filedesc);

int _sion_create_new_block( _sion_filedesc *sion_filedesc );
int _sion_flush_block( _sion_filedesc *sion_filedesc );
int _sion_update_fileposition( _sion_filedesc *sion_filedesc );
int _sion_check_on_collective_mode( _sion_filedesc *sion_filedesc);

char * _sion_get_multi_filename(const char *fname, int filenumber);

sion_int32 _sion_get_endianness_with_flags(sion_int64 flags);

 /* File mode flags */
#define _SION_FMODE_WRITE          1024
#define _SION_FMODE_READ           2048
#define _SION_FMODE_ANSI           1
#define _SION_FMODE_POSIX          2
#define _SION_FMODE_BUFFERED       4
#define _SION_FMODE_COMPRESS       8
#define _SION_FMODE_COLLECTIVE     16
#define _SION_FMODE_COLLECTIVE_MERGE  32
#define _SION_FMODE_KEYVAL         64
#define _SION_FMODE_KEYVAL_INLINE   4096
#define _SION_FMODE_KEYVAL_META     8192
#define _SION_FMODE_KEYVAL_HASH    16384
#define _SION_FMODE_KEYVAL_NONE    32768
#define _SION_FMODE_KEYVAL_UNKNOWN 65536
#define _SION_FMODE_ENDIANNESS_SET 131072
#define _SION_FMODE_ENDIANNESS_BIG 262144
#define _SION_FMODE_BUDDY          524288

char *_sion_getenv(const char *name);

sion_io_stat_t* _sion_alloc_io_info(int nfiles);
int _sion_free_io_info(sion_io_stat_t *p);


#endif
