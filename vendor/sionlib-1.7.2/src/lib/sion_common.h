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
 * Common SIONlib functions
 */

#ifndef SION_SION_COMMON_H
#define SION_SION_COMMON_H

/* SIONlib PUBLIC Interface: common */

#include <stdio.h>
#include <stdint.h>

#include "sion_const.h"
#include "sion_datatypes.h"

/* default mode */
#define SION_SERIAL_MASTER

#define SION_MAX_FSBLOCKSIZE     32*1024*1024
#define SION_DEFAULT_FSBLOCKSIZE  2*1024*1024

#define SION_FILENAME_LENGTH 1024

#define MAXCHUNKS 100     /*!< Initial maximum number of chunks in a sion file */

#define WFLARGEMETABLOCK_none /* for BENCHMARKING only */

#ifdef __cplusplus
extern "C" {
#endif


  int _sion_close_sid(int sid);
  
  /* I/O */
  size_t sion_fwrite(const void *data, size_t size, size_t nitems, int sid);
  size_t sion_fread( void *data, size_t size, size_t nitems, int sid);

  /* I/O: key-value */
  size_t sion_fwrite_key(const void *data, uint64_t key, size_t size, size_t nitems, int sid);
  size_t sion_fread_key(void *data,  uint64_t key, size_t size, size_t nitems, int sid);

  /* iterator for key-value pairs */
  int sion_fread_key_iterator_reset(int sid);
  int sion_fread_key_iterator_next(int sid, uint64_t *key, size_t *size);
  
  /* seek for key-value */
  int sion_seek_key(int sid, uint64_t key, int entrynum, sion_int64 posinentry );

  /* scans input file for all information about all keys of this rank */
  int sion_key_full_scan(int sid);

  /* iterator for list of keys  */
  int sion_key_list_iterator_reset(int sid);
  int sion_key_list_iterator_next(int sid, uint64_t *keyptr);

  typedef struct _sion_key_stat_struct sion_key_stat_t; 
  struct _sion_key_stat_struct { 
    uint64_t key; 
    sion_int64 num_blocks;
    sion_int64 total_size;
  }; 

  /* fills a stat-data structure containing information about the key */
  int sion_key_get_stat(int sid, uint64_t key, sion_key_stat_t *stat);

  /* helper functions */
  int sion_feof(int sid);
  int sion_ensure_free_space(int sid, sion_int64 bytes);

  int sion_flush(int sid);

  /* get size of internal data structure */
  int sion_get_sizeof(int sid, int *numbytes, int *numfds);

  /* duplicate fd (only parallel read-mode) */
  int sion_dup(int sid, int mode, int rank, uint64_t key);
  int sion_dedup(int sid);

  /* special helper functions, for used with szlib */
  int sion_set_fp_closed(int sid);
  int sion_set_second_fp(int sid, FILE *fp);
  int sion_unset_second_fp(int sid);
  FILE * sion_get_fp(int sid);
  int    sion_get_fd(int sid);
  int sion_optimize_fp_buffer(int sid);


  /* get information (with sion datatypes) */
  int sion_get_file_endianness(int sid);
  int sion_endianness_swap_needed(int sid);
  sion_int64 sion_get_bytes_written(int sid);
  sion_int64 sion_get_bytes_read(int sid);
  sion_int64 sion_bytes_avail_in_block( int sid );
  sion_int64 sion_bytes_avail_in_chunk( int sid );
  sion_int64 sion_get_position( int sid );

  int sion_get_mapping( int          sid,
			int         *mapping_size,
			sion_int32 **mapping,
			int         *numfiles );
  
  int sion_get_locations(    int    sid,
			     int   *ntasks,
			     int   *maxblocks,
			     sion_int64 *globalskip,
			     sion_int64 *start_of_varheader,
			     sion_int64 **sion_chunksizes,
			     sion_int64 **sion_globalranks,
			     sion_int64 **sion_blockcount,
			     sion_int64 **sion_blocksizes);
  /* parallel mode */
  int sion_get_current_location( int sid, 
				 int *currentblock, 
				 sion_int64 *currentpos,  
				 int *maxchunks, 
				 sion_int64 **chunksizes );
  /* serial mode */
  int sion_get_current_locations( int sid,
				  int *ntasks,
				  sion_int64 **sion_currentpos,
				  sion_int64 **sion_currentblocknr );
  int sion_get_number_of_files(   int sid );
  int sion_get_filenumber(  int sid );

  /* 1 is opened with sion_open or sion_open_rank */
  int sion_is_serial_opened(  int sid );

  /* 1 if using GPFS or Linux hints */
  int sion_using_hints(int sid);

  /*   return current endianness (1-> big endian, 0 ->little endian) */
  int sion_get_endianness(void);

  /*   return if SIONlib supports PTHREADS (Thread-safe) 1 -> yes, 0 -> no  */
  int sion_is_thread_safe(void);

  /* return keyvalue mode (constants described in sion_const.h) */
  int sion_get_keyval_mode(int sid);
  char* sion_keyval_type_to_str(int type); /* converts keyval mode to character string */


  /*   return version numbers */
  int sion_get_version(int *main_version, int *sub_version, int *patch_level, int *fileformat_version);

  typedef struct _sion_io_stat_struct sion_io_stat_t; 
  struct _sion_io_stat_struct { 
    int          nfiles;
    const char **names;
    size_t      *sizes;
    int         *roles;
  }; 

  /* return info about accessed (local) files on this task  */
  sion_io_stat_t* sion_get_io_info(int sid);

  /* return info about accessed (local) files on this task */
  sion_io_stat_t* sion_get_io_info_by_spec(int sid, int roles, int flag);
  int sion_free_io_info(sion_io_stat_t *info);


  /* Seeking */
  int sion_seek(  int  sid,
		  int  rank,
		  int  currentblocknr,
		  sion_int64 posinblk );

  int sion_seek_fp(  int  sid,
		     int  rank,
		     int  currentblocknr,
		     sion_int64 posinblk,
		     FILE **fileptr
		     );

  /* register user defined lock and unlock routines, which are needed to access shared data structures internally  */
  int sion_lock_register_lock_callbacks(int lock(void *), int unlock(void *), void *lock_data);
  int sion_lock_user_callbacks_defined(void);


  /* Utilities */
  void sion_swap(void *target, void *source, int size, int n, int do_swap);

  /* for backward compatibility */
#define _sion_stat_file _sion_file_stat_file
  int        _sion_file_stat_file(const char *fname);

#ifdef __cplusplus
}
#endif

  /* Debugging  */
#ifndef DPRINTF
#define DPRINTF(A)
#define DPRINTFP(A)
#define DPRINTTS(A,B)
#define DPRINTFTS2(A,B)
#endif
  

#endif
