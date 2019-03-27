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

#ifndef SION_SION_KEYVALUE_INLINE_H
#define SION_SION_KEYVALUE_INLINE_H

#include <stdint.h>

#include "sion_const.h"
#include "sion_common.h"
#include "sion_datatypes.h"
#include "sion_filedesc.h"

typedef struct _sion_key_info_table_struct _sion_key_info; 

struct _sion_key_info_table_struct { 
  sion_int64 key; 
  size_t offset; 
  size_t bytes_left; 
  sion_int32 new; 
}; 

size_t _sion_store_and_write_key_and_len_inline(_sion_filedesc *sion_filedesc, uint64_t key, size_t len);
size_t _sion_write_value_inline(_sion_filedesc *sion_filedesc, const void *data, uint64_t key, size_t len);

int _sion_key_full_scan_inline(_sion_filedesc *sion_filedesc);

int _sion_iterator_reset_inline(_sion_filedesc *sion_filedesc);
int _sion_iterator_next_inline(_sion_filedesc *sion_filedesc, uint64_t *keyptr, size_t *sizeptr);

int _sion_find_and_read_key_and_len_inline(_sion_filedesc *sion_filedesc, uint64_t key, size_t len, size_t *datalen);
size_t _sion_read_value_inline(_sion_filedesc *sion_filedesc, void *data, uint64_t key, size_t len);

int _sion_keyval_dup_dataptr_inline(_sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc);

int _sion_seek_key_inline(_sion_filedesc *sion_filedesc, uint64_t key, int blocknum, sion_int64 posinblock);

/* iterator for list of keys  */
int _sion_key_list_iterator_reset_inline(_sion_filedesc *sion_filedesc);
int _sion_key_list_iterator_next_inline(_sion_filedesc *sion_filedesc, uint64_t *keyptr);

/* fills a stat-data structure containing information about the key */
int _sion_key_get_stat_inline(_sion_filedesc *sion_filedesc, uint64_t key, sion_key_stat_t *keystat);

#endif
