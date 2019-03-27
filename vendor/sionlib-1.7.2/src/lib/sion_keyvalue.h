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

#ifndef SION_SION_KEYVALUE_H
#define SION_SION_KEYVALUE_H

#include <stdint.h>

#include "sion_const.h"
#include "sion_common.h"
#include "sion_filedesc.h"

/* read data stored in record under 'key' in current trunk, SION will maintain a file
   position per key so that subsequent reads are starting at last position of
   current record */
size_t sion_fwrite_key(const void *data, uint64_t key, size_t size, size_t nitems, int sid);
size_t sion_fread_key(void *data,  uint64_t key, size_t size, size_t nitems, int sid);

/* this function searches for a key and on success the file position will
   be move to the searched key and position, so that the data can be read with
   sion_fread_key
   Parameters: 
   sid:        sion id
   key:        key within its data the search should be performed
   entrynum:   Number of occurence of entry (in write order), or
               SION_CURRENT_ENTRY, or
	       SION_ABSOLUTE_POS
   posinentry: byte-position in entry, or absolute byte-position in 
               all entries in write-order (SION_ABSOLUTE_POS)
*/
int sion_seek_key(int sid, uint64_t key, int entrynum, sion_int64 posinentry );

/* scans input file for all information about all keys of this rank */
int sion_key_full_scan(int sid);

/* fills a stat-data structure containing information about the key */
int sion_key_get_stat(int sid, uint64_t key, sion_key_stat_t *keystat);

/* iterator for list of keys  */
int sion_key_list_iterator_reset(int sid);
int sion_key_list_iterator_next(int sid, uint64_t *keyptr);

/* reset iterator for current task */
int sion_fread_key_iterator_reset(int sid);
/* get next record for current task, data itself has to be read with
   sion_fread_key; this function will forward to next record although if data
   of last record is not or only read by part */
int sion_fread_key_iterator_next(int sid, uint64_t *key, size_t *size);

int sion_get_keyval_mode (int sid);
char* sion_keyval_type_to_str (int type);

/* internal functions */
int _sion_keyval_check_env(_sion_filedesc *sion_filedesc, sion_int64 file_mode_flags);
int _sion_keyval_dup_dataptr(_sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc);

int _sion_store_and_write_key_and_len(_sion_filedesc *sion_filedesc, uint64_t key, size_t len);
int _sion_write_value(_sion_filedesc *sion_filedesc, const void *data, uint64_t key, size_t len);

int _sion_find_and_read_key_and_len(_sion_filedesc *sion_filedesc, uint64_t key, size_t len, size_t *datalen);
int _sion_read_value(_sion_filedesc *sion_filedesc, void *data, uint64_t key, size_t len);

#endif
