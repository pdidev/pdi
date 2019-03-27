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

#ifndef SION_SION_KEYVALUE_KEYMNGR_H
#define SION_SION_KEYVALUE_KEYMNGR_H

#include <stdint.h>

#include "sion_const.h"
#include "sion_common.h"
/* for key-type */
#include "sion_keyvalue_table.h"

/* keyvalue_keymngr: handles memory based tables storing blocks for each key */

typedef struct _sion_keyvalue_keymngr_struct _sion_keyvalue_keymngr;

_sion_keyvalue_keymngr* _sion_keyvalue_keymngr_init(int size);
int _sion_keyvalue_keymngr_destroy(_sion_keyvalue_keymngr** keymngr);

_sion_keyvalue_keymngr* _sion_keyvalue_keymngr_dup(_sion_keyvalue_keymngr* keymngr_orig, int dup_mode, sion_table_key_t sel_key);

/* add info (offset and len) about a new block for a key */
int _sion_keyvalue_keymngr_add_block(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, size_t offset, size_t len);

/* update internal data of a key by advancing current_pos of that key by bytes_read 
   if a block is fully read, the meta data is no longer needed and the block will be destroyed */
int _sion_keyvalue_keymngr_update_read_pos(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, size_t bytes_read, sion_int64 current_pos);

/* searches for info about a key, if available (SION_SUCCESS) current_pos and
   bytes_left are returned, otherwise SION_NOT_SUCCESS */
int _sion_keyvalue_keymngr_lookup(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, size_t *current_pos, size_t *bytes_left);

/* searches for info about a key at position (entrynum,posinentry), if
   available (SION_SUCCESS) current_pos and bytes_left are returned, otherwise
   SION_NOT_SUCCESS.  In addition, on success the current_block/pos of that
   key is set to this position */
int _sion_keyvalue_keymngr_lookup_and_set_pos(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, int entrynum, sion_int64 posinentry,
					      size_t *current_pos, size_t *bytes_left);

/* sets next position from where next meta data could be read */
int _sion_keyvalue_keymngr_set_next_scan_pos(_sion_keyvalue_keymngr* keymngr, size_t pos);

/* gets next position from where next meta data could be read */
int _sion_keyvalue_keymngr_get_next_scan_pos(_sion_keyvalue_keymngr* keymngr, size_t *pos);

/* get/setter for flag scan_done: all data about keys is stored in keymngr */
int _sion_keyvalue_keymngr_set_scan_done(_sion_keyvalue_keymngr* keymngr);
int _sion_keyvalue_keymngr_is_scan_done(_sion_keyvalue_keymngr* keymngr);



/* iterator to run over all blocks of all keys in write order */
int _sion_keyvalue_keymngr_iterator_reset(_sion_keyvalue_keymngr* keymngr);
/* current_pos is set to a value if current read position is in this block, otherwise -1  */
int _sion_keyvalue_keymngr_iterator_next(_sion_keyvalue_keymngr* keymngr, sion_table_key_t *key, size_t *current_pos, size_t *offset, size_t *len);

/* iterator to get key list */
int _sion_keyvalue_keymngr_key_list_iterator_reset(_sion_keyvalue_keymngr* keymngr);
int _sion_keyvalue_keymngr_key_list_iterator_next(_sion_keyvalue_keymngr* keymngr, sion_table_key_t *key);

int _sion_keyvalue_keymngr_key_get_stat(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, sion_key_stat_t *keystat);

int _sion_keyvalue_keymngr_key_get_sizeof(_sion_keyvalue_keymngr* keymngr);

#endif
