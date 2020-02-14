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

#ifndef SION_SION_KEYVALUE_TABLE_H
#define SION_SION_KEYVALUE_TABLE_H

#include <stdint.h>

#include "sion_const.h"
#include "sion_filedesc.h"

#define INITIAL_HASH_SIZE (1*1024)
typedef uint64_t sion_table_key_t;

typedef struct _sion_keyvalue_table_struct _sion_keyvalue_table;

_sion_keyvalue_table* _sion_keyvalue_table_init(int size);

int _sion_keyvalue_table_destroy(_sion_keyvalue_table** table);

int _sion_keyvalue_table_store(_sion_keyvalue_table* table, sion_table_key_t key, void *data);

void* _sion_keyvalue_table_lookup(_sion_keyvalue_table* table, sion_table_key_t key);

int _sion_keyvalue_table_iterator_reset(_sion_keyvalue_table* table);
int _sion_keyvalue_table_iterator_next(_sion_keyvalue_table* table, sion_table_key_t *key, void **data);
int _sion_keyvalue_table_iterator_next_in_store_order(_sion_keyvalue_table* table, sion_table_key_t *key, void **data);

int _sion_keyvalue_table_get_size(_sion_keyvalue_table* table);

#endif
