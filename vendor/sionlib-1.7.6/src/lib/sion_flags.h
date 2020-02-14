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
 * Flags handling
 */

#ifndef SION_SION_FLAGS_H
#define SION_SION_FLAGS_H

#include "sion_datatypes.h"

typedef struct _sion_flags_store_struct _sion_flags_store;
typedef struct _sion_flags_entry_struct _sion_flags_entry;

struct _sion_flags_store_struct
{
  _sion_flags_entry* root;
  _sion_flags_entry* current;
  sion_int64         mask;
};

struct _sion_flags_entry_struct
{
  char*              key;
  char*              val;
  _sion_flags_entry* next;
};

void _sion_flags_init_entry(_sion_flags_entry* entry);
_sion_flags_entry* _sion_flags_create_entry(void);
void _sion_flags_destroy_entry(_sion_flags_entry** entry);

_sion_flags_entry* _sion_flags_iter(_sion_flags_store* store);

void _sion_flags_init_store(_sion_flags_store* store);
_sion_flags_store* _sion_flags_create_store(void);
void _sion_flags_destroy_store(_sion_flags_store** store);

_sion_flags_entry* _sion_flags_add_entry(_sion_flags_entry* entry,
                                         const char*        key,
                                         const char*        val);
void _sion_flags_add(_sion_flags_store* store,
                     const char*        key,
                     const char*        val);
_sion_flags_entry* _sion_flags_get(_sion_flags_store* store,
                                   const char*        key);
sion_int64 _sion_flags_update_mask(_sion_flags_store* store);

_sion_flags_store* _sion_parse_flags(const char* flags);

#endif
