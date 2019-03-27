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
 * Flags handling
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "sion.h"
#include "sion_flags.h"
#include "sion_internal.h"


#define DFUNCTION "_sion_flags_init_entry"
void _sion_flags_init_entry(_sion_flags_entry* entry)
{
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  entry->key  = NULL;
  entry->val  = NULL;
  entry->next = NULL;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_create_entry"
/*! \brief Create a flags entry
 *
 * Use _sion_flags_destroy_entry to free the memory when not needed any longer.
 *
 * @return     flag entry
 */
_sion_flags_entry* _sion_flags_create_entry(void)
{
  _sion_flags_entry* new_entry = NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  new_entry = (_sion_flags_entry*)malloc(sizeof (_sion_flags_entry));
  _sion_flags_init_entry(new_entry);

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));

  return new_entry;
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_destroy_entry"
void _sion_flags_destroy_entry(_sion_flags_entry** entry)
{
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  free((*entry)->key);
  free((*entry)->val);
  free(*entry);
  *entry = NULL;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_iter"
_sion_flags_entry* _sion_flags_iter(_sion_flags_store* store)
{
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  DPRINTFP((2, DFUNCTION, -1, "leave\n"));

  return store->root;
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_init_store"
void _sion_flags_init_store(_sion_flags_store* store)
{
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  store->root    = _sion_flags_create_entry();
  store->current = store->root;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_create_store"
/*! \brief Create a flags entry
 *
 * Use _sion_flags_destroy_store to free the memory when not needed any longer.
 *
 * @return     flag store with flag entries (key value pairs)
 */
_sion_flags_store* _sion_flags_create_store(void)
{
  _sion_flags_store* new_store = NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  new_store = (_sion_flags_store*)malloc(sizeof (_sion_flags_store));
  _sion_flags_init_store(new_store);

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));

  return new_store;
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_destroy_store"
void _sion_flags_destroy_store(_sion_flags_store** store)
{
  _sion_flags_entry* entry = NULL;
  _sion_flags_entry* next  = NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  entry = _sion_flags_iter(*store);

  while (entry != NULL) {
    next = entry->next;
    _sion_flags_destroy_entry(&entry);
    entry = next;
  }

  free(*store);
  *store = NULL;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_add_entry"
/*! \brief Create a flags entry
 *
 * Memory is deallocated in _sion_flags_destroy_entry.
 *
 * @param[in,out]   entry   current flag entry to add key and value to
 * @param[in]       key     key
 * @param[in]       val     value
 *
 * @return     flag store with key value pairs
 */
_sion_flags_entry* _sion_flags_add_entry(_sion_flags_entry* entry,
                                         const char*        key,
                                         const char*        val)
{
  _sion_flags_entry* new_entry = NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* include terminating null */
  entry->key = (char*)malloc(strlen(key) + 1);
  entry->val = (char*)malloc(strlen(val) + 1);
  new_entry  = _sion_flags_create_entry();
  strcpy(entry->key, key);
  strcpy(entry->val, val);
  entry->next = new_entry;

  DPRINTFP((2, DFUNCTION, -1, "leave (entry->key = %s, entry->val = %s\n",
            entry->key, entry->val));

  return new_entry;
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_add"
void _sion_flags_add(_sion_flags_store* store,
                     const char*        key,
                     const char*        val)
{
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  store->current = _sion_flags_add_entry(store->current, key, val);

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_get"
_sion_flags_entry* _sion_flags_get(_sion_flags_store* store, const char* key)
{
  _sion_flags_entry* iter = NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  for (iter = _sion_flags_iter(store); iter->next != NULL; iter = iter->next) {
    if (!strcmp(iter->key, key)) {
      DPRINTFP((2, DFUNCTION, -1, "leave (key in store)\n"));
      return iter;
    }
  }

  DPRINTFP((2, DFUNCTION, -1, "leave (key not in store)\n"));

  return NULL;
}
#undef DFUNCTION

#define DFUNCTION "_sion_flags_update_mask"
sion_int64 _sion_flags_update_mask(_sion_flags_store* store)
{
  _sion_flags_entry* iter = NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  store->mask = 0;
  store->mask |= _SION_FMODE_ANSI;

  for (iter = _sion_flags_iter(store); iter->next != NULL; iter = iter->next) {
    if ((!strcmp(iter->key, "w")) || (!strcmp(iter->key, "wb")) ||
        (!strcmp(iter->key, "bw"))) {
      store->mask |= _SION_FMODE_WRITE;
    }
    else if ((!strcmp(iter->key, "r")) || (!strcmp(iter->key, "rb")) ||
             (!strcmp(iter->key, "br"))) {
      store->mask |= _SION_FMODE_READ;
    }
    else if (!strcmp(iter->key, "buffered")) {
      store->mask |= _SION_FMODE_BUFFERED;
    }
    else if (!strcmp(iter->key, "buddy")) {
      store->mask |= _SION_FMODE_BUDDY;
    }
    else if (!strcmp(iter->key, "compress")) {
      store->mask |= _SION_FMODE_COMPRESS;
    }
    else if (!strcmp(iter->key, "collective")) {
      store->mask |= _SION_FMODE_COLLECTIVE;
    }
    else if ((!strcmp(iter->key, "collectivemerge")) ||
             (!strcmp(iter->key, "cmerge"))) {
      store->mask |= _SION_FMODE_COLLECTIVE_MERGE;
    }
    else if (!strcmp(iter->key, "keyval")) {
      if ((!strcmp(iter->val, "default")) || (!strcmp(iter->val, "inline")) ||
          (!strcmp(iter->val, ""))) {
        store->mask |= _SION_FMODE_KEYVAL_INLINE;
      }
      else if (!strcmp(iter->key, "meta")) {
        store->mask |= _SION_FMODE_KEYVAL_META;
      }
      else if (!strcmp(iter->key, "hash")) {
        store->mask |= _SION_FMODE_KEYVAL_HASH;
      }
      else if (!strcmp(iter->key, "none")) {
        store->mask |= _SION_FMODE_KEYVAL_NONE;
      }
      else if (!strcmp(iter->key, "unknown")) {
        store->mask |= _SION_FMODE_KEYVAL_UNKNOWN;
      }
    }
    else if (!strcmp(iter->key, "endianness")) {
      store->mask |= _SION_FMODE_ENDIANNESS_SET;
      if (!strcmp(iter->val, "big")) {
        store->mask |= _SION_FMODE_ENDIANNESS_BIG;
      }
    }
    else if (!strcmp(iter->key, "posix")) {
      store->mask |= _SION_FMODE_POSIX;
      store->mask ^= store->mask & _SION_FMODE_ANSI;
    }
    else if (!strcmp(iter->key, "ansi")) {
      store->mask |= _SION_FMODE_ANSI;
      store->mask ^= store->mask & _SION_FMODE_POSIX;
    }
  }

  DPRINTFP((2, DFUNCTION, -1, "leave (mask = %llx)\n", store->mask));

  return store->mask;
}
#undef DFUNCTION

#define DFUNCTION "_sion_parse_flags"
/*! \brief Parse flags and return a flags store with key value pairs
 *
 * @param[in]  flags   string containing the open flags
 *
 * @return     flag store with key value pairs
 */
_sion_flags_store* _sion_parse_flags(const char* flags)
{
  char*              tmps      = NULL;
  char*              key       = NULL;
  char*              val       = NULL;
  char*              pch       = NULL;
  char*              saveptr   = NULL;
  const char*        delim_tok = ",";
  const char*        delim_kv  = "=";
  int                span      = 0;
  /* include terminating null */
  const int          len       = strlen(flags) + 1;
  _sion_flags_store* store     = NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  store = _sion_flags_create_store();

  tmps = (char*)malloc(len);
  key  = (char*)malloc(len);
  val  = (char*)malloc(len);
  strcpy(tmps, flags);

  DPRINTF((32, DFUNCTION, -1, "tmps = '%s'\n", tmps));
  DPRINTF((32, DFUNCTION, -1, "delim_tok = '%s'\n", delim_tok));
  DPRINTF((32, DFUNCTION, -1, "delim_kv = '%s'\n", delim_kv));
  pch = strtok_r(tmps, delim_tok, &saveptr);
  while (pch != NULL) {
    span = strcspn(pch, delim_kv);
    strncpy(key, pch, span);
    key[span] = '\0';
    if (span < strlen(pch)) {
      strcpy(val, &pch[span + 1]);
    }
    else {
      val[0] = '\0';
    }
    DPRINTF((32, DFUNCTION, -1, "key = '%s'  |  val = '%s'\n", key, val));
    _sion_flags_add(store, key, val);
    pch = strtok_r(NULL, delim_tok, &saveptr);
  }

  free(tmps);
  free(key);
  free(val);

  _sion_flags_update_mask(store);

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));

  return store;
}
#undef DFUNCTION
