/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

#include "sion.h"
#include "sion_flags.h"

int main(int argc, char** argv)
{
  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  /* ------------------------------- */
  /* TEST A: Test key value handling */
  /* ------------------------------- */
  {
    int   idx   = 0;
    char* flags =
      "wb,buffered,keyfoo=valfoo,keybar=valbar,posix,ansi,k=v1=v2,l=";
    int   keys_len = 8;
    char* keys[]   = { "wb", "buffered", "keyfoo", "keybar", "k", "kl", "w",
                       "l" };


    _sion_flags_store* store = NULL;
    _sion_flags_entry* entry = NULL;


    printf("flags = %s\n", flags);

    store = _sion_parse_flags(flags);

    /* iterate over flags */
    for (entry = _sion_flags_iter(store); entry->next != NULL;
         entry = entry->next) {
      printf("key = '%s'  |  val = '%s'\n", entry->key, entry->val);
    }

    /* search for flags */
    for (idx = 0; idx < keys_len; idx++) {
      entry = _sion_flags_get(store, keys[idx]);
      if (entry) {
        printf("_sion_flags_get(store, \"%s\") = entry\n", keys[idx]);
        printf("entry->value = \"%s\"\n", entry->val);
      }
      else {
        printf("_sion_flags_get(store, \"%s\") = NULL\n", keys[idx]);
      }
    }

    /* check mask */
    printf("mask = 0x%llx\n", store->mask);
    _sion_flags_destroy_store(&store);
  }

  /* ---------------------- */
  /* TEST B: Test all flags */
  /* ---------------------- */
  {
    int   idx         = 0;
    int   flags_len   = 23;
    char* all_flags[] = {
      "",               "w",                      "wb",
      "bw",             "r",                      "rb",
      "br",             "buffered",               "compress",
      "collective",     "collectivemerge",        "cmerge",
      "keyval",         "keyval=default",         "keyval=inline",
      "keyval=",        "keyval=meta",            "keyval=hash",
      "keyval=non",     "keyval=unknown",         "endianness",
      "endianness=big", "posix"
    };


    _sion_flags_store* store = NULL;


    /* check mask */
    for (idx = 0; idx < flags_len; idx++) {
      store = _sion_parse_flags(all_flags[idx]);
      printf("flag = '%s', mask = 0x%llx\n", all_flags[idx], store->mask);
      _sion_flags_destroy_store(&store);
    }
  }

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */

  return 0;
}
