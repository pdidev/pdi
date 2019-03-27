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
#include <sys/types.h>
#include <sys/mman.h>
#include <assert.h>

#include "sion.h"
#include "sion_keyvalue_keymngr.h"

int main(int argc, char **argv)
{
  /* ------ */
  /* PROLOG */
  /* ------ */

  #define TABLE_SIZE 17
  /* ------------------------ */
  /* TEST A: test key manager */
  /* ------------------------ */
  {
    int i, rc;
    sion_table_key_t key;
    size_t len, offset, current_pos, bytes_left;
    _sion_keyvalue_keymngr *keymngr;

    keymngr = _sion_keyvalue_keymngr_init(TABLE_SIZE);
    if (keymngr == NULL) {
      fprintf(stderr, "kmngr_test: cannot initialize keyvalue manager of size %lu, aborting ...\n", (unsigned long) TABLE_SIZE);
    }
    printf("_sion_keyvalue_keymngr_init: returns valid manager\n");

    /* store key 4711 */
    key = 4711; offset = 1; len = 16;
    rc=_sion_keyvalue_keymngr_add_block(keymngr, key, offset, len);
    printf("_sion_keyvalue_keymngr_add_block: key=%ld offset=%zu len=%zu returns rc=%d\n",(long) key, offset, len, rc);

    /* store key 4811 */
    key = 4811; offset = 2; len = 32;
    rc=_sion_keyvalue_keymngr_add_block(keymngr, key, offset, len);
    printf("_sion_keyvalue_keymngr_add_block: key=%ld offset=%zu len=%zu returns rc=%d\n",(long) key, offset, len, rc);

    /* store key 4711 */
    key = 4711; offset = 3; len = 17;
    rc=_sion_keyvalue_keymngr_add_block(keymngr, key, offset, len);
    printf("_sion_keyvalue_keymngr_add_block: key=%ld offset=%zu len=%zu returns rc=%d\n",(long) key, offset, len, rc);

    /* store key 4711 */
    key = 4711; offset = 4; len = 18;
    rc=_sion_keyvalue_keymngr_add_block(keymngr, key, offset, len);
    printf("_sion_keyvalue_keymngr_add_block: key=%ld offset=%zu len=%zu returns rc=%d\n",(long) key, offset, len, rc);

    /* store key 4811 */
    key = 4811; offset = 5; len = 33;
    rc=_sion_keyvalue_keymngr_add_block(keymngr, key, offset, len);
    printf("_sion_keyvalue_keymngr_add_block: key=%ld offset=%zu len=%zu returns rc=%d\n",(long) key, offset, len, rc);

    /* lookup key 4711 */
    key = 4711;
    rc=_sion_keyvalue_keymngr_lookup(keymngr, key, &current_pos, &bytes_left);
    if (rc == SION_SUCCESS) {
      printf("_sion_keyvalue_keymngr_lookup: key=%ld returns current_pos=%zu bytes_left=%zu\n",(long) key, current_pos, bytes_left);
    } else {
      printf("_sion_keyvalue_keymngr_lookup: lookup of key=%ld failed\n", (long) key);
    }

    /* lookup key 1004711 */
    key = 1004711;
    rc=_sion_keyvalue_keymngr_lookup(keymngr, key, &current_pos, &bytes_left);
    if (rc == SION_SUCCESS) {
      printf("_sion_keyvalue_keymngr_lookup: key=%ld returns current_pos=%zu bytes_left=%zu\n",(long) key, current_pos, bytes_left);
    } else {
      printf("_sion_keyvalue_keymngr_lookup: lookup of key=%ld failed\n", (long) key);
    }

    /* lookup key 4811 */
    key = 4811;
    rc=_sion_keyvalue_keymngr_lookup(keymngr, key, &current_pos, &bytes_left);
    if (rc == SION_SUCCESS) {
      printf("_sion_keyvalue_keymngr_lookup: key=%ld returns current_pos=%zu bytes_left=%zu\n",(long) key, current_pos, bytes_left);
    } else {
      printf("_sion_keyvalue_keymngr_lookup: lookup of key=%ld failed\n", (long) key);
    }

    /* dump manager */
    rc=_sion_keyvalue_keymngr_iterator_reset(keymngr);
    printf("_sion_keyvalue_keymngr_iterator_reset: returns rc=%d\n", rc);
    i=0;
    while (_sion_keyvalue_keymngr_iterator_next(keymngr, &key, &current_pos , &offset, &len) == SION_SUCCESS) {
      printf("manager dump[%2d]: key=%ld current_pos=%zu offset=%zu len=%zu\n", i++, (long) key, current_pos, offset, len);
    }

    /* now fill the manager */
    for(i=0;i<TABLE_SIZE*2;i++) {
      key = 4911 + (i % (TABLE_SIZE / 2));
      offset = 10 + i;
      len = 11 + i;
      rc=_sion_keyvalue_keymngr_add_block(keymngr, key, offset, len);
      printf("_sion_keyvalue_keymngr_add_block: key=%ld offset=%zu len=%zu returns rc=%d\n",(long) key, offset, len, rc);
    }

    /* dump manager again */
    rc=_sion_keyvalue_keymngr_iterator_reset(keymngr);
    printf("_sion_keyvalue_keymngr_iterator_reset: returns rc=%d\n", rc);
    i=0;
    while (_sion_keyvalue_keymngr_iterator_next(keymngr, &key, &current_pos , &offset, &len) == SION_SUCCESS) {
      printf("manager dump[%2d]: key=%ld current_pos=%zu offset=%zu len=%zu\n", i++, (long) key, current_pos, offset, len);
    }

    /* destroy manager */
    rc=_sion_keyvalue_keymngr_destroy(&keymngr);
    printf("_sion_keyvalue_keymngr_destroy: returns rc=%d\n",rc);

  }


  /* ------ */
  /* EPILOG */
  /* ------ */

  return(0);

}
