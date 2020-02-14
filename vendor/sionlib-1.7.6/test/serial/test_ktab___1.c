/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
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
#include "sion_keyvalue_table.h"
#include "sion_debug.h"

int main(int argc, char **argv)
{

  _sion_debug_init();
  DPRINTFP((8, "test_ktab___2", 1, "init debugging\n"));

  #define TABLE_SIZE 17
  /* -------------------------- */
  /* TEST A: test with wrong parameters */
  /* -------------------------- */
  {
    int rc;
    _sion_keyvalue_table *key_table;
    int *data1, *data2, data;
    void *p;
    sion_table_key_t key;
    int i;

    key_table = _sion_keyvalue_table_init(TABLE_SIZE);
    if (key_table == NULL) {
      fprintf(stderr, "ktab_test: cannot initialize keyvalue table of size %lu, aborting ...\n", (unsigned long) TABLE_SIZE);
    }
    printf("_sion_keyvalue_table_init: returns valid table\n");


    /* store key 4711 */
    data1=(int*)malloc(sizeof(int));assert(data1!=NULL);
    key=4711;*data1=key*10000;
    rc=_sion_keyvalue_table_store(key_table,key,data1);
    printf("_sion_keyvalue_table_store: key=%ld data=%ld returns rc=%d\n",(long) key,(long) *data1, rc);

    /* store key 4811 */
    data2=(int*)malloc(sizeof(int));assert(data2!=NULL);
    key=4811;*data2=key*10000;
    rc=_sion_keyvalue_table_store(key_table,key,data2);
    printf("_sion_keyvalue_table_store: key=%ld data=%ld returns rc=%d\n",(long) key,(long) *data2, rc);

    /* lookup key 4711 */
    key=4711; p=_sion_keyvalue_table_lookup(key_table,key);
    if(p) {
      data= * (int *) p;
      printf("_sion_keyvalue_table_lookup: key=%ld returns data=%d\n",(long) key,data);
    } else {
      printf("_sion_keyvalue_table_lookup: key=%ld returns not found\n",(long) key);
    }

    /* lookup key 1004711 */
    key=1004711;p=_sion_keyvalue_table_lookup(key_table,key);
    if(p) {
      data= * (int *) p;
      printf("_sion_keyvalue_table_lookup: key=%ld returns data=%d\n",(long) key,data);
    } else {
      printf("_sion_keyvalue_table_lookup: key=%ld returns not found\n",(long) key);
    }

    /* lookup key 4811 */
    key=4811;p=_sion_keyvalue_table_lookup(key_table,key);
    if(p) {
      data= * (int *) p;
      printf("_sion_keyvalue_table_lookup: key=%ld returns data=%d\n",(long) key,data);
    } else {
      printf("_sion_keyvalue_table_lookup: key=%ld returns not found\n",(long) key);
    }

    /* dump table */
    i=0;
    _sion_keyvalue_table_iterator_reset(key_table);
    while(_sion_keyvalue_table_iterator_next(key_table,&key,&p)==SION_SUCCESS) {
      data= * (int *) p;
      printf("table dump[%2d]: key=%ld data=%d\n",++i,(long) key,data);
    }

    /* now fill the table */
    for(i=0;i<TABLE_SIZE*2;i++) {
      data1=(int*)malloc(sizeof(int));assert(data1!=NULL);
      key=4911+i;*data1=key*10000;
      rc=_sion_keyvalue_table_store(key_table,key,data1);
      printf("_sion_keyvalue_table_store: key=%ld data=%ld returns rc=%d\n",(long) key,(long) *data1, rc);
    }

    /* dump table */
    i=0;
    _sion_keyvalue_table_iterator_reset(key_table);
    while(_sion_keyvalue_table_iterator_next(key_table,&key,&p)==SION_SUCCESS) {
      data= * (int *) p;
      printf("table dump (key order) [%2d]: key=%ld data=%d\n",++i,(long) key,data);
    }

    /* dump table in store order */
    i=0;
    _sion_keyvalue_table_iterator_reset(key_table);
    while(_sion_keyvalue_table_iterator_next_in_store_order(key_table,&key,&p)==SION_SUCCESS) {
      data= * (int *) p;
      printf("table dump (store order) [%2d]: key=%ld data=%d\n",++i,(long) key,data);
    }

    /* dump table again */
    rc=_sion_keyvalue_table_destroy(&key_table);
    printf("_sion_keyvalue_table_destroy: returns rc=%d\n",rc);

  }


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */

  /* cleanup debug data structures */
  sion_dclose();

  return(0);
  
}
