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
#include "sion_debug.h"
#include "sion_keyvalue_table.h"

int main(int argc, char **argv)
{

  _sion_debug_init();
  DPRINTFP((8, "test_ktab___2", 1, "init debugging\n"));

#define TABLE_SIZE 127
  /* -------------------------- */
  /* TEST A: test with wrong parameters */
  /* -------------------------- */
  {
    int rc;
    _sion_keyvalue_table *key_table;
    int *data1, data;
    void *p;
    sion_table_key_t key;
    int i;

    /* keys from score-p */
    uint64_t keys[64] = {
		       103079215135ULL, 107374182431ULL, 111669149727ULL, 115964117023ULL, 120259084319ULL, 
		       124554051615ULL, 128849018911ULL, 12884901919ULL, 133143986207ULL, 137438953503ULL, 
		       141733920799ULL, 146028888095ULL, 150323855391ULL, 154618822687ULL, 158913789983ULL, 
		       163208757279ULL, 167503724575ULL, 171798691871ULL, 17179869215ULL, 176093659167ULL, 
		       180388626463ULL, 184683593759ULL, 188978561055ULL, 193273528351ULL, 197568495647ULL, 
		       201863462943ULL, 206158430239ULL, 210453397535ULL, 214748364831ULL, 21474836511ULL, 
		       219043332127ULL, 223338299423ULL, 227633266719ULL, 231928234015ULL, 236223201311ULL, 
		       240518168607ULL, 244813135903ULL, 249108103199ULL, 253403070495ULL, 257698037791ULL, 
		       25769803807ULL, 261993005087ULL, 266287972383ULL, 270582939679ULL, 30064771103ULL, 
		       31ULL, 34359738399ULL, 38654705695ULL, 42949672991ULL, 4294967327ULL, 
		       47244640287ULL, 51539607583ULL, 55834574879ULL, 60129542175ULL, 64424509471ULL, 
		       68719476767ULL, 73014444063ULL, 77309411359ULL, 81604378655ULL, 85899345951ULL, 
		       8589934623ULL, 90194313247ULL, 94489280543ULL, 98784247839ULL   }; 

    key_table = _sion_keyvalue_table_init(TABLE_SIZE);
    if (key_table == NULL) {
      fprintf(stderr, "ktab_test: cannot initialize keyvalue table of size %lu, aborting ...\n", (unsigned long) TABLE_SIZE);
    }
    printf("_sion_keyvalue_table_init: returns valid table\n");


    for(i=0;i<64;i++) { 

      /* store key 4711 */
      data1=(int*)malloc(sizeof(int));assert(data1!=NULL);
      key=keys[i];*data1=key;
      rc=_sion_keyvalue_table_store(key_table,key,data1);
      printf("_sion_keyvalue_table_store: key=%llu data=%llu returns rc=%d\n",(unsigned long long) key,(unsigned long long) *data1, rc);

    }

    /* dump table */
    i=0;
    _sion_keyvalue_table_iterator_reset(key_table);
    while(_sion_keyvalue_table_iterator_next(key_table,&key,&p)==SION_SUCCESS) {
      data= * (int *) p;
      printf("table dump[%2d]: key=%llu data=%llu\n",++i,(unsigned long long) key, (unsigned long long) data);
    }

    /* remove table */
    rc=_sion_keyvalue_table_destroy(&key_table);
    printf("_sion_keyvalue_table_destroy: returns rc=%d\n",rc);

  }

  /* cleanup debug data structures */
  sion_dclose();

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  
  return(0);
  
}
