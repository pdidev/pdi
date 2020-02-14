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

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>


#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_printts.h"
#include "sion_keyvalue_table.h"

typedef enum {
   KEYVALUE_TABLE_ENTRY_STATE_USED,
   KEYVALUE_TABLE_ENTRY_STATE_FREE,
   KEYVALUE_TABLE_ENTRY_STATE_UNKNOWN
} sion_keyvalue_table_entry_state_t;

typedef struct _sion_keyvalue_table_entry_struct _sion_keyvalue_table_entry;

struct _sion_keyvalue_table_struct {
  int used;
  int size;
  int num_added_entries;

  /* for iterator by index */
  int iterator_lastreadindex;
  _sion_keyvalue_table_entry* iterator_lastreadentry;

  /* for iterator by store orde */
  _sion_keyvalue_table_entry* iterator_next;
  _sion_keyvalue_table_entry* iterator_head;
  _sion_keyvalue_table_entry* iterator_tail;

  _sion_keyvalue_table_entry* entries;
};

struct _sion_keyvalue_table_entry_struct {
  sion_keyvalue_table_entry_state_t state;
  sion_table_key_t                  key;
  _sion_keyvalue_table_entry       *iterator_next;
  _sion_keyvalue_table_entry       *next;
  void                             *data;
};

#define HASH_FCT_SIMPLE_not
#define HASH_FCT_SPLIT

#define DFUNCTION "_sion_keyvalue_table_hash_fct"
unsigned int _sion_keyvalue_table_hash_fct(sion_table_key_t key, int tab_size) {
  unsigned int index=0;

#if defined(HASH_FCT_SIMPLE)
  /* simple modulo */
  { 
    index = (unsigned int) key % tab_size;
  }
#elif defined(HASH_FCT_SPLIT)
  {
  /* split modulo */
    uint32_t upper =( uint32_t) (key >> 32);
    uint32_t lower =( uint32_t) (key & 0xffffffff);
    DPRINTFP((2, DFUNCTION, -1, "key %ld -> %d %d \n",(long) key, (int) upper, (int) lower));
    index = (unsigned int) ( 
			    (unsigned int) upper % tab_size 
			    + (unsigned int) lower % tab_size 
			     ) % tab_size;
  }
#endif  

  return(index);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_init"
_sion_keyvalue_table* _sion_keyvalue_table_init(int size) {
  _sion_keyvalue_table*        table=NULL;
  _sion_keyvalue_table_entry*  entries=NULL;
  int i;

  DPRINTFP((2, DFUNCTION, -1, "enter init size=%d\n",size));

  table = malloc(sizeof(_sion_keyvalue_table));
  if (table == NULL) {
    _sion_errorprint(0,_SION_ERROR_RETURN,"cannot allocate internal keyvalue table of size %lu , aborting ...\n", (unsigned long) sizeof(_sion_keyvalue_table));
    return(NULL);
  }

  entries = malloc(size * sizeof(_sion_keyvalue_table_entry));
  if (entries == NULL) {
    _sion_errorprint(0,_SION_ERROR_RETURN,"cannot allocate internal keyvalue table entries of size %lu , aborting ...\n", (unsigned long) size);
    free(table);
    return(NULL);
  }
  table->entries=entries;

  table->size=size;
  table->used=0;
  table->num_added_entries=0;
  table->iterator_lastreadindex=-1;
  table->iterator_lastreadentry=NULL;
  table->iterator_next=NULL;
  table->iterator_head=NULL;
  table->iterator_tail=NULL;
  for(i=0;i<table->size;i++) {
    table->entries[i].state=KEYVALUE_TABLE_ENTRY_STATE_FREE;
    table->entries[i].key=0;
    table->entries[i].iterator_next=NULL;
    table->entries[i].next=NULL;
    table->entries[i].data=NULL;
  }


  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (table);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_destroy"
int  _sion_keyvalue_table_destroy(_sion_keyvalue_table** table) {
  size_t rc=SION_SUCCESS;
  int i;
  _sion_keyvalue_table_entry *entry,*next_entry;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  if((*table)->entries) {


    for(i=0;i<(*table)->size;i++) {
      /* free entry data */
      if(((*table)->entries[i].state!=KEYVALUE_TABLE_ENTRY_STATE_FREE) && ((*table)->entries[i].data!=NULL)) {
	_SION_SAFE_FREE((*table)->entries[i].data, NULL);
      } 
      /* free linked list */
      entry=(*table)->entries[i].next;
      while (entry != NULL) {
	if((entry->state!=KEYVALUE_TABLE_ENTRY_STATE_FREE) && (entry->data!=NULL)) {
	  _SION_SAFE_FREE(entry->data, NULL);
	} 
	next_entry=entry->next;
	_SION_SAFE_FREE(entry, NULL);
	entry = next_entry;
      }
    }
     
    free((*table)->entries);
    (*table)->entries=NULL; 
  }
  free(*table);
  *table = NULL;
 

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_store"
int _sion_keyvalue_table_store(_sion_keyvalue_table* table, sion_table_key_t key, void *data) {
   size_t rc=SION_SUCCESS;
   unsigned int index;
   _sion_keyvalue_table_entry *new_entry;

   DPRINTFP((2, DFUNCTION, -1, "enter\n"));

   index = _sion_keyvalue_table_hash_fct(key,table->size);
   DPRINTFP((2, DFUNCTION, -1, "store entry with key %ld index=%d\n", (long) key, index));
   
   new_entry = &table->entries[index];
   if (new_entry->state != KEYVALUE_TABLE_ENTRY_STATE_FREE) {
     while (new_entry->next != NULL) {
       new_entry = new_entry->next;
     }
     new_entry->next = (_sion_keyvalue_table_entry*) malloc(sizeof(_sion_keyvalue_table_entry));
     if (new_entry->next == NULL) {
       return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate internal keyvalue table entry, aborting ...\n"));
     }
     DPRINTFP((2, DFUNCTION, -1, "add new entry to next list\n"));
     table->num_added_entries++;
     new_entry = new_entry->next;
   }
   table->used++;

   new_entry->state= KEYVALUE_TABLE_ENTRY_STATE_USED;
   new_entry->key  = key;
   new_entry->data = data;
   new_entry->next = NULL;
   new_entry->iterator_next = NULL;

   if ( (table->iterator_head==NULL) && (table->iterator_tail==NULL) ) {
     /* first entry */
     table->iterator_next=table->iterator_head=table->iterator_tail=new_entry;
   } else {
     table->iterator_tail->iterator_next=new_entry;
     table->iterator_tail=table->iterator_tail->iterator_next; /* advance tail */
   }
   
   
   DPRINTFP((2, DFUNCTION, -1, "new entry successfully added (key %ld index=%d)\n", (long) key, index));
   DPRINTFP((2, DFUNCTION, -1, "TABLE[slots %d of %d, +%d, total %d] \n", table->used - table->num_added_entries,table->size,table->num_added_entries,table->used));

   DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_lookup"
void * _sion_keyvalue_table_lookup(_sion_keyvalue_table* table, sion_table_key_t key) {
  unsigned int index;
  _sion_keyvalue_table_entry *entry;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  index = _sion_keyvalue_table_hash_fct(key,table->size);
  DPRINTFP((2, DFUNCTION, -1, "lookup entry with key %ld index=%d\n", (long) key, index));
   
  entry = &(table->entries[index]);
  while (entry != NULL) {

    DPRINTFP((2, DFUNCTION, -1, "search: state=%ld\n",(long) entry->state ));
    
    if (entry->state == KEYVALUE_TABLE_ENTRY_STATE_USED) {
      if(entry->key == key) {
	DPRINTFP((2, DFUNCTION, -1, "key found %ld in key list\n",(long) entry->key ));
	return(entry->data);
      } else {
	DPRINTFP((2, DFUNCTION, -1, "skip this key another key found %ld in list\n",(long) entry->key ));
	entry = entry->next;
      }
    } else {
      /* slot is empty */
      return(NULL);
    }
  } 
  DPRINTFP((2, DFUNCTION, -1, "leave:entry not found\n"));
  return(NULL);

}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_iterator_reset"
int _sion_keyvalue_table_iterator_reset(_sion_keyvalue_table* table) {

  int rc=SION_SUCCESS;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  table->iterator_lastreadindex=-1;
  table->iterator_lastreadentry=NULL;
  table->iterator_next=table->iterator_head;
  DPRINTFP((2, DFUNCTION, -1, "leave\n"));

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_iterator_next_in_store_order"
int _sion_keyvalue_table_iterator_next_in_store_order(_sion_keyvalue_table* table, sion_table_key_t *key, void **data) {
  _sion_keyvalue_table_entry *entry;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  if(table->used==0) {
    return(SION_NOT_SUCCESS);
  }
  entry=table->iterator_next;

  if(entry != NULL) {
    *key=entry->key;
    *data=entry->data;
    DPRINTFP((2, DFUNCTION, -1, "found entry with key %ld\n",(long) *key));
    table->iterator_next=entry->iterator_next; /* move forward in iterator list */
    return(SION_SUCCESS);
  } else {
    return(SION_NOT_SUCCESS);
  }
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_iterator_next"
int _sion_keyvalue_table_iterator_next(_sion_keyvalue_table* table, sion_table_key_t *key, void **data) {

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  if(table->used==0) {
    return(SION_NOT_SUCCESS);
  }
  if(table->iterator_lastreadindex==-1) {
    /* search first entry which must be there (used>0) */
    DPRINTFP((2, DFUNCTION, -1, "start first\n"));
    table->iterator_lastreadindex++;
    while(table->iterator_lastreadindex < table->size) {
      if (table->entries[table->iterator_lastreadindex].state == KEYVALUE_TABLE_ENTRY_STATE_USED) {
	  table->iterator_lastreadentry = &table->entries[table->iterator_lastreadindex];
	  break;
      }
      table->iterator_lastreadindex++;
    }
    DPRINTFP((2, DFUNCTION, -1, "start first, found entry at index %d\n",table->iterator_lastreadindex));
    
  } else {

    /* another entry in list? */
    if(table->iterator_lastreadentry->next != NULL) {
      table->iterator_lastreadentry=table->iterator_lastreadentry->next;
      DPRINTFP((2, DFUNCTION, -1, "found another entry in list at index %d\n",table->iterator_lastreadindex));
    } else { 
      /* search forward */
      table->iterator_lastreadindex++;
      while(table->iterator_lastreadindex < table->size) {
	if (table->entries[table->iterator_lastreadindex].state == KEYVALUE_TABLE_ENTRY_STATE_USED) {
	  table->iterator_lastreadentry = &table->entries[table->iterator_lastreadindex];
	  DPRINTFP((2, DFUNCTION, -1, "found entry at another index %d\n",table->iterator_lastreadindex));
	  break;
	}
	table->iterator_lastreadindex++;
      }
    }
  }
  if(table->iterator_lastreadindex < table->size) {
    *key=table->iterator_lastreadentry->key;
    *data=table->iterator_lastreadentry->data;
    DPRINTFP((2, DFUNCTION, -1, "leave: entry found\n"));
    return(SION_SUCCESS);
  } else {
    DPRINTFP((2, DFUNCTION, -1, "leave: next entry not found\n"));
    return(SION_NOT_SUCCESS);
  }
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_table_get_size"
int  _sion_keyvalue_table_get_size(_sion_keyvalue_table* table) {
  size_t help_bytes, bytes=0;
  int i;
  _sion_keyvalue_table_entry *entry;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  
  help_bytes= sizeof(_sion_keyvalue_table);
  DPRINTFP((2, DFUNCTION, -1, " sizeof key_table=          %5d\n", help_bytes));
  bytes+=help_bytes;

  if(table->entries) {
    
    help_bytes= table->size*sizeof(_sion_keyvalue_table_entry);
    DPRINTFP((2, DFUNCTION, -1, " sizeof key_entries=        %5d\n", help_bytes));
    bytes+=help_bytes;

    help_bytes=0;
    for(i=0;i<table->size;i++) {
      entry=table->entries[i].next;
      while (entry != NULL) {
	help_bytes+=sizeof(_sion_keyvalue_table_entry);
	entry = entry->next;
      }
    }

    DPRINTFP((2, DFUNCTION, -1, " sizeof key_addentries=     %5d\n", help_bytes));
    bytes+=help_bytes;
    
  }

  DPRINTFP((2, DFUNCTION, -1, "leave bytes=%d\n",bytes));
  return (bytes);
}
#undef DFUNCTION
