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
#include "sion_keyvalue_keymngr.h"
#include "sion_keyvalue_table.h"

typedef struct _sion_key_entry_struct       _sion_key_entry;
typedef struct _sion_key_entry_block_struct _sion_key_block_entry;

#define POS_BEHIND_END -302 

/*! overall structure where siondesc->keyvalptr points to */
struct _sion_keyvalue_keymngr_struct {
  int                   size;
  _sion_key_entry       *lastentry_used;          /*!< last entry of a for which data was handled */
  size_t                next_scan_pos;            /*!< absolute position of position from where next meta data could be read */
  int                    scan_done;               /*!< is true if all data about keys is stored in keymngr */
  _sion_keyvalue_table  *key_table;	          /*!< Table of key entries */
  _sion_key_block_entry *iterator_last_block;     /*!< is NULL is iterator was reseted or pointer to the last existing iterated block */
  _sion_key_block_entry *block_inwriteorder_head; /*!< pointer to first element of blocks in write order */
  _sion_key_block_entry *block_inwriteorder_tail; /*!< pointer to last element of blocks in write order */
};

/*! entry to keyvalue table, describes info of one key */
struct _sion_key_entry_struct {
  sion_int64 key;                           /*!< key */
  ssize_t current_pos;                      /*!< offset to current position in current block of key */
  size_t bytes_left;	                    /*!< bytes left in current block */
  int blocks_avail;	                    /*!< number of blocks stored in list */
  _sion_key_block_entry *blocklist_head;    /*!< pointer to first element of blocklist */
  _sion_key_block_entry *blocklist_current; /*!< pointer to current element of blocklist from which data will be read */
  _sion_key_block_entry *blocklist_tail;    /*!< pointer to last element of blocklist */
};

/*! describes one block containing data for a key, it is element of a linked list */
struct _sion_key_entry_block_struct {
  size_t offset; 		            /*!< absolute position of that block in file */
  size_t len;			            /*!< length of that block */
  int    blocknum;		            /*!< number of in squence of blocks for that entry */
  size_t offset_in_entry;	            /*!< absolute position of that block in data for that entry */
  _sion_key_block_entry *next;              /*!< pointer to next element of blocklist */
  _sion_key_block_entry *next_inwriteorder; /*!< pointer to next element of blocks in write order */
  _sion_key_entry       *entry;		    /*!< reference to entry containing this block */
};


#define DFUNCTION "_sion_keyvalue_keymngr_init"
_sion_keyvalue_keymngr* _sion_keyvalue_keymngr_init(int size) {
  _sion_keyvalue_keymngr*        keymngr=NULL;

  DPRINTFP((2, DFUNCTION, -1, "enter init size=%d\n",size));

  keymngr = malloc(sizeof(_sion_keyvalue_keymngr));
  if (keymngr == NULL) {
    _sion_errorprint(0,_SION_ERROR_RETURN,"cannot allocate internal keyvalue keymngr of size %lu , aborting ...\n", (unsigned long) sizeof(_sion_keyvalue_keymngr));
    return(NULL);
  }

  /* init main data structure */
  keymngr->lastentry_used= NULL;
  keymngr->next_scan_pos = 0;
  keymngr->scan_done = SION_NOT_SUCCESS;
  keymngr->iterator_last_block = NULL;
  keymngr->block_inwriteorder_head = NULL;
  keymngr->block_inwriteorder_tail = NULL;
  keymngr->size = size;
  keymngr->key_table     = _sion_keyvalue_table_init(size);
  if (keymngr->key_table == NULL) {
    _sion_errorprint(0,_SION_ERROR_RETURN,"cannot allocate internal keyvalue table of for %lu entries , aborting ...\n", (unsigned long) size);
    free(keymngr);
    return(NULL);
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (keymngr);
}
#undef DFUNCTION


#define DFUNCTION "_sion_keyvalue_keymngr_destroy"
int _sion_keyvalue_keymngr_destroy(_sion_keyvalue_keymngr** keymngr) {
   size_t rc=SION_SUCCESS;
   _sion_key_block_entry *block, *tmp_block;
   
   DPRINTFP((2, DFUNCTION, -1, "enter\n"));
   block = (*keymngr)->block_inwriteorder_head;
   while (block != NULL) {
     tmp_block = block;
     block = block->next_inwriteorder;
     free(tmp_block);
     tmp_block = NULL;
   }
   if((*keymngr)->key_table) {
     rc=_sion_keyvalue_table_destroy(&((*keymngr)->key_table));
   }
   free((*keymngr));
   *keymngr = NULL;
   
   DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
   return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_add_block"
int _sion_keyvalue_keymngr_add_block(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, size_t offset, size_t len) {
  size_t rc=SION_SUCCESS;
  _sion_key_entry *entry;
  _sion_key_block_entry *new_block;
  void *p;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* first check if key is known */
  p=_sion_keyvalue_table_lookup(keymngr->key_table,key);
  if( p==NULL ) {
    /* new key */

    entry = (_sion_key_entry*) malloc(sizeof(_sion_key_entry));
    if (entry == NULL) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate internal keyvalue table entry, aborting ...\n"));
    }
    entry->key         = key;
    entry->blocks_avail= 0;
    entry->blocklist_head = NULL;   /* block will be added later */
    entry->blocklist_current = NULL;   /* block will be added later */
    entry->blocklist_tail = NULL;   /* block will be added later */

    /* it is the first block of this key, 
       therefore initialize also info about current read pos */
    entry->current_pos = offset; /* it is the first block for that key */
    entry->bytes_left  = len;    /* and no bytes available so far in this block */
  
    p= (void *) entry;

    /* store new entry in keytable */
    rc=_sion_keyvalue_table_store(keymngr->key_table,key,p);
    if (rc != SION_SUCCESS) return(rc);
    
  } else {
    entry = (_sion_key_entry *) p;
  } 
  
  /* add new block */
  new_block = (_sion_key_block_entry*) malloc(sizeof(_sion_key_block_entry));
  if (new_block == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate internal keyvalue table block entry, aborting ...\n"));
  }
  new_block->entry  = entry;
  new_block->offset = offset;
  new_block->len    = len;
  new_block->next   = NULL; 	/* will be added at tail */
  new_block->next_inwriteorder = NULL; 	/* will be added at tail */

  /* push block to blocklist of entry */
  if((entry->blocklist_head != NULL) && (entry->blocklist_current != NULL) && (entry->blocklist_tail != NULL)) {
    /* list exists */
    new_block->blocknum         = entry->blocklist_tail->blocknum + 1;
    new_block->offset_in_entry  = entry->blocklist_tail->offset_in_entry + entry->blocklist_tail->len;
    entry->blocklist_tail->next = new_block;                       /* append block */
    entry->blocklist_tail       = entry->blocklist_tail->next;     /* advance tail pointer */

    /* advance currentpos if currentpos behind last blocks */
    if(entry->current_pos == POS_BEHIND_END) {
      entry->blocklist_current = new_block; /* it must be this block */
      entry->current_pos       = entry->blocklist_current->offset;
      entry->bytes_left        = entry->blocklist_current->len;
    }
    
  } else {
    /* init list */
    new_block->blocknum         = 0;
    new_block->offset_in_entry  = 0;
    entry->blocklist_head    = new_block;
    entry->blocklist_current = entry->blocklist_head;
    entry->blocklist_tail    = entry->blocklist_current;
    entry->current_pos       = entry->blocklist_current->offset;
    entry->bytes_left        = entry->blocklist_current->len;
  }
  entry->blocks_avail++;

  /* push block to lists of blocks in write order */
  if((keymngr->block_inwriteorder_head != NULL) && (keymngr->block_inwriteorder_tail != NULL)) {
    keymngr->block_inwriteorder_tail->next_inwriteorder = new_block;                       /* append block */
    keymngr->block_inwriteorder_tail                    = keymngr->block_inwriteorder_tail->next_inwriteorder;   /* advance tail pointer */
  } else {
    /* init list */
    keymngr->block_inwriteorder_head = new_block;                       
    keymngr->block_inwriteorder_tail = keymngr->block_inwriteorder_head;                       
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_update_read_pos"
int _sion_keyvalue_keymngr_update_read_pos(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, 
					   size_t bytes_read, sion_int64 current_pos) {
  size_t rc=SION_SUCCESS;
  _sion_key_entry *entry=NULL;
  void *p=NULL;

  /* check if using the same key as last time */
  if(keymngr->lastentry_used!=NULL) {
    if(keymngr->lastentry_used->key == key) {
      DPRINTFP((2, DFUNCTION, -1, "its the last key %ld\n",(long) key));
      entry=keymngr->lastentry_used;
    }
  }
  
  if( entry==NULL) {
    /* try to lookup */
    DPRINTFP((2, DFUNCTION, -1, "lookup for key %ld\n",(long) key));
    p=_sion_keyvalue_table_lookup(keymngr->key_table,key);
    if( p ) {
      DPRINTFP((2, DFUNCTION, -1, "key %ld found\n",(long) key));
      entry = (_sion_key_entry *) p;
    } else {
      DPRINTFP((2, DFUNCTION, -1, "key %ld not found\n",(long) key));
      rc=SION_NOT_SUCCESS;
    }
  }

  if(entry) {
    if(bytes_read<=entry->bytes_left) {

      /* advance counter */
      DPRINTFP((2, DFUNCTION, -1, "advance key counters by %ld bytes\n",(long) bytes_read));
      entry->current_pos = current_pos;
      entry->bytes_left -= bytes_read;

      /* check if block is completely read */
      if(entry->bytes_left==0) {
	DPRINTFP((2, DFUNCTION, -1, "block is completely read, move to next if possible\n"));

	/* next block available */
	if(entry->blocklist_current->next!=NULL) {

	  /* move forward to next block  */
	  entry->blocklist_current=entry->blocklist_current->next; 
	  entry->current_pos=entry->blocklist_current->offset;
	  entry->bytes_left=entry->blocklist_current->len;
	  DPRINTFP((2, DFUNCTION, -1, "another block found (%ld,%ld)\n", (long) entry->current_pos, (long) entry->bytes_left ));

	} else {
	  /* stay in current block, but behind end */
	  DPRINTFP((2, DFUNCTION, -1, "no other block found so far ...\n"));
	  entry->current_pos = POS_BEHIND_END;
	  entry->bytes_left  = 0;
	}

      }
      rc=SION_SUCCESS;
    } else {
      DPRINTFP((2, DFUNCTION, -1, "bytes_read > entry->bytes_left (%zu > %zu)\n", bytes_read, entry->bytes_left));
      rc=SION_NOT_SUCCESS;
    }
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return (rc);
  
}
#undef DFUNCTION


#define DFUNCTION "_sion_keyvalue_keymngr_set_next_scan_pos"
int _sion_keyvalue_keymngr_set_next_scan_pos(_sion_keyvalue_keymngr* keymngr, size_t pos) {
  size_t rc=SION_SUCCESS;
  DPRINTFP((2, DFUNCTION, -1, "enter pos=%ld\n",rc, (long) pos));
  keymngr->next_scan_pos=pos;
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d next_scan_pos=%ld\n",rc, (long) keymngr->next_scan_pos));
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_get_next_scan_pos"
int _sion_keyvalue_keymngr_get_next_scan_pos(_sion_keyvalue_keymngr* keymngr, size_t *pos) {
  size_t rc=SION_SUCCESS;
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  *pos=keymngr->next_scan_pos;

  if(*pos==0) {
    rc=SION_NOT_SUCCESS;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d pos=%ld\n",rc, (long) *pos));
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_set_scan_done"
int _sion_keyvalue_keymngr_set_scan_done(_sion_keyvalue_keymngr* keymngr) {
  size_t rc=SION_SUCCESS;
  keymngr->scan_done=SION_SUCCESS;
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d scan_done=%d\n",rc, (int) keymngr->scan_done));
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_is_scan_done"
int _sion_keyvalue_keymngr_is_scan_done(_sion_keyvalue_keymngr* keymngr) {
  size_t rc=keymngr->scan_done;
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return(rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_keyvalue_keymngr_lookup"
int _sion_keyvalue_keymngr_lookup(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, size_t *current_pos, size_t *bytes_left) {
  size_t rc=SION_SUCCESS;
  _sion_key_entry *entry;
  void *p;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* check if key is known */
  p=_sion_keyvalue_table_lookup(keymngr->key_table,key);
  if( p==NULL ) {
    rc=SION_NOT_SUCCESS;
  } else {
    rc=SION_SUCCESS;
    entry=(_sion_key_entry *) p;
    DPRINTFP((2, DFUNCTION, -1, "found entry with %d blocks\n",entry->blocks_avail));

    if(entry->current_pos != POS_BEHIND_END) {
      *current_pos = entry->current_pos;
      *bytes_left  = entry->bytes_left;
      DPRINTFP((2, DFUNCTION, -1, "found block #%d --> (%ld,%ld)\n",(int) entry->blocklist_current->blocknum,(long) *current_pos,(long) *bytes_left));

    } else {
      rc=SION_NOT_SUCCESS;
    }
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_lookup_and_set_pos"
int _sion_keyvalue_keymngr_lookup_and_set_pos(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, int blocknum, sion_int64 posinblock,
					      size_t *current_pos, size_t *bytes_left) {
  size_t rc=SION_SUCCESS;
  _sion_key_entry *entry;
  _sion_key_block_entry *block;
  sion_int64 absolutepos;
  void *p;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  *current_pos = 0;
  *bytes_left  = 0;

  /* check if key is known */
  p=_sion_keyvalue_table_lookup(keymngr->key_table,key);
  if( p==NULL ) {
    rc=SION_NOT_SUCCESS;
    DPRINTFP((2, DFUNCTION, -1, "leave key not found rc=%d\n",rc));
    return(rc);
  } 
  
  entry=(_sion_key_entry *) p;
  DPRINTFP((2, DFUNCTION, -1, "found entry with %d blocks\n",entry->blocks_avail));

  if((posinblock == SION_CURRENT_POS) && (blocknum==SION_CURRENT_BLOCK) ) {
    /* dummy call, leave position unchanged */
      *current_pos = entry->current_pos;
      *bytes_left  = entry->bytes_left;
      DPRINTFP((2, DFUNCTION, -1, "dummy call (SION_CURRENT_BLOCK,SION_CURRENT_POS), leave position unchanged rc=%d\n",rc));

      if(entry->current_pos != POS_BEHIND_END) {
	rc=SION_SUCCESS;
	DPRINTFP((2, DFUNCTION, -1, "leave, seek position is already current pos in block %d\n",blocknum));
      } else {
	rc=SION_NOT_SUCCESS;
	DPRINTFP((2, DFUNCTION, -1, "leave, currently behind last block rc=%d\n",rc));
      }
      return(rc);
  }
  
  if(blocknum == SION_CURRENT_BLOCK) {
    
    DPRINTFP((2, DFUNCTION, -1, "seek in current_block request\n"));
    
    if(entry->current_pos != POS_BEHIND_END) {
      
      /* search in current block */
      blocknum = entry->blocklist_current->blocknum; 
      DPRINTFP((2, DFUNCTION, -1, "seek position is in block %d\n",blocknum));

    } else {
     
      rc=SION_NOT_SUCCESS;
      DPRINTFP((2, DFUNCTION, -1, "leave, currently behind last block rc=%d\n",rc));
      return(rc);
    } 

  }

  if(blocknum != SION_ABSOLUTE_POS) {

    /* search for blocknum */

    if ( (blocknum<0) || (blocknum >= entry->blocks_avail)) {
      rc=SION_NOT_SUCCESS; /* wrong blocknum */
      DPRINTFP((2, DFUNCTION, -1, "leave, blocknum outside known range rc=%d\n",rc));
      return(rc);
    }
    
    /* determine start block for search: current block or head of list  */
    if (blocknum >= entry->blocklist_current->blocknum) {
      block=entry->blocklist_current;
    } else {
      block=entry->blocklist_head;
    }
        
    /* loop until block found */
    while ((block!=NULL) && (blocknum != block->blocknum)) {
      block=block->next;
    }      
    if(block==NULL) {
      DPRINTFP((2, DFUNCTION, -1, "internal error: somethink went wrong, seek, aborting, blocknum position block is NULL\n"));
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"internal error: somethink went wrong, seek, aborting ...\n"));
    }
    
    /* position is in block */
    DPRINTFP((2, DFUNCTION, -1, "position is in this block %d\n",block->blocknum));
    
    if((posinblock>=0) && (posinblock < block->len)) {
      entry->blocklist_current = block;
      entry->current_pos       = entry->blocklist_current->offset + posinblock;
      entry->bytes_left        = block->len - posinblock;
      DPRINTFP((2, DFUNCTION, -1, "set current pointer for entry: current_pos=%d bytes_left=%d blocknum=%d\n",(int) entry->current_pos,(int) entry->bytes_left, (int) block->blocknum));
    } else {
      DPRINTFP((2, DFUNCTION, -1, "wrong pos in block %d\n",(int) posinblock));
      rc=SION_NOT_SUCCESS; /* wrong pos in block */
    }
    
  } else {  /* (blocknum == SION_ABSOLUTE_POS) */

    absolutepos=posinblock;
    if ( (absolutepos < 0) 
	 || ( absolutepos >= (entry->blocklist_tail->offset_in_entry + entry->blocklist_tail->len) ) 
	 ){
      rc=SION_NOT_SUCCESS; /* wrong absolute position */
      DPRINTFP((2, DFUNCTION, -1, "leave, absolute position outside known range rc=%d\n",rc));
      return(rc);
    }

    /* determine start block for search: current block or head of list  */
    if (absolutepos >= entry->blocklist_current->offset_in_entry) {
      block=entry->blocklist_current;
    } else {
      block=entry->blocklist_head;
    }
    
    /* loop until block found */
    while ((block!=NULL) && (absolutepos >= (block->offset_in_entry + block->len))) {
      block=block->next;
    }      
    if(block==NULL) {
      DPRINTFP((2, DFUNCTION, -1, "internal error: somethink went wrong, seek, aborting, absolute position block is NULL\n"));
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"internal error: somethink went wrong, seek, aborting ...\n"));
    }
    /* recompute posinblock */
    posinblock = absolutepos - block->offset_in_entry;

    /* set current information */
    entry->blocklist_current = block;
    entry->current_pos       = entry->blocklist_current->offset + posinblock;
    entry->bytes_left        = block->len - posinblock;
    DPRINTFP((2, DFUNCTION, -1, "set current pointer for entry: current_pos=%d bytes_left=%d blocknum=%d\n",(int) entry->current_pos,(int) entry->bytes_left, (int) block->blocknum));

  } 

  *current_pos = entry->current_pos;
  *bytes_left  = entry->bytes_left;
  DPRINTFP((2, DFUNCTION, -1, "found block --> (%ld,%ld)\n",(long) *current_pos,(long) *bytes_left));
     
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
    
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_iterator_reset"
  int _sion_keyvalue_keymngr_iterator_reset(_sion_keyvalue_keymngr* keymngr) {

    int rc=SION_SUCCESS;

    DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  
    keymngr->iterator_last_block = NULL;

    DPRINTFP((2, DFUNCTION, -1, "leave\n"));
    return(rc);
  }
#undef DFUNCTION

  /* moves to next block in write order, sets also internal current_pos to beginning of block */
  /* this function could also be called after end of block list is reached, this is nedded for supporting iterator on in 
     inline mode where meta data is read on the fly during iteration */
#define DFUNCTION "_sion_keyvalue_keymngr_iterator_next"
  int _sion_keyvalue_keymngr_iterator_next(_sion_keyvalue_keymngr* keymngr, sion_table_key_t *key, size_t *current_pos, size_t *offset, size_t *len) {
    int rc=SION_SUCCESS;
    int found = 0;

    DPRINTFP((2, DFUNCTION, -1, "enter\n"));

    /* first time called after reset */
    if(keymngr->iterator_last_block==NULL) {
      /* next block is the head block */
      keymngr->iterator_last_block=keymngr->block_inwriteorder_head;
      if(keymngr->iterator_last_block!=NULL) {
	found=1;
      }
    } else {
      /* not already at end of list  */
      if(keymngr->iterator_last_block!=keymngr->block_inwriteorder_tail) {
	/* move forward in list */
	keymngr->iterator_last_block=keymngr->iterator_last_block->next_inwriteorder;
	if(keymngr->iterator_last_block==NULL) {
	  return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"internal error: somethink went wrong, checking end of list , aborting ...\n"));
	}    
	found=1;
      }
    }
 
    if(found) {
      
      /* set current_block and current_pos and bytes_left of entry to this block */
      keymngr->iterator_last_block->entry->current_pos        = keymngr->iterator_last_block->offset;
      keymngr->iterator_last_block->entry->bytes_left         = keymngr->iterator_last_block->len;
      keymngr->iterator_last_block->entry->blocklist_current  = keymngr->iterator_last_block;
      
      /* set output parameters */
      *key        = keymngr->iterator_last_block->entry->key;
      *offset     = keymngr->iterator_last_block->offset;
      *len        = keymngr->iterator_last_block->len;
      *current_pos= keymngr->iterator_last_block->offset;
      
      rc=SION_SUCCESS;
      
    } else {
      
      rc=SION_NOT_SUCCESS;
      
    }
    
    DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
    return(rc);
  }
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_key_list_iterator_reset"
  int _sion_keyvalue_keymngr_key_list_iterator_reset(_sion_keyvalue_keymngr* keymngr) {

    int rc=SION_SUCCESS;

    DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  
    _sion_keyvalue_table_iterator_reset(keymngr->key_table);

    DPRINTFP((2, DFUNCTION, -1, "leave\n"));
    return(rc);
  }
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_key_list_iterator_next"
  int _sion_keyvalue_keymngr_key_list_iterator_next(_sion_keyvalue_keymngr* keymngr, sion_table_key_t *key) {
    int rc=0;
    void *data;

    DPRINTFP((2, DFUNCTION, -1, "enter\n"));

    rc=_sion_keyvalue_table_iterator_next_in_store_order(keymngr->key_table, key, &data);
    
    DPRINTFP((2, DFUNCTION, -1, "leave rc=%d key=%ld\n",rc, (long) *key));
    return(rc);
  }
#undef DFUNCTION

  /* this function will create a duplicate of the keymngr structure containing the the description of all blocks
     of all keys, execpt that the current read posiotion for each key is a beginning of first block of each key */
#define DFUNCTION "_sion_keyvalue_keymngr_dup"
_sion_keyvalue_keymngr* _sion_keyvalue_keymngr_dup(_sion_keyvalue_keymngr* keymngr_orig, int dup_mode, sion_table_key_t sel_key) {
    _sion_keyvalue_keymngr *keymngr=NULL;
    _sion_key_block_entry  *block; 
    _sion_key_entry *entry;
    void *p;

    DPRINTFP((2, DFUNCTION, -1, "enter dup\n"));

    keymngr = malloc(sizeof(_sion_keyvalue_keymngr));
    if (keymngr == NULL) {
      _sion_errorprint(0,_SION_ERROR_RETURN,"cannot allocate internal keyvalue keymngr of size %lu , aborting ...\n", (unsigned long) sizeof(_sion_keyvalue_keymngr));
      return(NULL);
    }

    /* init main data structure */
    keymngr->lastentry_used= NULL;
    keymngr->next_scan_pos = keymngr_orig->next_scan_pos;
    keymngr->iterator_last_block = NULL;
    keymngr->block_inwriteorder_head = NULL;
    keymngr->block_inwriteorder_tail = NULL;

    /* create empty table of keys */
    keymngr->size = keymngr_orig->size;
    keymngr->key_table = _sion_keyvalue_table_init(keymngr_orig->size);
    if (keymngr->key_table == NULL) {
      _sion_errorprint(0,_SION_ERROR_RETURN,"cannot allocate internal keyvalue table of for %lu entries , aborting ...\n", (unsigned long) keymngr->size);
      free(keymngr);
      return(NULL);
    }


    if(dup_mode==SION_DESCSTATE_DUP_SEL_RANK_KEY) {
      DPRINTFP((2, DFUNCTION, -1, "start duplicating entry for key %llu\n",sel_key));

      p=_sion_keyvalue_table_lookup(keymngr_orig->key_table,sel_key);
      if( p!=NULL ) {
	entry = (_sion_key_entry *) p;
	block=entry->blocklist_head;
	while(block!=NULL) {
	  /* using own API routine to add block to new keymngr structure */
	  DPRINTFP((2, DFUNCTION, -1, "add block to keymngr key=%ld offset=%ld len=%ld\n",
		    (long)block->entry->key,(long)block->offset,(long)block->len));
	  _sion_keyvalue_keymngr_add_block(keymngr,block->entry->key,block->offset,block->len);
	  block=block->next;
	}
      }
      DPRINTFP((2, DFUNCTION, -1, "end duplicating one entry\n"));

    } else {
      /* loop over all blocks in orig keymngr */
      DPRINTFP((2, DFUNCTION, -1, "start duplicating all entries\n"));
      block=keymngr_orig->block_inwriteorder_head;
      while(block!=NULL) {
	/* using own API routine to add block to new keymngr structure */
	DPRINTFP((2, DFUNCTION, -1, "add block to keymngr key=%ld offset=%ld len=%ld\n",
		(long)block->entry->key,(long)block->offset,(long)block->len));
	_sion_keyvalue_keymngr_add_block(keymngr,block->entry->key,block->offset,block->len);
	block=block->next_inwriteorder;
      }
      DPRINTFP((2, DFUNCTION, -1, "end duplicating entries\n"));
    }

    DPRINTFP((2, DFUNCTION, -1, "leave\n"));
    return (keymngr);
  }
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_key_get_stat"
int _sion_keyvalue_keymngr_key_get_stat(_sion_keyvalue_keymngr* keymngr, sion_table_key_t key, sion_key_stat_t *keystat) {
  size_t rc=SION_SUCCESS;
  _sion_key_entry *entry;
  void *p;

  DPRINTFP((2, DFUNCTION, -1, "enter key=%ld\n",(long) key));

  /* check if key is known */
  p=_sion_keyvalue_table_lookup(keymngr->key_table,key);
  if( p==NULL ) {
    rc=SION_NOT_SUCCESS;
  } else {
    entry=(_sion_key_entry *) p;
    DPRINTFP((2, DFUNCTION, -1, "found entry with %d blocks\n",entry->blocks_avail));

    if(entry->blocklist_tail != NULL) {
      keystat->key        = (uint64_t) key;
      keystat->num_blocks = entry->blocklist_tail->blocknum+1;
      keystat->total_size = entry->blocklist_tail->offset_in_entry+entry->blocklist_tail->len;
      rc=SION_SUCCESS;
      DPRINTFP((2, DFUNCTION, -1, "found entry for key %ld (%ld,%ld)\n",(long) keystat->key, (long) keystat->num_blocks, (long) keystat->total_size ));
    } else {
      rc=SION_NOT_SUCCESS;
    }
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_keyvalue_keymngr_key_get_sizeof"
int _sion_keyvalue_keymngr_key_get_sizeof(_sion_keyvalue_keymngr* keymngr) {
  size_t bytes=0, help_bytes;
  _sion_key_block_entry *block;

  
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  help_bytes=sizeof(_sion_keyvalue_keymngr);
  DPRINTFP((2, DFUNCTION, -1, " sizeof key_keymgr=          %5d\n", help_bytes));
  bytes+=help_bytes;

  block = keymngr->block_inwriteorder_head;
  while (block != NULL) {
    help_bytes=sizeof(_sion_key_block_entry);
     block = block->next_inwriteorder;
  }
  DPRINTFP((2, DFUNCTION, -1, " sizeof key_blocks=          %5d\n", help_bytes));
  bytes+=help_bytes;
  
  if(keymngr->key_table) {
    help_bytes=_sion_keyvalue_table_get_size(keymngr->key_table);
    DPRINTFP((2, DFUNCTION, -1, " sizeof key_blocks=          %5d\n", help_bytes));
    bytes+=help_bytes;

  }
  DPRINTFP((2, DFUNCTION, -1, "leave bytes=%d\n",bytes));

  return(bytes);
}
#undef DFUNCTION
