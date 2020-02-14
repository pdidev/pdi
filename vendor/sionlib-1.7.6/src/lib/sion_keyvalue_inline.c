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
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef _SION_CUDA
#include <cuda_runtime.h>
#endif

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_filedesc.h"
#include "sion_tools.h"
#include "sion_fd.h"
#include "sion_file.h"
#include "sion_metadata.h"
#include "sion_internal.h"
#include "sion_internal_seek.h"
#include "sion_printts.h"
#include "sion_keyvalue.h"
#include "sion_keyvalue_inline.h"
#include "sion_keyvalue_keymngr.h"

#define TABLE_SIZE 127

#define SEARCH_TO_KEY 0
#define SEARCH_TO_NEXT 1
#define SEARCH_TO_END 2

/* Utility functions */
_sion_keyvalue_keymngr * _sion_get_or_init_key_info(_sion_filedesc *sion_filedesc);
int _sion_move_to_pos(_sion_filedesc *sion_filedesc, size_t pos);
int _sion_scan_forward_to_key(_sion_filedesc *sion_filedesc, uint64_t key, int search_mode);
int _sion_move_to_next_chunk(_sion_filedesc *sion_filedesc);
size_t _sion_write_data_to_chunks_inline(_sion_filedesc *sion_filedesc, const void *data, sion_int64 bytes_to_write);
sion_int64 _sion_compute_next_position_inline(_sion_filedesc *sion_filedesc, sion_int64 bytes_to_read);
size_t _sion_skip_data_from_chunks_inline(_sion_filedesc *sion_filedesc, sion_int64 bytes_to_read);
size_t _sion_read_data_from_chunks_inline(_sion_filedesc *sion_filedesc, void *data, sion_int64 bytes_to_read);


/* ***************** */
/* Write functions   */
/* ***************** */



#define DFUNCTION "_sion_store_and_write_key_and_len_inline"
size_t _sion_store_and_write_key_and_len_inline(_sion_filedesc *sion_filedesc, uint64_t key, size_t len) {
  sion_int64 bytes_to_write;  
  sion_int64 buffer[2];
  size_t rc=SION_SUCCESS, frc;


  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* TODO: need to check if current_pos is set correctly (in case of sion_seeks in between, currently not allowed) ... */

  /* assemble data */
  buffer[0]=(sion_int64) key;
  buffer[1]=(sion_int64) len;
  bytes_to_write=2*sizeof(sion_int64);

  frc=_sion_write_data_to_chunks_inline(sion_filedesc, (const void *) buffer, (sion_int64) bytes_to_write);

  /* check return code */
  if(frc != bytes_to_write) {
    return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
				    "could not write data (%d bytes) to file (frc=%d sid=%d) ...",  (int) bytes_to_write, (int) frc, sion_filedesc->sid));
  }
  
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_write_value_inline"
size_t _sion_write_value_inline(_sion_filedesc *sion_filedesc, const void *data, uint64_t key, size_t len) {
  size_t rc=0, frc;
  sion_int64 bytes_to_write;  
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));


  bytes_to_write=len;
  frc=_sion_write_data_to_chunks_inline(sion_filedesc, data, (sion_int64) bytes_to_write);
  if(frc != bytes_to_write) {
    return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
				"could not write data (%d bytes) to file (frc=%d sid=%d) ...",  (int) bytes_to_write, (int) frc, sion_filedesc->sid));
  }
  
  rc=(size_t) bytes_to_write;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION


/* ***************** */
/* Read functions    */
/* ***************** */

#define DFUNCTION "_sion_find_and_read_key_and_len_inline"
int _sion_find_and_read_key_and_len_inline(_sion_filedesc *sion_filedesc, uint64_t key, size_t len, size_t *datalen) {
  _sion_keyvalue_keymngr *keymngr;
  int        rc = 0;
  int        position_found=0;   
  sion_table_key_t mkey;
  size_t     key_current_pos = (size_t) -1, key_bytes_left = (size_t) -1;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);

  /* map key to table type */ 
  mkey = key;
  
  *datalen=0;  

  /* indicator if data could read, key_current_pos, key_bytes_left are set correctly */
  position_found=0;

  /* check if info about key is available */
  rc=_sion_keyvalue_keymngr_lookup(keymngr, mkey,  &key_current_pos, &key_bytes_left);
  if(rc!=SION_SUCCESS) {
    DPRINTFP((2, DFUNCTION, -1, "key %ld is not found in keymngr, scan forward\n", (long)mkey));

    /* scan forward until key is found */
    rc=_sion_scan_forward_to_key(sion_filedesc, key, SEARCH_TO_KEY);

    /* check again if info about key is now available */
    if(rc==SION_SUCCESS) {
      DPRINTFP((2, DFUNCTION, -1, "scan forward found key %ld, get info \n", (long)mkey));
      rc=_sion_keyvalue_keymngr_lookup(keymngr, mkey,  &key_current_pos, &key_bytes_left);
      DPRINTFP((2, DFUNCTION, -1, "key %ld, get info(2): pos=%ld bytes_left=%ld\n", (long)mkey, (long) key_current_pos, (long) key_bytes_left ));
    }
  } else {
    DPRINTFP((2, DFUNCTION, -1, "key %ld, get info(1): pos=%ld bytes_left=%ld\n", (long)mkey, (long) key_current_pos, (long) key_bytes_left ));
  }
  if(rc==SION_SUCCESS) {
    /* check if enough data is available in block  */
    position_found=1; 
    if(len<=key_bytes_left) {
      *datalen=len;
    } else {
      /* only a part of requested data can be read */
      *datalen=key_bytes_left;
    }
  } else {
    /* not more data in file for key */
    rc=SION_NOT_SUCCESS;
    DPRINTFP((2, DFUNCTION, -1, "no more data for key %ld\n", (long)mkey ));
  }
  
  /* check current position and move if necessary */
  DPRINTFP((2, DFUNCTION, -1, "check position: current_pos %ld, key_current_pos=%ld len=%ld key_bytes_left=%ld\n", 
	    (long) sion_filedesc->currentpos, (long) key_current_pos, (long) len, (long) key_bytes_left ));
  if(position_found) {
    if(key_current_pos!=sion_filedesc->currentpos) {
      rc=_sion_move_to_pos(sion_filedesc, key_current_pos);
    }
    /* ready to read data  */
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n", rc));
  return rc;
}
#undef DFUNCTION

#define DFUNCTION "_sion_read_value_inline"
size_t _sion_read_value_inline(_sion_filedesc *sion_filedesc, void *data, uint64_t key, size_t len) {
  size_t rc=0;
  _sion_keyvalue_keymngr *keymngr;
  sion_table_key_t mkey;
  sion_int64 bread;
  sion_int64 bytes_to_read;
  char *out;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);
  mkey = (sion_table_key_t) key;

  /* there are enough bytes available, checked during read of key,len  */

  out = (char*) data;

  /* read data */
  bytes_to_read=len;

  bread=_sion_read_data_from_chunks_inline(sion_filedesc, (void *) out, (sion_int64) bytes_to_read);
  if (bread != bytes_to_read) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,
				    "could not read data (%d bytes) from file ...",  (int) bytes_to_read));
  }

  DPRINTFP((2, DFUNCTION, -1, "first value = %3hhu | %c\n", out[0], out[0]));
  
  /* update meta data of current block  */
  _sion_keyvalue_keymngr_update_read_pos(keymngr, mkey, (size_t) bread, (sion_int64) sion_filedesc->currentpos);
  
  rc = (size_t) bread;

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",(int) rc));
  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_key_full_scan_inline"
int _sion_key_full_scan_inline(_sion_filedesc *sion_filedesc) {
  int rc=SION_NOT_SUCCESS;
  _sion_keyvalue_keymngr* keymngr;
  sion_table_key_t key=0;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);

  if(!_sion_keyvalue_keymngr_is_scan_done(keymngr)) {
    rc=_sion_scan_forward_to_key(sion_filedesc, key, SEARCH_TO_END);
  } else {
    /* scan already done */
    rc=SION_SUCCESS;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return rc;
}
#undef DFUNCTION


#define DFUNCTION "_sion_iterator_reset_inline"
int _sion_iterator_reset_inline(_sion_filedesc *sion_filedesc) {
  int rc=0;
  _sion_keyvalue_keymngr* keymngr;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);

  rc=_sion_keyvalue_keymngr_iterator_reset(keymngr);

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return rc;
}
#undef DFUNCTION

#define DFUNCTION "_sion_iterator_next_inline"
int _sion_iterator_next_inline(_sion_filedesc *sion_filedesc, uint64_t *keyptr, size_t *sizeptr) {
  int rc=SION_NOT_SUCCESS;
  _sion_keyvalue_keymngr* keymngr;
  sion_table_key_t key=0;
  size_t current_pos, offset, len;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);
  
  if(_sion_keyvalue_keymngr_iterator_next(keymngr, &key, &current_pos, &offset, &len)==SION_SUCCESS) {
    /* next block found */
    rc=SION_SUCCESS;
  } else {
    /* scan forward if not at end of file to get more meta data */
    if (_sion_scan_forward_to_key(sion_filedesc, key, SEARCH_TO_NEXT)==SION_SUCCESS) {
      /* get info about block */
      if(_sion_keyvalue_keymngr_iterator_next(keymngr, &key, &current_pos, &offset, &len)==SION_SUCCESS) {
	rc=SION_SUCCESS;
      } else {
	return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN, sion_filedesc->rank,
					"internal error: block could not be find at end of block list ..."));
      }
    } else {
      /* end of file reached and all blocks are already iterated  */
      rc=SION_NOT_SUCCESS;
    }
  }

  /* set output parameter */
  if(rc==SION_SUCCESS) {
    *keyptr  = (uint64_t) key;
    *sizeptr = (size_t) len;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return rc;
}
#undef DFUNCTION

#define DFUNCTION "_sion_seek_key_inline"
int _sion_seek_key_inline(_sion_filedesc *sion_filedesc, uint64_t key, int blocknum, sion_int64 posinblock) {
  int rc=SION_NOT_SUCCESS;
  _sion_keyvalue_keymngr* keymngr;
  size_t offset, len;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);
  
  /* lookup if a key entry at that position is already in keymngr  */
  rc=_sion_keyvalue_keymngr_lookup_and_set_pos(keymngr, key, blocknum, posinblock, &offset, &len);
  DPRINTFP((2, DFUNCTION, -1, "after first lookup rc=%d offset=%d len=%d\n",rc, (int)offset, (int) len ));

  while( (rc!=SION_SUCCESS)  && !_sion_keyvalue_keymngr_is_scan_done(keymngr) ) {

    /* scan forward to next entry with that key */
    DPRINTFP((2, DFUNCTION, -1, "scan forward to find key %d\n",(int) key));
    if (_sion_scan_forward_to_key(sion_filedesc, key, SEARCH_TO_NEXT)==SION_SUCCESS) {
      
      /* lookup again if a key entry at that position is already in keymngr  */
      rc=_sion_keyvalue_keymngr_lookup_and_set_pos(keymngr, key, blocknum, posinblock, &offset, &len);
      DPRINTFP((2, DFUNCTION, -1, "in loop lookup rc=%d offset=%d len=%d\n",rc, (int)offset, (int) len ));
    } else {
      rc=SION_NOT_SUCCESS;
    }
  }

  /* move to scanpos if necessary */
  if( rc==SION_SUCCESS ) {
    DPRINTFP((2, DFUNCTION, -1, "move from %ld to new scanpos (%ld) \n",(long) sion_filedesc->currentpos,(long) offset));
    rc=_sion_move_to_pos(sion_filedesc, offset);
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return rc;
}
#undef DFUNCTION

#define DFUNCTION "_sion_key_list_iterator_reset_inline"
int _sion_key_list_iterator_reset_inline(_sion_filedesc *sion_filedesc) {
  int rc=0;
  _sion_keyvalue_keymngr* keymngr;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);

  rc=_sion_keyvalue_keymngr_key_list_iterator_reset(keymngr);

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return rc;
}
#undef DFUNCTION

#define DFUNCTION "_sion_iterator_next_inline"
int _sion_key_list_iterator_next_inline(_sion_filedesc *sion_filedesc, uint64_t *keyptr) {
  int rc=SION_NOT_SUCCESS;
  _sion_keyvalue_keymngr* keymngr;
  sion_table_key_t key=0;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);
  
  if(_sion_keyvalue_keymngr_key_list_iterator_next(keymngr, &key)==SION_SUCCESS) {
    *keyptr  = (uint64_t) key;
    rc=SION_SUCCESS;
  } else {
    *keyptr  = 0;
    rc=SION_NOT_SUCCESS;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d key=%ld\n",rc,(long) key));
  return rc;
}
#undef DFUNCTION

/* fills a stat-data structure containing information about the key */
#define DFUNCTION "_sion_key_get_stat_inline"
int _sion_key_get_stat_inline(_sion_filedesc *sion_filedesc, uint64_t searchkey, sion_key_stat_t *keystat) {
  int rc=SION_NOT_SUCCESS;
  _sion_keyvalue_keymngr* keymngr;
  sion_table_key_t key=0;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* get or init internal data structure if first call */
  keymngr=_sion_get_or_init_key_info(sion_filedesc);
  
  key= (sion_table_key_t) searchkey;

  if(_sion_keyvalue_keymngr_key_get_stat(keymngr, key, keystat)==SION_SUCCESS) {
    rc=SION_SUCCESS;
  } else {
    rc=SION_NOT_SUCCESS;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d key=%ld\n",rc,(long) key));
  return rc;
}
#undef DFUNCTION

/* ***************** */
/* Utility functions */
/* ***************** */

#define DFUNCTION "_sion_scan_forward_to_key"
int _sion_scan_forward_to_key(_sion_filedesc *sion_filedesc, uint64_t key, int search_mode) {
  int        rc = SION_SUCCESS;
  _sion_keyvalue_keymngr *keymngr;
  sion_table_key_t mkey;

  size_t     scanpos;
  int        key_found;   
  size_t     bytes_left, lastposinblock, lastposinfile;
  uint64_t   key_and_len[2];

  size_t     bread, bskip, len;

  DPRINTFP((2, DFUNCTION, -1, "enter key=%ld\n",(long) key));

  /* get keymngr */
  keymngr = (_sion_keyvalue_keymngr *) sion_filedesc->keyvalptr;

  key_found=0;

  /* get position from where scan has to be started */
  if(_sion_keyvalue_keymngr_get_next_scan_pos(keymngr, &scanpos) != SION_SUCCESS) {
    /* can only be at start time, start then from current position in file */
    scanpos=(size_t) sion_filedesc->currentpos;
    DPRINTFP((2, DFUNCTION, -1, "set scanpos to first pos in file pos=%ld\n",(long) scanpos));
  }
  /* check if file is already completely scanned */
  lastposinfile=sion_filedesc->startpos + sion_filedesc->lastchunknr * sion_filedesc->globalskip 
                                             + sion_filedesc->blocksizes[sion_filedesc->lastchunknr];

  DPRINTFP((2, DFUNCTION, -1, "check if not already scanned to endoffile scanpos=%ld lastposinfile=%ld lastchunknr=%d\n",(long) scanpos,(long) lastposinfile, (int) sion_filedesc->lastchunknr ));
  if(scanpos>=lastposinfile) {
    DPRINTFP((2, DFUNCTION, -1, "scan is already at end of file (%ld>=%ld) \n",(long) scanpos, (long) lastposinfile));
    _sion_keyvalue_keymngr_set_scan_done(keymngr);
    rc=SION_NOT_SUCCESS;
  }

  /* move to scanpos if necessary */
  if( (sion_filedesc->currentpos != scanpos) && (!_sion_keyvalue_keymngr_is_scan_done(keymngr)) ) {
    DPRINTFP((2, DFUNCTION, -1, "move from %ld to new scanpos (%ld) \n",(long) sion_filedesc->currentpos,(long) scanpos));
    rc=_sion_move_to_pos(sion_filedesc, scanpos);
  }

  /* search forward */
  while ( (!_sion_keyvalue_keymngr_is_scan_done(keymngr)) && (!key_found) ) {

    lastposinblock = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip 
                                             + sion_filedesc->blocksizes[sion_filedesc->currentblocknr];
    bytes_left     = lastposinblock - sion_filedesc->currentpos;
    DPRINTFP((2, DFUNCTION, -1, "search loop: lastposinblock=%ld bytes_left=%ld currentpos=%ld\n",(long) lastposinblock,(long) bytes_left,(long) sion_filedesc->currentpos));
  

    /* is there data in current chunk to start reading next block? */
    if ( bytes_left > 0 ) {
      /* read next block from current and following chunks */
      bread=_sion_read_data_from_chunks_inline(sion_filedesc, (void *) key_and_len, (sion_int64) sizeof(key_and_len));
      if (bread != sizeof(key_and_len)) {
	return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,
					"could not read data (%d bytes) from file ...",  (int) sizeof(key_and_len)));
      }
      sion_swap(key_and_len, key_and_len, sizeof(sion_int64), 2, sion_filedesc->swapbytes);


      DPRINTFP((2, DFUNCTION, -1, "search loop: found next key,len (%ld,%ld)\n",(long) key_and_len[0],(long) key_and_len[1]));
      bytes_left-=bread;

      DPRINTFP((2, DFUNCTION, -1, "search loop: position is now %ld bytes_left=%ld\n",(long) sion_filedesc->currentpos, (long) bytes_left));

      /* check if filepointer has to moved to next block to be in front of data */
      if(bytes_left==0) {
	DPRINTFP((2, DFUNCTION, -1, "search loop: data starts  in next chunk, moving forward\n"));
	if(! _sion_move_to_next_chunk(sion_filedesc)) {
	  return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,
					  "could not move to data section in next block ..."));
	}
	lastposinblock = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip 
	  + sion_filedesc->blocksizes[sion_filedesc->currentblocknr];
	bytes_left     = lastposinblock - sion_filedesc->currentpos;
	DPRINTFP((2, DFUNCTION, -1, "search loop: position is moved to %ld bytes_left=%ld\n",(long) sion_filedesc->currentpos, (long) bytes_left));
      }

      /* store new block info */
      mkey=key_and_len[0];
      len =key_and_len[1];
      rc=_sion_keyvalue_keymngr_add_block(keymngr, mkey, (size_t) sion_filedesc->currentpos, len);
      DPRINTFP((2, DFUNCTION, -1, "search loop: stored new block key=%ld pos=%ld len=%ld\n",(long) mkey, (long) sion_filedesc->currentpos, (long) len));

      /* check key */
      if ( 
	  ( ( (search_mode==SEARCH_TO_KEY) && (key_and_len[0] == key) ) || (search_mode==SEARCH_TO_NEXT) ) 
	  && ( !(search_mode==SEARCH_TO_END) ) 
	   )  {
	DPRINTFP((2, DFUNCTION, -1, "search loop: found searched key=%ld, leaving\n",(long) key));
	rc=SION_SUCCESS;
	key_found=1; 		/* end of loop */
	scanpos=_sion_compute_next_position_inline(sion_filedesc, len); /* next search behind data of that block */
      } else {
	DPRINTFP((2, DFUNCTION, -1, "search loop: found another key=%ld <=> %ld, continue\n",(long) key_and_len[0], (long) key));

	/* move forward behind data of block in the same or next chunk */
	DPRINTFP((2, DFUNCTION, -1, "search loop: found move position forward by %ld bytes\n",(long) len));
	bskip=_sion_skip_data_from_chunks_inline(sion_filedesc, (sion_int64) len);

	if ( bskip == len  ) {
	  
	  /* recompute bytes_left in current block */
	  lastposinblock = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip 
	    + sion_filedesc->blocksizes[sion_filedesc->currentblocknr];
	  bytes_left     = lastposinblock - sion_filedesc->currentpos;
	  scanpos=sion_filedesc->currentpos; /* next search behind data */

	} else {
	  return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,
					  "could not skip data section of one block (%d bytes) from file ...",  (int) len));
	}
	
	/* check if filepointer has to moved to next block */
	if(bytes_left==0) {
	  DPRINTFP((2, DFUNCTION, -1, "search loop: chunk empty, move to next chunk\n"));
	  if(! _sion_move_to_next_chunk(sion_filedesc)) {
	    _sion_keyvalue_keymngr_set_scan_done(keymngr); 		/* end of loop */
	    if(search_mode!=SEARCH_TO_END) {
	      rc=SION_NOT_SUCCESS;
	    } else {
	      rc=SION_SUCCESS; 	/* end of data reached */
	    }
	  }
	}
      }
    
    } else {
      /* not enough byte in block to read next key+len */
      if(bytes_left==0) {
	if(! _sion_move_to_next_chunk(sion_filedesc)) {
	  _sion_keyvalue_keymngr_set_scan_done(keymngr); 		/* end of loop */
	  rc=SION_NOT_SUCCESS;
	}
      } else {
	  return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,
					  "unknown data at end of chunk (%d bytes)  ...",  (int) bytes_left));
      }
    }
      
  } /* while */
  
  /* update scanpos */
  if(_sion_keyvalue_keymngr_set_next_scan_pos(keymngr, scanpos) != SION_SUCCESS) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,
				    "internal error set seekpos  ..."));
  }
  
  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_get_or_init_key_info"
_sion_keyvalue_keymngr * _sion_get_or_init_key_info(_sion_filedesc *sion_filedesc) {

  _sion_keyvalue_keymngr* keymngr;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if (sion_filedesc->keyvalptr == NULL) {
    DPRINTFP((2, DFUNCTION, -1, "sion_filedesc->keyvalptr == NULL\n"));
    keymngr =_sion_keyvalue_keymngr_init(TABLE_SIZE);
    if (keymngr == NULL) {
      _sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,"could not allocate keymngr ...");
    }
    sion_filedesc->keyvalptr=keymngr;
    DPRINTFP((2, DFUNCTION, -1, "alloc now KEYVALPTR = %x\n",sion_filedesc->keyvalptr));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return sion_filedesc->keyvalptr;
}
#undef DFUNCTION

#define DFUNCTION "_sion_move_to_next_chunk"
int _sion_move_to_next_chunk(_sion_filedesc *sion_filedesc) {
  int rc = SION_SUCCESS;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* is another chink available */
  if (sion_filedesc->currentblocknr < sion_filedesc->lastchunknr) {
    /* move to next chunk */
    sion_filedesc->currentblocknr++;
    sion_filedesc->currentpos = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip;
    _sion_file_purge(sion_filedesc->fileptr);
    _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);
  } else {
    rc=SION_NOT_SUCCESS;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave, rc = %d\n", rc));
  return rc;
}
#undef DFUNCTION

#define DFUNCTION "_sion_move_to_pos"
int _sion_move_to_pos(_sion_filedesc *sion_filedesc, size_t pos) {
  int rc = SION_SUCCESS;
  size_t block_min_pos, block_max_pos;
  int c, pos_found=0;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  
  block_min_pos  = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip;
  block_max_pos  = block_min_pos+sion_filedesc->blocksizes[sion_filedesc->currentblocknr]; /*  position of first byte behind chunk */

  DPRINTFP((2, DFUNCTION, -1, "current_chunk: %ld to %ld (pos=%ld)\n", (long) block_min_pos, (long) block_max_pos, (long) pos));

  if ( (pos>=block_min_pos) && (pos<block_max_pos) ) {
    /* stay in the same chunk */
    pos_found=1;
    sion_filedesc->currentpos=pos;
    _sion_file_purge(sion_filedesc->fileptr);
    _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);
    DPRINTFP((2, DFUNCTION, -1, "stay in current chunk: currentpos=%ld\n", (long) sion_filedesc->currentpos));

  } else {
    /* scan from beginning over all chunks */

    for(c=0;c<=sion_filedesc->lastchunknr;c++) {
      block_min_pos  = sion_filedesc->startpos + c * sion_filedesc->globalskip;
      block_max_pos  = block_min_pos+sion_filedesc->blocksizes[c]; /*  position of first byte behind chunk */

      DPRINTFP((2, DFUNCTION, -1, "check chunk%2d: %ld to %ld (pos=%ld)\n", c, (long) block_min_pos, (long) block_max_pos, (long) pos));
      
      if ( (pos>=block_min_pos) && (pos<block_max_pos) ) {

	/* stay in this chunk */
	pos_found=1;
	sion_filedesc->currentblocknr=c;
	sion_filedesc->currentpos=pos;
	_sion_file_purge(sion_filedesc->fileptr);
	_sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);
	DPRINTFP((2, DFUNCTION, -1, "stay in this chunk: currentpos=%ld\n", (long) sion_filedesc->currentpos));
	break;

      }
    } 
  }

  if(!pos_found) {
    rc=SION_NOT_SUCCESS;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave, rc = %d\n", rc));
  return rc;
}
#undef DFUNCTION

/* write data block to current and following chunks, data could be distributed over several chunks */
#define DFUNCTION "_sion_write_data_to_chunks_inline"
size_t _sion_write_data_to_chunks_inline(_sion_filedesc *sion_filedesc, const void *data, sion_int64 bytes_to_write) {
  sion_int64 btowr, byteswritten, offset;  
  size_t rc=0, frc;


  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* loop to write data in current and following chunks */
  offset=0;
  while(bytes_to_write>0) {

    _sion_flush_block(sion_filedesc);

    /* determine size of data which could written into current chunk  */
    byteswritten = sion_filedesc->blocksizes[sion_filedesc->currentblocknr];
    btowr=bytes_to_write;
    if ((byteswritten + btowr) > sion_filedesc->chunksize) btowr = sion_filedesc->chunksize - byteswritten;
    DPRINTFP((2, DFUNCTION, -1, " bytes_to_write=%ld btowr=%ld chunksize=%ld byteswritten=%ld currentblocknr=%d blsize[%d]=%ld\n",
	      (long) bytes_to_write, (long) btowr, (long) sion_filedesc->chunksize, (long) byteswritten, (int) sion_filedesc->currentblocknr, 
	      sion_filedesc->currentblocknr, (long) sion_filedesc->blocksizes[sion_filedesc->currentblocknr] ));
    
    /* write part or rest of data */
#ifdef _SION_CUDA
    struct cudaPointerAttributes attrs;
    cudaError_t err = cudaPointerGetAttributes(&attrs, data);
    if ((err == cudaSuccess) && (!attrs.isManaged && (attrs.memoryType == cudaMemoryTypeDevice)) ) {
      char* buffer = malloc(sion_filedesc->fsblksize);
      const char* data_ = (char *)data + offset;
      while (frc < btowr) {
        sion_int64 to_write = (btowr - frc) > sion_filedesc->fsblksize ? sion_filedesc->fsblksize : (btowr - frc);
        cudaMemcpy(buffer, data_, to_write, cudaMemcpyDeviceToHost);
        sion_int64 frc_ = _sion_file_write(buffer, to_write, sion_filedesc->fileptr);
        if (frc_ != to_write) break;
        frc += frc_;
        data_ += frc_;
      }
      free(buffer);
    } else {
      frc = _sion_file_write((char *)data + offset, btowr, sion_filedesc->fileptr);
    }
#else
    frc = _sion_file_write(((char *) data+offset), btowr, sion_filedesc->fileptr);
#endif

    /* check return code */
    if(frc != btowr) {
      return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
				      "could not write data (%d bytes) to file (frc=%d sid=%d) ...",  (int) btowr, (int) frc, sion_filedesc->sid));
    }

    /* increase current position and update other counters */
    sion_filedesc->currentpos+=btowr;
    bytes_to_write           -=btowr;
    offset                   +=btowr;

    /* check if all data has already be processed */
    if(bytes_to_write>0) {
      
      /* create new block for writing next data */
      _sion_flush_block(sion_filedesc);
      _sion_create_new_block(sion_filedesc);
    }
  }
  rc=offset;
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return(rc);
}
#undef DFUNCTION

/* skip data block from current and following chunks, data could be distributed over several chunks */
#define DFUNCTION "_sion_compute_next_position_inline"
sion_int64 _sion_compute_next_position_inline(_sion_filedesc *sion_filedesc, sion_int64 bytes_to_read) {
  sion_int64 btord = 0, bytesread = 0, offset = 0;
  int blocknr = 0;


  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* check data in current block */
  blocknr = sion_filedesc->currentblocknr;
  bytesread = sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);

  DPRINTFP((2, DFUNCTION, -1, " currentpos=%ld bytesread=%ld blocknr=%d bytes_to_read=%ld\n",
	    (long) sion_filedesc->currentpos, (long) bytesread, (int) blocknr, (long) bytes_to_read ));

  DPRINTFP((2, DFUNCTION, -1, " blocksizes[%d]=%ld\n",
	    (int) sion_filedesc->currentblocknr, (long) sion_filedesc->blocksizes[sion_filedesc->currentblocknr] ));
  
  /* data is all in current block */
  if ((bytesread + bytes_to_read) <= sion_filedesc->blocksizes[sion_filedesc->currentblocknr]) {
    offset=sion_filedesc->currentpos+bytes_to_read;
  } else {
    btord = sion_filedesc->blocksizes[sion_filedesc->currentblocknr] - bytesread; /* size of data in current chunk */
    bytes_to_read-=btord; 	/* rest of data in next chunks */
    
    DPRINTFP((2, DFUNCTION, -1, " skip to next block, bytes_to_read=%ld btord=%ld\n",(long) bytes_to_read, (long) btord ));
    
    /* loop to behind data in current and following chunks */
    while(bytes_to_read>0) {
      
      /* next block */
      if (blocknr < sion_filedesc->lastchunknr) { blocknr++; } else {
	return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,"internal error next block not available, but should  ..."));
      }
      
      DPRINTFP((2, DFUNCTION, -1, " blocksizes[%d]=%ld\n", (int) blocknr, (long) sion_filedesc->blocksizes[blocknr] ));
      if ( bytes_to_read > sion_filedesc->blocksizes[blocknr])  {
	btord = sion_filedesc->blocksizes[blocknr]; 
	bytes_to_read-=btord; 	/* rest of data in next chunks */
	DPRINTFP((2, DFUNCTION, -1, "whole block, bytes_to_read=%ld btord=%ld\n",(long) bytes_to_read, (long) btord ));
      } else {
	offset=sion_filedesc->startpos + blocknr * sion_filedesc->globalskip + bytes_to_read;
	bytes_to_read=0;
      }
    }
  }
  
  /* check if behind end of actual block */
  if(offset == (sion_filedesc->startpos + blocknr * sion_filedesc->globalskip + sion_filedesc->blocksizes[blocknr])) {
      /* move next block if available, if last blovk of file, stay behind current block */
      if (blocknr < sion_filedesc->lastchunknr) { 
	blocknr++; 
	offset=sion_filedesc->startpos + blocknr * sion_filedesc->globalskip;
	DPRINTFP((2, DFUNCTION, -1, " behind end of block, move to next block, blocksizes[%d]=%ld\n", (int) blocknr, (long) sion_filedesc->blocksizes[blocknr] ));
      } 
  }
  

  DPRINTFP((2, DFUNCTION, -1, "leave offset=%ld\n",(long) offset));
  return(offset);
}
#undef DFUNCTION

/* skip data block from current and following chunks, data could be distributed over several chunks */
#define DFUNCTION "_sion_skip_data_from_chunks_inline"
size_t _sion_skip_data_from_chunks_inline(_sion_filedesc *sion_filedesc, sion_int64 bytes_to_read) {
  sion_int64 btord, bytesread, offset;  
  size_t rc=0;


  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* loop to read data in current and following chunks */
  offset=0;
  while(bytes_to_read>0) {

    /* determine size of data which could read from current chunk  */
    bytesread = sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);

    btord=bytes_to_read;
    if ((bytesread + btord) > sion_filedesc->blocksizes[sion_filedesc->currentblocknr]) 
      btord = sion_filedesc->blocksizes[sion_filedesc->currentblocknr] - bytesread;
    DPRINTFP((2, DFUNCTION, -1, " bytes_to_read=%ld btord=%ld\n",(long) bytes_to_read, (long) btord ));
    
    /* file pointer will not here adjusted, only at end of loop */

    /* increase current position and update other counters */
    sion_filedesc->currentpos+=btord;
    bytes_to_read            -=btord;
    offset                   +=btord;

    /* check if all data has already be processed, otherwise forward to next chunk */
    if(bytes_to_read>0) {
      
      if(! _sion_move_to_next_chunk(sion_filedesc)) {
	return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
					"could not read data (%d bytes) to file (end of file reached sid=%d) ...",  (int) btord, sion_filedesc->sid));
      }

    }
  }

  /* move to new position */
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);

  rc=offset;
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return(rc);
}
#undef DFUNCTION

/* read data block from current and following chunks, data could be distributed over several chunks */
#define DFUNCTION "_sion_read_data_from_chunks_inline"
size_t _sion_read_data_from_chunks_inline(_sion_filedesc *sion_filedesc, void *data, sion_int64 bytes_to_read) {
  sion_int64 btord, bytesread, offset;  
  size_t rc=0, frc;


  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  /* loop to read data in current and following chunks */
  offset=0;
  while(bytes_to_read>0) {

    /* determine size of data which could read from current chunk  */
    bytesread = sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);
    DPRINTFP((2, DFUNCTION, -1, "bytesread=%ld, curpos=%ld,startpos=%ld,curblock=%ld,gskip=%ld\n",
	      (long) bytesread, (long) sion_filedesc->currentpos, (long) sion_filedesc->startpos, 
	      (long) sion_filedesc->currentblocknr, (long) sion_filedesc->globalskip ));

    btord=bytes_to_read;
    if ((bytesread + btord) > sion_filedesc->blocksizes[sion_filedesc->currentblocknr]) { 
      btord = sion_filedesc->blocksizes[sion_filedesc->currentblocknr] - bytesread;
      DPRINTFP((2, DFUNCTION, -1, "block is split to multiple chunks, bytes_to_read=%ld btord=%ld\n",(long) bytes_to_read, (long) btord ));
    } else {
      DPRINTFP((2, DFUNCTION, -1, "block in one chunk, bytes_to_read=%ld btord=%ld\n",(long) bytes_to_read, (long) btord ));
    }
    
    /* read part or rest of data */
#ifdef _SION_CUDA
    struct cudaPointerAttributes attrs;
    cudaError_t err = cudaPointerGetAttributes(&attrs, data);
    if ((err == cudaSuccess) && (!attrs.isManaged && (attrs.memoryType == cudaMemoryTypeDevice)) ) {
      char* buffer = malloc(sion_filedesc->fsblksize);
      char* data_ = (char *)data + offset;
      while (frc < btord) {
        sion_int64 to_read = (btord - frc) > sion_filedesc->fsblksize ? sion_filedesc->fsblksize : (btord - frc);
        sion_int64 frc_ = _sion_file_read(buffer, to_read, sion_filedesc->fileptr);
        if (frc_ != to_read) break;
        cudaMemcpy(data_, buffer, frc_, cudaMemcpyHostToDevice);
        frc += frc_;
        data_ += frc_;
      }
      free(buffer);
    } else {
      frc = _sion_file_read(((char *)data + offset), btord, sion_filedesc->fileptr);
    }
#else
    frc = _sion_file_read(((char *)data+offset), btord, sion_filedesc->fileptr);
#endif

    /* check return code */
    if(frc != btord) {
      return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
				      "could not read data (%d bytes) to file (frc=%d sid=%d) ...",  (int) btord, (int) frc, sion_filedesc->sid));
    }

    /* increase current position and update other counters */
    sion_filedesc->currentpos+=btord;
    bytes_to_read            -=btord;
    offset                   +=btord;

    /* check if all data has already be processed, otherwise forward to next chunk */
    if(bytes_to_read>0) {
      
      if(! _sion_move_to_next_chunk(sion_filedesc)) {
	return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
					"could not read data (%d bytes) to file (end of file reached, frc=%d sid=%d) ...",  (int) btord, (int) frc, sion_filedesc->sid));
      }

    }
  }
  
  rc=offset;
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  return(rc);
}
#undef DFUNCTION

/* dup internal data structures from first to secon sion_filedesc */
#define DFUNCTION "_sion_keyval_dup_dataptr_inline"
int _sion_keyval_dup_dataptr_inline(_sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc) {
  int rc=0;
  _sion_keyvalue_keymngr* keymngr_orig;
  _sion_keyvalue_keymngr* keymngr;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  
  if(sion_filedesc->keyvalptr!=NULL) {

    /* get orig keymngr */
    keymngr_orig = (_sion_keyvalue_keymngr *) sion_filedesc->keyvalptr;
    
    keymngr =_sion_keyvalue_keymngr_dup(keymngr_orig,new_filedesc->dup_mode, new_filedesc->dup_sel_key);
    if (keymngr == NULL) {
      return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,"dup: could not duplicate keymngr ..."));
    }
    new_filedesc->keyvalptr=keymngr;

  } else {

    new_filedesc->keyvalptr=NULL;

  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION

