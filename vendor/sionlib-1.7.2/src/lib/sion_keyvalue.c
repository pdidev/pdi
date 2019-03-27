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

#define DFUNCTION "_sion_keyval_check_env"
int  _sion_keyval_check_env(_sion_filedesc *sion_filedesc, sion_int64 file_mode_flags) {
  const char *t;
  int rc=1;
  sion_int32 keyvalmode_env;
  sion_int32 keyvalmode_parm;

  DPRINTFP((2, DFUNCTION, -1, "keyval mode is initially set to %s\n", sion_keyval_type_to_str((int) sion_filedesc->keyvalmode)));

  /* check env-var */
  keyvalmode_env=SION_KEYVAL_NOTSET;
  t = _sion_getenv("SION_KEYVALUE_MODE");
  if(t) {
    if (strstr(t, "inline"))     keyvalmode_env=SION_KEYVAL_INLINE;
    if (strstr(t, "meta"))       keyvalmode_env=SION_KEYVAL_META;
    if (strstr(t, "hash"))       keyvalmode_env=SION_KEYVAL_HASH;
    if (strstr(t, "none"))       keyvalmode_env=SION_KEYVAL_NONE;
    if (strstr(t, "unknown"))    keyvalmode_env=SION_KEYVAL_UNKNOWN;
  }
  DPRINTFP((2, DFUNCTION, -1, "keyval mode from env is=%s\n", sion_keyval_type_to_str((int) keyvalmode_env)));

  /* check file mode from open call */
  keyvalmode_parm=SION_KEYVAL_NOTSET;
  if(file_mode_flags&_SION_FMODE_KEYVAL_INLINE)  keyvalmode_parm=SION_KEYVAL_INLINE;
  if(file_mode_flags&_SION_FMODE_KEYVAL_META)    keyvalmode_parm=SION_KEYVAL_META;
  if(file_mode_flags&_SION_FMODE_KEYVAL_HASH)    keyvalmode_parm=SION_KEYVAL_HASH;
  if(file_mode_flags&_SION_FMODE_KEYVAL_NONE)    keyvalmode_parm=SION_KEYVAL_NONE;
  if(file_mode_flags&_SION_FMODE_KEYVAL_UNKNOWN) keyvalmode_parm=SION_KEYVAL_UNKNOWN;
  DPRINTFP((2, DFUNCTION, -1, "keyval mode from parameter is=%s\n", sion_keyval_type_to_str((int) keyvalmode_parm)));
  
  if(file_mode_flags&_SION_FMODE_READ) {
    DPRINTFP((2, DFUNCTION, -1, "file is opened for reading\n"));
    /* keyvalmode is already read from meta-data of file */
    /* open parameter and environment have to be identical */

    if(keyvalmode_parm==SION_KEYVAL_UNKNOWN) {
      /* user will query mode later */
      DPRINTFP((2, DFUNCTION, -1, "file read: parameter is 'unknown' keyvalmode will taken from file ...\n"));
      
    } else {
      /* check for consistency */
      if((keyvalmode_parm!=SION_KEYVAL_NOTSET) && (keyvalmode_parm!=sion_filedesc->keyvalmode)) {
	DPRINTFP((2, DFUNCTION, -1, "file read: keyvalmode of file is different to requested keyvalmode from parameter, aborting ...\n"));
	return(_sion_errorprint(0,_SION_ERROR_RETURN,"file read: keyvalmode of file is different to requested keyvalmode from parameter, aborting ...\n"));
      }
      if((keyvalmode_env!=SION_KEYVAL_NOTSET) && (keyvalmode_env!=sion_filedesc->keyvalmode)) {
	DPRINTFP((2, DFUNCTION, -1, "file read: keyvalmode of file is different to requested keyvalmode from environment, aborting ...\n"));
	return(_sion_errorprint(0,_SION_ERROR_RETURN,"file read: keyvalmode of file is different to requested keyvalmode from env-var, aborting ...\n"));
      }
    }

    } else {

    DPRINTFP((2, DFUNCTION, -1, "file is opened for writing\n"));
    /* order: first env, then parameter */
    sion_filedesc->keyvalmode=SION_KEYVAL_NONE;
    if(keyvalmode_env!=SION_KEYVAL_NOTSET) sion_filedesc->keyvalmode=keyvalmode_env;
    if(keyvalmode_parm!=SION_KEYVAL_NOTSET) sion_filedesc->keyvalmode=keyvalmode_parm;
  }


  DPRINTFP((2, DFUNCTION, -1, "keyval mode is=%s\n", sion_keyval_type_to_str((int) sion_filedesc->keyvalmode)));
  return (rc);
}
#undef DFUNCTION

/* dup internal data structures from first to second sion_filedesc */
#define DFUNCTION "_sion_keyval_dup_dataptr"
int _sion_keyval_dup_dataptr(_sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc) {
  int rc=0;
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) { 
    rc=_sion_keyval_dup_dataptr_inline(sion_filedesc,new_filedesc);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to call sion_keyval_dup_dataptr in inknown keyval mode , aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION



#define DFUNCTION "sion_fwrite_key"
/*!
 * \ingroup keyval_api
 * @brief Writes data for key.
 *
 * Writes data to sion file associating it to key. This function uses the
 * diffenent implementations of which one is chosen with the open call.
 *
 * @param[in]  data    data to be written
 * @param[in]  key     key associated to the data
 * @param[in]  size    size of single item
 * @param[in]  nitems  number of items
 * @param[in]  sid     sion file handle
 *
 * @return  number of successfully written items
 */
size_t sion_fwrite_key(const void *data, uint64_t key, size_t size, size_t nitems, int sid) {
  size_t rc=0, frc;
  size_t len;
  _sion_filedesc *sion_filedesc;

  /*                                                                      */ DPRINTFTS(-1, "enter sion_fwrite_key");
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));
  
  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 
  DPRINTFP((2, DFUNCTION, -1, "fwrite_key: key=%d size=%d, nitems=%d\n",(int) key,(int) size, (int) nitems));

  len=size*nitems;

  /* store key and length */
  frc=_sion_store_and_write_key_and_len(sion_filedesc,key,len);
  if(frc != SION_SUCCESS) {
    return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
				    "could not write meta data to file (frc=%d sid=%d) ...",  (int) frc, sid));
  }

  /* store data */
  frc=_sion_write_value(sion_filedesc,data,key,len);
  if(frc != len) {
    return(_sion_errorprint_on_rank(-1,_SION_ERROR_RETURN,sion_filedesc->rank,
				    "could not write data (%d bytes) to file (frc=%d sid=%d) ...",  (int) len, (int) frc, sid));
  }

  rc=(size_t) len / size;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_fwrite_key");
  return (rc);
}
#undef DFUNCTION



#define DFUNCTION "sion_fread_key"
/*!
 * \ingroup keyval_api
 * @brief Read data for key.
 *
 * Reads data from sion file associated to key. This function uses the
 * diffenent implementations of which one is chosen with the open call.
 *
 * @param[out]  data    buffer to be read into
 * @param[in]   key     key associated to the data
 * @param[in]   size    size of single item
 * @param[in]   nitems  number of items
 * @param[in]   sid     sion file handle
 *
 * @return  number of successfully read items
 */
size_t sion_fread_key(void *data, uint64_t key, size_t size, size_t nitems, int sid) {
  size_t rc=0, frc;
  size_t len, datalen=0;
  _sion_filedesc *sion_filedesc;

  /*                                                                      */ DPRINTFTS(-1, "enter sion_fread_key");
  DPRINTFP((8, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 
  DPRINTFP((32, DFUNCTION, -1, "fread_key: sid=%d key=%ld size=%d, nitems=%d\n",(int) sid, (long) key,(int) size, (int) nitems));

  len=size*nitems;

  /* find key and length */
  if (_sion_find_and_read_key_and_len(sion_filedesc,key,len,&datalen) !=
      SION_SUCCESS) {
    DPRINTFP((8, DFUNCTION, -1, "_sion_find_and_read_key_and_len return NOT SUCCESS\n"));
    return(SION_NOT_SUCCESS);
  }
  
  /* read as much elements of size <size> as posible */
  len = ( (int) datalen/size ) * size;

  /* read data */
  frc=_sion_read_value(sion_filedesc,data,key,len);
  if(frc != len) {
    return(SION_NOT_SUCCESS);
  }

  rc=(size_t) len / size;

  DPRINTFP((8, DFUNCTION, -1, "leave rc=%d\n",(int) rc));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_fread_key");
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "sion_key_full_scan"
/*!
 * \ingroup keyval_api
 * @brief Performs a full scan of all meta data in current file.
 *
 * @param[in]   sid     sion file handle
 *
 * @return  SION_SUCCESS on success
 */
int sion_key_full_scan(int sid) {
  size_t rc=SION_NOT_SUCCESS;
  _sion_filedesc *sion_filedesc;
  
  /*                                                                      */ DPRINTFTS(-1, "enter sion_key_full_scan");
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 
  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_key_full_scan_inline(sion_filedesc);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to perform full scan of file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_key_full_scan");
  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "sion_fread_key_iterator_reset"
/*!
 * \ingroup keyval_api
 * @brief Resets key iterator.
 *
 * @param[in]   sid     sion file handle
 *
 * @return  SION_SUCCESS on success
 */
int sion_fread_key_iterator_reset(int sid) {
  size_t rc=SION_NOT_SUCCESS;
  _sion_filedesc *sion_filedesc;
  
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 

  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_iterator_reset_inline(sion_filedesc);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to reset iterator in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "sion_fread_key_iterator_next"
/*!
 * \ingroup keyval_api
 * @brief Forward to next key.
 *
 * @param[in]   sid      sion file handle
 * @param[out]  keyptr   filled with next key
 * @param[out]  sizeptr  filled with next size
 *
 * @return  SION_SUCCESS on success
 */
int sion_fread_key_iterator_next(int sid, uint64_t *keyptr, size_t *sizeptr) {
  size_t rc=SION_NOT_SUCCESS;
  _sion_filedesc *sion_filedesc;
  
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 
  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_iterator_next_inline(sion_filedesc, keyptr, sizeptr);
    DPRINTFP((2, DFUNCTION, -1, "sion_fread_key_iterator_next: key=%d size=%d\n",(int) *keyptr,(int) *sizeptr));
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to forward iterator in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "sion_seek_key"
/*!
 * \ingroup keyval_api
 * @brief Seek to position in key.
 *
 * @param[in]  sid         sion file handle
 * @param[in]  key         key to seek in
 * @param[in]  entrynum    nth occurrence of key or SION_ABSOLUTE_POS for
 *                         absolute position
 * @param[in]  posinentry  position inside of entry
 *
 * @return  SION_SUCCESS on success
 */
int sion_seek_key(int sid, uint64_t key, int entrynum, sion_int64 posinentry) {
  size_t rc=SION_NOT_SUCCESS;
  _sion_filedesc *sion_filedesc;
  
  /*                                                                      */ DPRINTFTS(-1, "enter sion_seek_key");
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 
  
  if( (posinentry==SION_CURRENT_POS) && (entrynum!=SION_CURRENT_BLOCK) )  {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"Error: SION_CURRENT_POS set in sion_search_key, but blovk is not SION_CURRENT_BLOCK, aborting ...\n"));
  }
  
  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    DPRINTFP((2, DFUNCTION, -1, "sion_seek_key: key=%d entrynum=%d posinentry=%d\n",(int) key,(int) entrynum, (int) posinentry));
    rc=_sion_seek_key_inline(sion_filedesc, key, entrynum, posinentry);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to search within key in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_seek_key");
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "sion_get_keyval_mode"
/*!
 * \ingroup keyval_api
 * @brief Return selected mode for key value.
 *
 * @param[in]  sid  sion file handle
 *
 * @return  keyval mode, e.g. SION_KEYVAL_INLINE
 */
int sion_get_keyval_mode(int sid) {
  size_t rc=SION_KEYVAL_UNKNOWN;

  _sion_filedesc *sion_filedesc;
  
  /*                                                                      */ DPRINTFTS(-1, "enter sion_get_keyval_mode");
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_KEYVAL_UNKNOWN,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  rc=sion_filedesc->keyvalmode;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_get_keyval_mode");
  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "sion_keyval_type_to_str"
#define STR_PRT(X) case X: return # X
/*!
 * \ingroup keyval_api
 * @brief Returns key value mode as string.
 *
 * @param[in]  type  mode to be converted
 *
 * @return  string representation of mode
 */
char* sion_keyval_type_to_str (int type) {
   switch (type) {
      STR_PRT(SION_KEYVAL_NONE);
      STR_PRT(SION_KEYVAL_INLINE);
      STR_PRT(SION_KEYVAL_META);
      STR_PRT(SION_KEYVAL_HASH);
      STR_PRT(SION_KEYVAL_UNKNOWN);
   }
   return "unknown";
}
#undef DFUNCTION



#define DFUNCTION "sion_key_list_iterator_reset"
/*!
 * \ingroup keyval_api
 * @brief Resets key iterator.
 *
 * @param[in]   sid     sion file handle
 *
 * @return  SION_SUCCESS on success
 */
int sion_key_list_iterator_reset(int sid) {
  size_t rc=SION_NOT_SUCCESS;
  _sion_filedesc *sion_filedesc;
  
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 

  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_key_list_iterator_reset_inline(sion_filedesc);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to reset iterator in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "sion_key_list_iterator_next"
/*!
 * \ingroup keyval_api
 * @brief Forward to next key.
 *
 * @param[in]   sid      sion file handle
 * @param[out]  keyptr   filled with next key
 *
 * @return  SION_SUCCESS on success
 */
int sion_key_list_iterator_next(int sid, uint64_t *keyptr) {
  size_t rc=SION_NOT_SUCCESS;
  _sion_filedesc *sion_filedesc;
  
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 
  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_key_list_iterator_next_inline(sion_filedesc, keyptr);
    DPRINTFP((2, DFUNCTION, -1, "sion_fread_key_iterator_next: key=%d\n",(int) *keyptr));
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to forward iterator in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "sion_key_get_stat"
/*!
 * \ingroup keyval_api
 * @brief get statistics about key
 *
 * @param[in]   sid         sion file handle
 * @param[in]   key         key
 * @param[out]  keystatptr  data structure which will be filled
 *
 * @return  SION_SUCCESS on success
 */
int sion_key_get_stat(int sid, uint64_t key, sion_key_stat_t *keystatptr) {
  size_t rc=SION_NOT_SUCCESS;
  _sion_filedesc *sion_filedesc;
  
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if(sion_filedesc->rank<0) {   /* empty file */
    return(SION_NOT_SUCCESS);
  } 
  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_key_get_stat_inline(sion_filedesc, key, keystatptr);
    DPRINTFP((2, DFUNCTION, -1, "_sion_key_get_stat_inline: key=%d rc=%d\n",(int) key, rc));
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to get stat of key from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION


/* internal wrapper function */


#define DFUNCTION "_sion_store_and_write_key_and_len"
int _sion_store_and_write_key_and_len(_sion_filedesc *sion_filedesc, uint64_t key, size_t len) {
  int rc=0;
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_store_and_write_key_and_len_inline(sion_filedesc,key,len);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to write in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_write_value"
int _sion_write_value(_sion_filedesc *sion_filedesc, const void *data, uint64_t key, size_t len) {
  int rc=0;
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_write_value_inline(sion_filedesc,data,key,len);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to read in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_find_and_read_key_and_len"
int _sion_find_and_read_key_and_len(_sion_filedesc *sion_filedesc, uint64_t key, size_t len, size_t *datalen) {
  int rc = SION_NOT_SUCCESS;
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) {
    rc=_sion_find_and_read_key_and_len_inline(sion_filedesc, key, len, datalen);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to read in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_read_value"
int _sion_read_value(_sion_filedesc *sion_filedesc, void *data, uint64_t key, size_t len) {
  int rc=0;
  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE) { 
    rc=_sion_read_value_inline(sion_filedesc,data,key,len);
  } else {
    return(_sion_errorprint(0,_SION_ERROR_RETURN,"trying to read in keyval mode from file which is not opened in that mode, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  return (rc);
}
#undef DFUNCTION


