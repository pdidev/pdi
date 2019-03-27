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
 * Functions related to the duplication of sion file descriptor.
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_metadata.h"
#include "sion_filedesc.h"
#include "sion_fd.h"
#include "sion_file.h"
#include "sion_printts.h"
#include "sion_keyvalue.h"

#include "sion_dup.h"
#include "sion_buffer.h"


#define DFUNCTION "_sion_dup"
/*!
 * @brief Create a duplicated sion file descriptor.
 *
 * @param[in]   sid    original sid to be duplicated
 * @param[in]   mode   mode for duplication (SION_DUP_RANK or SION_DUP_RANK_KEY)
 * @param[in]   rank   rank for new file descriptor
 * @param[in]   key    key for new file descriptor
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dup( int sid, int mode, int rank, uint64_t key ) {
  int        rc = SION_NOT_SUCCESS, new_sid = SION_ID_NOT_VALID;
  _sion_filedesc *sion_filedesc;
  _sion_filedesc *new_filedesc;

  /*                                                                      */ DPRINTFTS(-1, "enter sion_dup");
  DPRINTFP((2, DFUNCTION, -1, "enter dup sid=%d\n",sid));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN, DFUNCTION ": invalid sion_filedesc, returning %d ...\n", sid));
  }

  if (sion_filedesc->mode != SION_FILEMODE_READ) {
    DPRINTFP((8, DFUNCTION, -1, "invalid opened\n"));
    return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
				    DFUNCTION "[%2d]: file is not opened in read mode, returning ...", sion_filedesc->rank));
  }

  _sion_print_filedesc(sion_filedesc, 512, "_sion_dup_orig ", _SION_DEBUG_PRINT_ALL|_SION_DEBUG_PRINT_RECURSIVE);


  new_filedesc = _sion_dup_filedesc(sion_filedesc);
  if (!new_filedesc) {
    return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank, DFUNCTION"[%2d]: could not duplicate internal data structure, returning ...", sion_filedesc->rank));
  }
  /* reset dup_state if only one rank/key is selected */
  if(mode==SION_DUP_RANK) {
    new_filedesc->dup_mode = SION_DESCSTATE_DUP_SEL_RANK;
    new_filedesc->dup_sel_rank = (sion_int32) rank;
  }
  if(mode==SION_DUP_RANK_KEY) {
    new_filedesc->dup_mode = SION_DESCSTATE_DUP_SEL_RANK_KEY;
    new_filedesc->dup_sel_rank = (sion_int32) rank;
    new_filedesc->dup_sel_key  = (sion_uint64) key;
  }

  /* New sion file handle */
  new_sid = _sion_newvcd(new_filedesc, SION_FILEDESCRIPTOR);
  new_filedesc->sid=new_sid;


  /* mode PAROPEN: in this mode is only one filedesc structure defined; if a
       multifile was opened each task is only taking part on one of these
       files, therefore we have not to duplicate recursive */
  if (sion_filedesc->state == SION_FILESTATE_PAROPEN) {
    DPRINTFP((8, DFUNCTION, -1, "file is in state SION_FILESTATE_PAROPEN\n"));
    rc=_sion_dup_paropen(sion_filedesc, new_filedesc);
  }

  /* mode seropen: in this mode a sion file could have multiple filedesc structure when opening a SION multi-file */
  if (sion_filedesc->state == SION_FILESTATE_SEROPEN) {
    DPRINTFP((8, DFUNCTION, -1, "file is in state SION_FILESTATE_SEROPEN\n"));
    /* TODO WF implement */
    return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank, 
				    DFUNCTION "[%2d]: mode not implemented, aborting ...", sion_filedesc->rank));
  }

  /* mode seropenrank: in this mode only a simple filedesc structure is used one file, one task */
  if (sion_filedesc->state == SION_FILESTATE_SEROPENRANK) {
    DPRINTFP((8, DFUNCTION, -1, "file is in state SION_FILESTATE_SEROPENRANK\n"));
    /* TODO WF implement */
    return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank, 
				    DFUNCTION "[%2d]: mode not implemented, aborting ...", sion_filedesc->rank));
  }

  /* mode PAROPEN_MAPPED_MASTER: in this mode the whole structure including sub-datastructures for multifiles have to be duplicated */
  if (sion_filedesc->state == SION_FILESTATE_PAROPENMAPPEDMASTER) {
    DPRINTFP((8, DFUNCTION, -1, "file is in state SION_FILESTATE_PAROPENMAPPEDMASTER\n"));
    rc=_sion_dup_paropenmappedmaster(sion_filedesc, new_filedesc);
  }


  if (rc!=SION_SUCCESS) {
    return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank, 
				    DFUNCTION "[%2d]: could not duplicate, returning ...", sion_filedesc->rank));
  }


  /* set position on selected rank if multiple ranks are available
     Hints: in the optimized version (to be impl.) 
            this is not needed: only the selected rank is copied */
  if ( (sion_filedesc->state == SION_FILESTATE_SEROPEN) || 
       (sion_filedesc->state == SION_FILESTATE_PAROPENMAPPEDMASTER)
      ){
    /* _sion_print_filedesc(new_filedesc, 512, "_sion_dup_clone_before_seek", _SION_DEBUG_PRINT_ALL|_SION_DEBUG_PRINT_RECURSIVE); */
    sion_seek(new_filedesc->sid,rank,SION_CURRENT_CHUNK,SION_CURRENT_POS);
  }

  _sion_print_filedesc(new_filedesc, 512, "_sion_dup_clone", _SION_DEBUG_PRINT_ALL|_SION_DEBUG_PRINT_RECURSIVE);

  DPRINTFP((2, DFUNCTION, -1, "leave dup sid=%d new_sid=%d\n",sid,new_sid));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_dup");

  return(new_sid);

}
#undef DFUNCTION

#define DFUNCTION "_sion_dedup"
/*!
 * @brief Destroy a duplicated sion file descriptor.
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dedup( int sid ) {
  int rc = SION_SUCCESS;
  int filenr, lfile, lrank;
  _sion_filedesc *sion_filedesc;
  _sion_filedesc *sion_filedesc_sub;

  /*                                                                      */ DPRINTFTS(-1, "enter sion_dedup");
  DPRINTFP((2, DFUNCTION, -1, "enter dedup sid=%d\n",sid));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN, DFUNCTION ": invalid sion_filedesc, returning %d ...\n", sid));
  }

  if (sion_filedesc->dup_mode == SION_DESCSTATE_ORIG) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank, DFUNCTION "[%2d]: descriptor is not a duplicate, returning ...", sion_filedesc->rank));
  }

#ifdef SION_DEBUG
  {
    int numbytes, numfds;
    sion_get_sizeof(sid, &numbytes, &numfds);
    DPRINTFP((2, DFUNCTION, -1, "internal data size of sid %2d (%d bytes, %d fds) \n", sid,numbytes, numfds));
  }
#endif  


  /* close all files if multi file */
  if ( (sion_filedesc->state == SION_FILESTATE_SEROPENMASTER) 
       || (sion_filedesc->state == SION_FILESTATE_PAROPENMAPPEDMASTER) ) {

    /* pointer to keyval structure -> all_keyvalptr */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) {
      lfile=sion_filedesc->filenumber;
      lrank=sion_filedesc->rank; /* index to local list */
      sion_filedesc_sub=sion_filedesc->multifiles[lfile];
      if(   (lrank>=0) && (lfile>=0)  
	 && (sion_filedesc->keyvalptr!=NULL) 
	 && (sion_filedesc_sub->all_keyvalptr!=NULL) ) {	  

	DPRINTFP((2, DFUNCTION, -1, "keyvalptr all_keyvalptr[%d] = %x\n",lrank,sion_filedesc->keyvalptr));
	if(sion_filedesc_sub->all_keyvalptr[lrank]!=NULL) {
	  DPRINTFP((2, DFUNCTION, -1, "WARNING keyvalptr already set all_keyvalptr[%d] = %x new =%x \n",lrank,
		    sion_filedesc_sub->all_keyvalptr[lrank],sion_filedesc->keyvalptr));
	  sion_filedesc_sub->all_keyvalptr[lrank] = sion_filedesc->keyvalptr; 
	}
	sion_filedesc_sub->all_keyvalptr[lrank] = sion_filedesc->keyvalptr;
      }
    }

    /* close file and free data structure for each sub-file */
    for(filenr=0;filenr<sion_filedesc->nfiles;filenr++) {
      sion_filedesc_sub=sion_filedesc->multifiles[filenr];

      if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE) {
	sion_filedesc_sub->keyvalptr = NULL;
      }
      if (sion_filedesc_sub->usebuffer)  _sion_buffer_flush(sion_filedesc_sub);
      if (sion_filedesc_sub->fileptr!=NULL) {
	_sion_file_close(sion_filedesc_sub->fileptr);
	sion_filedesc_sub->fileptr=NULL;
      }
      _sion_free_filedesc(sion_filedesc_sub);
      sion_filedesc->multifiles[filenr] = NULL;
    }

    free(sion_filedesc->multifiles);

    /* the top-level keyvalptr is a pointer to one of the all_keyvalptr of the subfiles, which are already freed  */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc->keyvalptr = NULL;

    _sion_freevcd(sid);
    _sion_free_filedesc(sion_filedesc);
    sion_filedesc = NULL;

  } else {
    /* close file and free data structure */
    if (sion_filedesc->usebuffer) {
      _sion_buffer_flush(sion_filedesc);
    }
    _sion_file_close(sion_filedesc->fileptr);
    sion_filedesc->fileptr=NULL;
    _sion_freevcd(sid);
    _sion_free_filedesc(sion_filedesc);
    sion_filedesc = NULL;
  }

  DPRINTFP((2, DFUNCTION, -1, "leave dedup sid=%d rc=%d\n",sid,rc));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_dedup");

  return(rc);

}
#undef DFUNCTION


#define DFUNCTION "_sion_dup_paropen"
/*!
 * @brief Duplicate sion_filedesc if the file is opened in paropen_mapped_master.
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dup_paropen( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc ) {
  int rc = SION_SUCCESS;

  DPRINTFP((2, DFUNCTION, -1, "enter \n"));
  
  _sion_dup_blocksizes(sion_filedesc, new_filedesc);
  
  /* open again the file */
  new_filedesc->fileptr=_sion_file_open(new_filedesc->fname,sion_filedesc->fileptr->flags,0);
  if (!new_filedesc->fileptr) {
    return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank, 
				    DFUNCTION "[%2d]: could not duplicate fileptr, returning ...", sion_filedesc->rank));
  }

  /* set filepointer to position in original descriptor */
  _sion_file_set_position(new_filedesc->fileptr, new_filedesc->currentpos);

 
  /* duplicate keyval structures */
  if(new_filedesc->keyvalmode!=SION_KEYVAL_NONE) {
    _sion_dup_keyvalptr(sion_filedesc, new_filedesc);
  }

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));

  return(rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_dup_paropenmappedmaster"
/*!
 * @brief Duplicate sion_filedesc if the file is opened in paropen_mapped_master.
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dup_paropenmappedmaster( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc ) {
  int rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc_sub;
  int filenr, lfile, lrank;

  DPRINTFP((2, DFUNCTION, -1, "enter \n"));
  
  /* transfer meta data to corresponding sub datastructure, changes maybe caused by seeks before dup  */
  lfile=sion_filedesc->filenumber;
  lrank=sion_filedesc->rank; /* index to local list */
  sion_filedesc_sub=sion_filedesc->multifiles[lfile];

  sion_filedesc_sub->currentpos     = sion_filedesc->currentpos;
  sion_filedesc_sub->currentblocknr = sion_filedesc->currentblocknr;

  /* pointer to keyval structure */
  if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE)  sion_filedesc_sub->keyvalptr = sion_filedesc->keyvalptr;

  /* store data of current rank on sub datastructure */
  DPRINTFP((4, DFUNCTION, -1, "store current information lrank=%d lastchunknr=%d\n", lrank,sion_filedesc_sub->lastchunknr));
  sion_filedesc_sub->all_currentpos[lrank]     = sion_filedesc_sub->currentpos;
  sion_filedesc_sub->all_currentblocknr[lrank] = sion_filedesc_sub->currentblocknr;

  /* pointer to keyval structure */
  if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE)  {
    DPRINTFP((2, DFUNCTION, -1, "keyvalptr all_keyvalptr[%d] = %x\n",lrank,sion_filedesc_sub->keyvalptr));
    sion_filedesc_sub->all_keyvalptr[lrank] = sion_filedesc_sub->keyvalptr;
  }

  _sion_dup_blocksizes(sion_filedesc, new_filedesc);

  /* allocate vector for all datastructures */
  new_filedesc->multifiles = (_sion_filedesc **) malloc(sion_filedesc->nfiles * sizeof(_sion_filedesc*));
  if (new_filedesc->multifiles == NULL) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure vector of size %lu (sion_filedesc), aborting ...\n", 
			    (unsigned long) sion_filedesc->nfiles * sizeof(_sion_filedesc*)));
  }

  /* create data structure for every multifile */
  for(filenr=0;filenr<sion_filedesc->nfiles;filenr++) {
    new_filedesc->multifiles[filenr] = _sion_dup_filedesc(sion_filedesc->multifiles[filenr]);

    new_filedesc->multifiles[filenr]->dup_mode=new_filedesc->dup_mode;
    new_filedesc->multifiles[filenr]->dup_sel_rank=new_filedesc->dup_sel_rank;
    new_filedesc->multifiles[filenr]->dup_sel_key=new_filedesc->dup_sel_key;

    /* open again the file */
    if(sion_filedesc->multifiles[filenr]->fileptr!=NULL) { /* only if needed */
      DPRINTFP((2, DFUNCTION, -1, " open file for multifiles[%d]\n",filenr));
      new_filedesc->multifiles[filenr]->fileptr=_sion_file_open(sion_filedesc->multifiles[filenr]->fname,
								sion_filedesc->multifiles[filenr]->fileptr->flags,0);
      if (!new_filedesc->multifiles[filenr]->fileptr) {
	return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank, DFUNCTION 
					"[%2d]: could not duplicate fileptr, returning ...", sion_filedesc->rank));
      }

      /* set filepointer to position in original descriptor */
      _sion_file_set_position(new_filedesc->multifiles[filenr]->fileptr, new_filedesc->multifiles[filenr]->currentpos);

      /* copy further data structures */
      _sion_dup_blocksizes(sion_filedesc->multifiles[filenr], new_filedesc->multifiles[filenr]);
      
      /* copy further all_* structures */
      _sion_dup_all_ds(sion_filedesc->multifiles[filenr], new_filedesc->multifiles[filenr]);
      
      /* duplicate keyval structures */
      if(new_filedesc->multifiles[filenr]->keyvalmode!=SION_KEYVAL_NONE) {
	_sion_dup_all_keyvalptr(sion_filedesc->multifiles[filenr], new_filedesc->multifiles[filenr]);
      }
      
    }
    

  }

  /* set file pointer of master, position is already set */
  lfile=sion_filedesc->filenumber; 
  lrank=sion_filedesc->rank;
  new_filedesc->fileptr=new_filedesc->multifiles[lfile]->fileptr;
    
  if(new_filedesc->keyvalmode!=SION_KEYVAL_NONE) {
    DPRINTFP((2, DFUNCTION, -1, "set keyvalptr on lfile=%d,lrank=%d to %x\n",lfile,lrank,new_filedesc->keyvalptr));
    new_filedesc->keyvalptr=new_filedesc->multifiles[lfile]->all_keyvalptr[lrank];
  }
  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));

  return(rc);

}
#undef DFUNCTION


#define DFUNCTION "_sion_dup_blocksizes"
/*!
 * @brief Duplicate the blocksizes array of sion_filedesc.
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dup_blocksizes( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc ) {
  int rc = SION_SUCCESS;
  int i;

  DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "maxusedchunks=%d maxchunks=%d (%d)\n", sion_filedesc->maxusedchunks,sion_filedesc->maxchunks, MAXCHUNKS));

  /* realloc array */
  _sion_realloc_filedesc_blocklist(new_filedesc, new_filedesc->maxusedchunks);
  
  /* copy data fields */
  if(sion_filedesc->blocksizes !=NULL) {
    for (i = 0; i <= sion_filedesc->lastchunknr; i++) {
      new_filedesc->blocksizes[i] = sion_filedesc->blocksizes[i];
    }
  }


  return(rc);
}
#undef DFUNCTION



#define DFUNCTION "_sion_dup_all_ds"
/*!
 * @brief Duplicate the all* array of sion_filedesc.
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dup_all_ds( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc ) {
  int rc = SION_SUCCESS;
  int i,ltask;

  DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "ntasks=%d\n", sion_filedesc->ntasks));

  /* alloc arrays */
  _sion_alloc_filedesc_all_chunksizes(new_filedesc);
  _sion_alloc_filedesc_all_globalranks(new_filedesc);
  _sion_alloc_filedesc_all_localranks(new_filedesc);
  _sion_alloc_filedesc_all_startpointers(new_filedesc);

  _sion_alloc_filedesc_block_arrays(new_filedesc); /* all_blockcount, all_currentpos, all_currentblocknr, all_blocksizes */
  
  /* copy data fields */
  for(ltask=0;ltask<new_filedesc->ntasks;ltask++) {
    new_filedesc->all_chunksizes[ltask]=sion_filedesc->all_chunksizes[ltask];
    new_filedesc->all_globalranks[ltask]=sion_filedesc->all_globalranks[ltask];
    new_filedesc->all_localranks[ltask]=sion_filedesc->all_localranks[ltask];
    new_filedesc->all_startpointers[ltask]=sion_filedesc->all_startpointers[ltask];
    new_filedesc->all_currentpos[ltask]=sion_filedesc->all_currentpos[ltask];
    new_filedesc->all_currentblocknr[ltask]=sion_filedesc->all_currentblocknr[ltask];
    new_filedesc->all_blockcount[ltask]=sion_filedesc->all_blockcount[ltask];
    DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "  sel all_*[%d]\n", ltask));
  }
  
  DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "  sel all_blocksizes[0..%d]\n", new_filedesc->maxchunks * new_filedesc->ntasks));
  for (i = 0; i < new_filedesc->maxchunks * new_filedesc->ntasks; i++) {
    /* DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "  all_blocksizes[%d] = %ld <- %ld\n", i, (long) new_filedesc->all_blocksizes[i], (long) sion_filedesc->all_blocksizes[i])); */
    new_filedesc->all_blocksizes[i] = sion_filedesc->all_blocksizes[i];
  }

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_dup_keyvalptr"
/*!
 * @brief Duplicate the keyvalue data structure of sion_filedesc.
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dup_keyvalptr( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc ) {
  int rc = 0;

  DPRINTFP((2, DFUNCTION, -1, "enter\n"));

  rc=_sion_keyval_dup_dataptr(sion_filedesc,new_filedesc);

  DPRINTFP((2, DFUNCTION, -1, "leave rc=%d\n",rc));
  
  return(rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_dup_all_keyvalptr"
/*!
 * @brief Duplicate the keyvalue data structures of sion_filedesc.
 *
 * @return     SION_SUCCESS if okay
 */
int _sion_dup_all_keyvalptr( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc ) {
  int rc = SION_SUCCESS;
  int ltask;
  void *savep;

  DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "ntasks=%d\n", sion_filedesc->ntasks));

  /* alloc arrays */
  _sion_alloc_filedesc_all_keyvalptr(new_filedesc);
  
  /* dup keyvalptr */
  savep=sion_filedesc->keyvalptr;
  for(ltask=0;ltask<new_filedesc->ntasks;ltask++) {
    if(sion_filedesc->all_keyvalptr[ltask]!=NULL) {
      sion_filedesc->keyvalptr=sion_filedesc->all_keyvalptr[ltask];

      _sion_keyval_dup_dataptr(sion_filedesc,new_filedesc);

      DPRINTFP((2, DFUNCTION, -1, "keyvalptr all_keyvalptr[%d] = %x\n",ltask,new_filedesc->keyvalptr));
      new_filedesc->all_keyvalptr[ltask]=new_filedesc->keyvalptr;

      DPRINTFP((32, DFUNCTION, sion_filedesc->rank, "  duplicated keyvalptr[%d]\n", ltask));

    }

  }
  sion_filedesc->keyvalptr=savep;
  
  return(rc);
}
#undef DFUNCTION
