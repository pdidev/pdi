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

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_filedesc.h"
#include "sion_keyvalue_keymngr.h"


#define DFUNCTION "_sion_init_filedesc"
/*!\brief Initialize the sion file description
 *
 * @param *sion_filedesc        sion file description struct (_sion_filedesc)
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_init_filedesc(_sion_filedesc *sion_filedesc)
{

  int       rc = SION_SUCCESS;

  if (sion_filedesc == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot initalized, data structure is not allocated, aborting ...\n"));
  }
  sion_filedesc->fileptr = NULL;
  sion_filedesc->fname = NULL;
  sion_filedesc->sid = -1;
  sion_filedesc->rank = -1;
  sion_filedesc->lrank = -1;
  sion_filedesc->globalrank = -1;
  sion_filedesc->ntasks = -1;
  sion_filedesc->state = SION_FILESTATE_UNKNOWN;
  sion_filedesc->mode = SION_FILEMODE_UNKNOWN;
  sion_filedesc->dup_mode = SION_DESCSTATE_ORIG;
  sion_filedesc->dup_sel_rank = 0;
  sion_filedesc->dup_sel_key = 0;
  sion_filedesc->endianness = -1;
  sion_filedesc->swapbytes = 0;
  sion_filedesc->fileversion = SION_FILEFORMAT_VERSION;
  sion_filedesc->filesionversion = 1000 * SION_MAIN_VERSION + SION_SUB_VERSION;
  sion_filedesc->filesionpatchlevel = SION_SUB_VERSION;
  sion_filedesc->fsblksize = -1;        /* fs block size */
  sion_filedesc->lastchunknr = -1;        /* number of current block */
  sion_filedesc->maxchunks = -1;
  sion_filedesc->maxusedchunks = -1;

  sion_filedesc->startpos = -1;
  sion_filedesc->chunksize = -1;
  sion_filedesc->chunksize_req = -1;
  sion_filedesc->blocksizes = NULL;     /* vector of written bytes in each block for current task */

  sion_filedesc->globalskip = -1;       /* number of bytes to skip to next block of same processor */
  sion_filedesc->end_of_header = -1;
  sion_filedesc->start_of_varheader = -1;
  sion_filedesc->start_of_data = -1;

  sion_filedesc->all_chunksizes = NULL; /* only used if sion is opened from a single task for read  */
  sion_filedesc->all_globalranks = NULL;        /* only used if sion is opened from a single task for read  */
  sion_filedesc->all_localranks = NULL;         /* only used if sion is opened in mapped mode */
  sion_filedesc->all_startpointers = NULL;      /* only used if sion is opened from a single task for read  */

  sion_filedesc->all_currentpos = NULL; /* only used if sion is opened from a single task for write  */
  sion_filedesc->all_currentblocknr = NULL;     /* only used if sion is opened from a single task for write  */

  sion_filedesc->all_coll_collector = NULL;  /* only used on rank 0 if usecoll=1 */
  sion_filedesc->all_coll_collsize = NULL;   /* only used on rank 0 if usecoll=1 */
  sion_filedesc->all_coll_capability = NULL; /* only used on rank 0 if usecoll=1 */

  sion_filedesc->all_blockcount = NULL; /* size, only used if sion is opened from a single task for read  */
  sion_filedesc->all_blocksizes = NULL; /* size*maxchunks, only used if sion is opened from a single task for read  */

  sion_filedesc->currentpos = -1;       /* only used if opened  for one rank, reading */
  sion_filedesc->currentblocknr = -1;   /* only used if opened  for one rank, reading */
  sion_filedesc->debug = -1;    /* 0 or 1 for debugging */

  sion_filedesc->nfiles = -1;
  sion_filedesc->filenumber = -1;
  sion_filedesc->mapping_size = -1;
  sion_filedesc->mapping = NULL;

  sion_filedesc->ntotaltasksinfile = -1;
  sion_filedesc->nlocaltasksinfile = -1;
  sion_filedesc->filemanagedbytask = -1;

  sion_filedesc->ntasks = -1;

  sion_filedesc->multifiles = NULL;

  sion_filedesc->flag1 = _SION_FLAG1_NONE;
  sion_filedesc->flag2 = _SION_FLAG2_NONE;
  sion_filedesc->prefix = NULL;

  sion_filedesc->dataptr = NULL;
  sion_filedesc->keyvalptr = NULL;

  sion_filedesc->keyvalmode = SION_KEYVAL_NONE;
  sion_filedesc->keyvalptr = NULL;
  sion_filedesc->all_keyvalptr = NULL;

  sion_filedesc->fpbuffer      = NULL;
  sion_filedesc->fpbuffer_size = -1;

  sion_filedesc->usecache    =  0;
  sion_filedesc->cachesize   = -1; 
  sion_filedesc->cacheid     = -1;
  if(SION_CACHE_FNLEN>0) {
    sion_filedesc->cachefn[0]='\0';
  }
  sion_filedesc->cachemode = SION_CACHE_UNKNOWN;      
  sion_filedesc->cacheptr  = NULL;

  sion_filedesc->usebuffer   = 0;
  sion_filedesc->buffer      = NULL;
  sion_filedesc->buffer_size = -1;
  sion_filedesc->buffer_ptr  = 0;

  sion_filedesc->compress = 0;


  sion_filedesc->usecoll    = 0;
  sion_filedesc->collsize   = -1;
  sion_filedesc->collector  = -1;
  sion_filedesc->coll_capability = SION_CAPABILITY_FULL;
  sion_filedesc->colldebug  = 0;
  sion_filedesc->collcmdused = 0;
  sion_filedesc->fileptr_exported = 0;
  sion_filedesc->collmergemode = 0;

  sion_filedesc->usebuddy   = 0;
  sion_filedesc->buddylevel = 0;
  sion_filedesc->buddynr    = 0;
  sion_filedesc->buddies    = NULL;

  sion_filedesc->usehints   = 0;
  sion_filedesc->hinttype   = SION_HINTS_TYPE_UNKNOWN;

  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_alloc_filedesc"
/*!\brief Allocates memory for internal sion structure
 *
 * @retval      pointer to a new sion_filedesc structure
 */
_sion_filedesc * _sion_alloc_filedesc(void)
{
  _sion_filedesc *sion_filedesc;
  
  sion_filedesc = (_sion_filedesc *) malloc(sizeof(_sion_filedesc));
  if (sion_filedesc == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n", (unsigned long) sizeof(sion_filedesc));
    return(NULL);
  }
  
  return(sion_filedesc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_realloc_filedesc_blocklist"
/*!\brief Increase the memory used by the internal sion structure for the blocklist
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 * @param  maxchunks                            maximum number of chunks written by all tasks
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_realloc_filedesc_blocklist(_sion_filedesc *sion_filedesc, sion_int32 maxchunks)
{
  int       rc = SION_SUCCESS;
  int       i;
  if (sion_filedesc->maxchunks <= 0) {
    sion_filedesc->blocksizes = (sion_int64 *) malloc(maxchunks * sizeof(sion_int64));
    if (sion_filedesc->blocksizes == NULL) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_blocksizes), aborting ...\n",
			      (unsigned long) maxchunks * sizeof(sion_int64)));
    }
    for (i = 0; i < maxchunks; i++)
      sion_filedesc->blocksizes[i] = 0; /* init */
    sion_filedesc->maxchunks = maxchunks;
    DPRINTFP((2, DFUNCTION, -1, " alloc for %d chunks\n", maxchunks ));
  }
  else {
    if (sion_filedesc->maxchunks <= maxchunks) {
      sion_filedesc->blocksizes = (sion_int64 *) realloc(sion_filedesc->blocksizes, maxchunks * sizeof(sion_int64));
      for (i = sion_filedesc->maxchunks; i < maxchunks; i++)
        sion_filedesc->blocksizes[i] = 0;       /* init */
      DPRINTFP((2, DFUNCTION, -1, " realloc from %d to %d chunks\n", sion_filedesc->maxchunks, maxchunks ));
      sion_filedesc->maxchunks = maxchunks;
    }
    else {
      fprintf(stderr, "allocate temporary memory of size %zu (sion_blocksizes) not necessary (allocated size %zu) ...\n",
              (size_t) maxchunks * sizeof(sion_int64), (size_t) sion_filedesc->maxchunks * sizeof(sion_int64));
      DPRINTFP((2, DFUNCTION, -1, "allocate temporary memory of size %zu (sion_blocksizes) not necessary (allocated size %zu) ...\n",
              (size_t) maxchunks * sizeof(sion_int64), (size_t) sion_filedesc->maxchunks * sizeof(sion_int64) ));
    }
  }
  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_alloc_filedesc_all_chunksizes"
int _sion_alloc_filedesc_all_chunksizes(_sion_filedesc *sion_filedesc)
{
  int       i, rc = SION_SUCCESS;
  sion_int64 *p;
  
  if (sion_filedesc->ntasks<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate internal arrays ntasks<0, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "enter alloc arrays size=%d (%lu bytes)\n", sion_filedesc->ntasks, (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  sion_filedesc->all_chunksizes = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_chunksizes == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_chunksizes), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_alloc_filedesc_all_startpointers"
int _sion_alloc_filedesc_all_startpointers(_sion_filedesc *sion_filedesc)
{
  int       i, rc = SION_SUCCESS;
  sion_int64 *p;
  
  if (sion_filedesc->ntasks<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate internal arrays ntasks<0, aborting ...\n"));
  }
  sion_filedesc->all_startpointers = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_startpointers == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_all_startpointers), aborting ...\n",
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_alloc_filedesc_all_globalranks"
int _sion_alloc_filedesc_all_globalranks(_sion_filedesc *sion_filedesc)
{
  int       i, rc = SION_SUCCESS;
  sion_int64 *p;
  
  if (sion_filedesc->ntasks<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate internal arrays ntasks<0, aborting ...\n"));
  }
  sion_filedesc->all_globalranks = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_globalranks == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_all_globalranks), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_alloc_filedesc_all_localranks"
int _sion_alloc_filedesc_all_localranks(_sion_filedesc *sion_filedesc)
{
  int       i, rc = SION_SUCCESS;
  sion_int64 *p;
  
  if (sion_filedesc->ntasks<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate internal arrays ntasks<0, aborting ...\n"));
  }
  sion_filedesc->all_localranks = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_localranks == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_all_localranks), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_alloc_filedesc_all_keyvalptr"
int _sion_alloc_filedesc_all_keyvalptr(_sion_filedesc *sion_filedesc)
{
  int       i, rc = SION_SUCCESS;
  void     **p;
  
  if (sion_filedesc->ntasks<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate internal arrays ntasks<0, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "enter alloc arrays size=%d (%lu bytes)\n", sion_filedesc->ntasks, (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  sion_filedesc->all_keyvalptr = p = (void *) malloc(sion_filedesc->ntasks * sizeof(void *));
  if (sion_filedesc->all_keyvalptr == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_all_keyvalptr), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = NULL;
  return(rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_alloc_filedesc_arrays"
/*!\brief Allocate memory for the internal sion arrays
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_alloc_filedesc_arrays(_sion_filedesc *sion_filedesc)
{
  DPRINTFP((2, DFUNCTION, -1, "enter alloc arrays size=%d (%lu bytes)\n", sion_filedesc->ntasks, (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  
  _sion_alloc_filedesc_all_chunksizes(sion_filedesc);
  _sion_alloc_filedesc_all_startpointers(sion_filedesc);
  _sion_alloc_filedesc_all_globalranks(sion_filedesc);

  DPRINTFP((2, DFUNCTION, -1, "leave alloc arrays size=%d\n", sion_filedesc->ntasks));
  return (1);
}
#undef DFUNCTION


#define DFUNCTION "_sion_free_filedesc_all_chunksizes"
int _sion_free_filedesc_all_chunksizes(_sion_filedesc *sion_filedesc)
{
  DPRINTFP((2, DFUNCTION, -1, "enter free arrays\n"));
  if (sion_filedesc->all_chunksizes != NULL) {
    free(sion_filedesc->all_chunksizes);
    sion_filedesc->all_chunksizes=NULL;
  }
  DPRINTFP((2, DFUNCTION, -1, "leave free arrays\n"));
  return (1);
}
#undef DFUNCTION

#define DFUNCTION "_sion_free_filedesc_all_globalranks"
int _sion_free_filedesc_all_globalranks(_sion_filedesc *sion_filedesc)
{
  DPRINTFP((2, DFUNCTION, -1, "enter free arrays\n"));
  if (sion_filedesc->all_globalranks != NULL) {
    free(sion_filedesc->all_globalranks);
    sion_filedesc->all_globalranks=NULL;
  }
  DPRINTFP((2, DFUNCTION, -1, "leave free arrays\n"));
  return (1);
}
#undef DFUNCTION

#define DFUNCTION "_sion_free_filedesc_all_startpointers"
int _sion_free_filedesc_all_startpointers(_sion_filedesc *sion_filedesc)
{
  DPRINTFP((2, DFUNCTION, -1, "enter free arrays\n"));
  if (sion_filedesc->all_startpointers != NULL) {
    free(sion_filedesc->all_startpointers);
    sion_filedesc->all_startpointers=NULL;
  }
  DPRINTFP((2, DFUNCTION, -1, "leave free arrays\n"));
  return (1);
}
#undef DFUNCTION

#define DFUNCTION "_sion_free_filedesc_all_localranks"
int _sion_free_filedesc_all_localranks(_sion_filedesc *sion_filedesc)
{
  DPRINTFP((2, DFUNCTION, -1, "enter free arrays\n"));
  if (sion_filedesc->all_localranks != NULL) {
    free(sion_filedesc->all_localranks);
    sion_filedesc->all_localranks=NULL;
  }
  DPRINTFP((2, DFUNCTION, -1, "leave free arrays\n"));
  return (1);
}
#undef DFUNCTION

#define DFUNCTION "_sion_free_filedesc_all_keyvalptr"
int _sion_free_filedesc_all_keyvalptr(_sion_filedesc *sion_filedesc)
{
  int tasknr = 0;
  DPRINTFP((2, DFUNCTION, -1, "enter free arrays\n"));
  if (sion_filedesc->all_keyvalptr != NULL) {
    for(tasknr=0;tasknr<sion_filedesc->ntasks;tasknr++) {
      if (sion_filedesc->all_keyvalptr[tasknr]) {
	DPRINTFP((2, DFUNCTION, -1, "free now KEYVALPTR all_keyvalptr[%d] = %x\n",tasknr,sion_filedesc->all_keyvalptr[tasknr]));
        _sion_keyvalue_keymngr_destroy((_sion_keyvalue_keymngr **)&(sion_filedesc->all_keyvalptr[tasknr]));
      }
    }
    free(sion_filedesc->all_keyvalptr);
    sion_filedesc->all_keyvalptr=NULL;
  }
  DPRINTFP((2, DFUNCTION, -1, "leave free arrays\n"));
  return (1);
}
#undef DFUNCTION

#define DFUNCTION "_sion_free_filedesc_arrays"
/*!\brief free memory for the internal sion arrays
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_free_filedesc_arrays(_sion_filedesc *sion_filedesc)
{

  _sion_free_filedesc_all_chunksizes(sion_filedesc);
  _sion_free_filedesc_all_globalranks(sion_filedesc);
  _sion_free_filedesc_all_startpointers(sion_filedesc);
  return (1);
}
#undef DFUNCTION


#define DFUNCTION "_sion_alloc_filedesc_coll_arrays"
/*!\brief Allocate memory for the internal sion arrays
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_alloc_filedesc_coll_arrays(_sion_filedesc *sion_filedesc)
{
  int       i;
  sion_int32 *p;
  
  if (sion_filedesc->ntasks<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate internal arrays ntasks<0, aborting ...\n"));
  }

  DPRINTFP((2, DFUNCTION, -1, "enter alloc arrays size=%d (%lu bytes)\n", sion_filedesc->ntasks, (unsigned long) sion_filedesc->ntasks * sizeof(sion_int32)));
  sion_filedesc->all_coll_collsize = p = (sion_int32 *) malloc(sion_filedesc->ntasks * sizeof(sion_int32));
  if (sion_filedesc->all_coll_collsize == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_all_collsize), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int32)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  DPRINTFP((2, DFUNCTION, -1, "enter alloc arrays size=%d (%lu bytes)\n", sion_filedesc->ntasks, (unsigned long) sion_filedesc->ntasks * sizeof(sion_int32)));
  sion_filedesc->all_coll_collector = p = (sion_int32 *) malloc(sion_filedesc->ntasks * sizeof(sion_int32));
  if (sion_filedesc->all_coll_collector == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_all_collector), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int32)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  DPRINTFP((2, DFUNCTION, -1, "enter alloc arrays size=%d (%lu bytes)\n", sion_filedesc->ntasks, (unsigned long) sion_filedesc->ntasks * sizeof(sion_int32)));
  sion_filedesc->all_coll_capability = p = (sion_int32 *) malloc(sion_filedesc->ntasks * sizeof(sion_int32));
  if (sion_filedesc->all_coll_capability == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_all_capability), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int32)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;


  DPRINTFP((2, DFUNCTION, -1, "leave alloc arrays size=%d\n", sion_filedesc->ntasks));
  return (1);
}
#undef DFUNCTION


#define DFUNCTION "_sion_free_filedesc_coll_arrays"
/*!\brief free memory for the internal sion arrays
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_free_filedesc_coll_arrays(_sion_filedesc *sion_filedesc)
{

  DPRINTFP((2, DFUNCTION, -1, "enter free arrays\n"));
  if (sion_filedesc->all_coll_collsize != NULL) {
    free(sion_filedesc->all_coll_collsize);
    sion_filedesc->all_coll_collsize=NULL;
  }
  if (sion_filedesc->all_coll_collector != NULL) {
    free(sion_filedesc->all_coll_collector);
    sion_filedesc->all_coll_collector=NULL;
  }
  if (sion_filedesc->all_coll_capability != NULL) {
    free(sion_filedesc->all_coll_capability);
    sion_filedesc->all_coll_capability=NULL;
  }
  DPRINTFP((2, DFUNCTION, -1, "leave free arrays\n"));
  return (1);
}
#undef DFUNCTION

#define DFUNCTION "_sion_alloc_filedesc_block_arrays"
/*!\brief Allocate memory for the internal sion structure, fields for all chunksizes of all tasks
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_alloc_filedesc_block_arrays(_sion_filedesc *sion_filedesc)
{
  int       i;
  sion_int64 *p;

  DPRINTFP((2, DFUNCTION, -1, "enter alloc locations size=%d, maxblocks=%d\n", sion_filedesc->ntasks, sion_filedesc->maxchunks));
  sion_filedesc->all_blockcount = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_blockcount == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_blockcount), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  sion_filedesc->all_currentpos = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_currentpos == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_currentpos), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  sion_filedesc->all_currentblocknr = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_currentblocknr == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_currentblocknr), aborting ...\n",
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  sion_filedesc->all_blocksizes = p = (sion_int64 *) malloc(sion_filedesc->maxchunks * sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_blocksizes == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_blocksizes), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->maxchunks * sion_filedesc->ntasks; i++) p[i] = -1;


  DPRINTFP((2, DFUNCTION, -1, "leave alloc locations size=%d, maxblocks=%d\n", sion_filedesc->ntasks, sion_filedesc->maxchunks));
  return (1);
}
#undef DFUNCTION


#define DFUNCTION "_sion_alloc_filedesc_block_arrays_only"
/*!\brief Allocate memory for the internal sion structure, fields for all chunksizes of all tasks
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_alloc_filedesc_block_arrays_only(_sion_filedesc *sion_filedesc)
{
  int       i;
  sion_int64 *p;

  DPRINTFP((2, DFUNCTION, -1, "enter alloc locations size=%d, maxblocks=%d\n", sion_filedesc->ntasks, sion_filedesc->maxchunks));
  sion_filedesc->all_blockcount = p = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_blockcount == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_blockcount), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->ntasks; i++) p[i] = -1;

  sion_filedesc->all_blocksizes = p = (sion_int64 *) malloc(sion_filedesc->maxchunks * sion_filedesc->ntasks * sizeof(sion_int64));
  if (sion_filedesc->all_blocksizes == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot allocate temporary memory of size %lu (sion_blocksizes), aborting ...\n", 
			    (unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
  }
  for (i = 0; i < sion_filedesc->maxchunks * sion_filedesc->ntasks; i++) p[i] = -1;

  DPRINTFP((2, DFUNCTION, -1, "leave alloc locations size=%d, maxblocks=%d\n", sion_filedesc->ntasks, sion_filedesc->maxchunks));
  return (1);
}
#undef DFUNCTION

#define DFUNCTION "_sion_free_filedesc"
int _sion_free_filedesc(_sion_filedesc *sion_filedesc)
{

  int       rc = SION_SUCCESS;

  if (sion_filedesc == NULL) {
    return (0);
  }

  if (sion_filedesc->fname)
    free(sion_filedesc->fname);
  if (sion_filedesc->blocksizes)
    free(sion_filedesc->blocksizes);
  if (sion_filedesc->all_chunksizes)
    free(sion_filedesc->all_chunksizes);
  if (sion_filedesc->all_globalranks)
    free(sion_filedesc->all_globalranks);
  if (sion_filedesc->all_localranks)
    free(sion_filedesc->all_localranks);
  if (sion_filedesc->all_startpointers)
    free(sion_filedesc->all_startpointers);
  if (sion_filedesc->all_currentpos)
    free(sion_filedesc->all_currentpos);
  if (sion_filedesc->all_currentblocknr)
    free(sion_filedesc->all_currentblocknr);
  if (sion_filedesc->all_blockcount)
    free(sion_filedesc->all_blockcount);
  if (sion_filedesc->all_blocksizes)
    free(sion_filedesc->all_blocksizes);
  if (sion_filedesc->all_coll_collsize)
    free(sion_filedesc->all_coll_collsize);
  if (sion_filedesc->all_coll_collector)
    free(sion_filedesc->all_coll_collector);
  if (sion_filedesc->all_keyvalptr)
    _sion_free_filedesc_all_keyvalptr(sion_filedesc);
  if (sion_filedesc->keyvalptr)
    _sion_keyvalue_keymngr_destroy((_sion_keyvalue_keymngr **)&(sion_filedesc->keyvalptr));
  if (sion_filedesc->prefix)
    free(sion_filedesc->prefix);
  if (sion_filedesc->mapping)
    free(sion_filedesc->mapping);
  /* is only a payload pointer, memory should not freed here */
  /* if (sion_filedesc->dataptr) */
  /*   free(sion_filedesc->dataptr); */
  if (sion_filedesc->fpbuffer)
    free(sion_filedesc->fpbuffer);
  if (sion_filedesc->buffer)
    free(sion_filedesc->buffer);

  free(sion_filedesc);

  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_print_filedesc"
/*!\brief Print the initialized sion file description
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_print_filedesc(_sion_filedesc *sion_filedesc, int level, char *desc, int flag)
{

  int       rc = SION_SUCCESS;
  int       i;

  if (sion_filedesc == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, DFUNCTION ": cannot print, data structure is not allocated, aborting ...\n"));
  }

  DPRINTFP((level, desc, sion_filedesc->rank, "DUMP: sion_filedesc\n"));
  DPRINTFP((level, desc, sion_filedesc->rank, "-------------------\n"));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%x\n", "fileptr", sion_filedesc->fileptr));
  if(sion_filedesc->fileptr) {
    DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d %s\n", "fileptr->flags", sion_filedesc->fileptr->flags,_sion_get_fileptr_desc(sion_filedesc->fileptr)));
  }
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%s\n", "fname", sion_filedesc->fname));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "sid", sion_filedesc->sid));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "rank", sion_filedesc->rank));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "globalrank", sion_filedesc->globalrank));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "ntasks", sion_filedesc->ntasks));
  if(sion_filedesc->state==SION_FILESTATE_PAROPEN)           DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_PAROPEN)\n", "state", sion_filedesc->state));
  else if(sion_filedesc->state==SION_FILESTATE_SEROPEN)      DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_SEROPEN)\n", "state", sion_filedesc->state));
  else if(sion_filedesc->state==SION_FILESTATE_SEROPENRANK)  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_SEROPENRANK)\n", "state", sion_filedesc->state));
  else if(sion_filedesc->state==SION_FILESTATE_SEROPENMASTER)DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_SEROPENMASTER)\n", "state", sion_filedesc->state));
  else if(sion_filedesc->state==SION_FILESTATE_CLOSE)        DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_CLOSE)\n", "state", sion_filedesc->state));
  else if(sion_filedesc->state==SION_FILESTATE_PAROPENMAPPED)       DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_PAROPENMAPPED)\n", "state", sion_filedesc->state));
  else if(sion_filedesc->state==SION_FILESTATE_PAROPENMAPPEDMANAGED)DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_PAROPENMAPPEDMANAGED)\n", "state", sion_filedesc->state));
  else if(sion_filedesc->state==SION_FILESTATE_PAROPENMAPPEDMASTER) DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILESTATE_PAROPENMAPPEDMASTER)\n", "state", sion_filedesc->state));
  else                                                       DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (unknown)\n", "state", sion_filedesc->state));
  if(sion_filedesc->mode==SION_FILEMODE_WRITE)               DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILEMODE_WRITE)\n", "mode", sion_filedesc->mode));
  else if(sion_filedesc->mode==SION_FILEMODE_READ)           DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_FILEMODE_READ)\n", "mode", sion_filedesc->mode));
  else                                                       DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (unknown)\n", "mode", sion_filedesc->mode));
  if(sion_filedesc->dup_mode==SION_DESCSTATE_ORIG)                  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_DESCSTATE_ORIG)\n", "dup", sion_filedesc->dup_mode));
  else if(sion_filedesc->dup_mode==SION_DESCSTATE_DUP)              DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_DESCSTATE_DUP)\n", "dup", sion_filedesc->dup_mode));
  else if(sion_filedesc->dup_mode==SION_DESCSTATE_DUP_SEL_RANK)     DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_DESCSTATE_DUP_SEL_RANK)\n", "dup", sion_filedesc->dup_mode));
  else if(sion_filedesc->dup_mode==SION_DESCSTATE_DUP_SEL_RANK_KEY) DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (SION_DESCSTATE_DUP_SEL_RANK_KEY)\n", "dup", sion_filedesc->dup_mode));
  else                                                       DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d (unknown)\n", "dup", sion_filedesc->dup_mode));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "endianness", sion_filedesc->endianness));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "swapbytes", sion_filedesc->swapbytes));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "fileversion", sion_filedesc->fileversion));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "filesionversion", sion_filedesc->filesionversion));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "filesionpatchlevel", sion_filedesc->filesionpatchlevel));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "fsblksize", sion_filedesc->fsblksize));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "lastchunknr", sion_filedesc->lastchunknr));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "maxchunks", sion_filedesc->maxchunks));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "maxusedchunks", sion_filedesc->maxusedchunks));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "currentpos", sion_filedesc->currentpos));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "currentblocknr", sion_filedesc->currentblocknr));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%lld\n", "startpos", sion_filedesc->startpos));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%lld\n", "chunksize", sion_filedesc->chunksize));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%lld\n", "chunksize_req", sion_filedesc->chunksize_req));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%lld\n", "globalskip", sion_filedesc->globalskip));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%lld\n", "end_of_header", sion_filedesc->end_of_header));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%lld\n", "start_of_varheader", sion_filedesc->start_of_varheader));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%lld\n", "start_of_data", sion_filedesc->start_of_data));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "nfiles", sion_filedesc->nfiles));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "filenumber", sion_filedesc->filenumber));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=\"%s\"\n", "prefix", sion_filedesc->prefix));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%x\n", "fpbuffer", sion_filedesc->fpbuffer));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "fpbuffer_size", sion_filedesc->fpbuffer_size));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%x\n", "buffer", sion_filedesc->buffer));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "buffer_size", sion_filedesc->buffer_size));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "buffer_ptr", sion_filedesc->buffer_ptr));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "compress", sion_filedesc->compress));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "usecache",  sion_filedesc->usecache));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "cachesize", sion_filedesc->cachesize));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "usecoll",  sion_filedesc->usecoll));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "collsize", sion_filedesc->collsize));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "collector", sion_filedesc->collector));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "colldebug", sion_filedesc->colldebug));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "collcmdused", sion_filedesc->collcmdused));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "collmergemode", sion_filedesc->collmergemode));
  if(sion_filedesc->coll_capability==SION_CAPABILITY_FULL)           DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%s\n", "coll_capability", "FULL"));
  if(sion_filedesc->coll_capability==SION_CAPABILITY_ONLY_SENDER)    DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%s\n", "coll_capability", "ONLY_SENDER"));
  if(sion_filedesc->coll_capability==SION_CAPABILITY_NONE)           DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%s\n", "coll_capability", "NONE"));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "fileptr_exported", sion_filedesc->fileptr_exported));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "usebuddy",  sion_filedesc->usebuddy));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "buddylevel",  sion_filedesc->buddylevel));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "buddynr",  sion_filedesc->buddynr));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%x\n", "buddies",  sion_filedesc->buddies));

  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "usehints",  sion_filedesc->usehints));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "hinttype",  sion_filedesc->hinttype));


  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "lrank",  sion_filedesc->lrank));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "nlocaltasksinfile",  sion_filedesc->nlocaltasksinfile));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "ntotaltasksinfile",  sion_filedesc->ntotaltasksinfile));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%d\n", "filemanagedbytask",  sion_filedesc->filemanagedbytask));


  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%x\n", "dataptr", sion_filedesc->dataptr));

  if(sion_filedesc->keyvalmode==SION_KEYVAL_NONE)    DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%s\n", "keyvalmode", "NONE"));
  if(sion_filedesc->keyvalmode==SION_KEYVAL_INLINE)  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%s\n", "keyvalmode", "INLINE"));
  if(sion_filedesc->keyvalmode==SION_KEYVAL_UNKNOWN) DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%s\n", "keyvalmode", "UNKNOWN"));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%x\n", "keyvalptr", sion_filedesc->keyvalptr));
  DPRINTFP((level, desc, sion_filedesc->rank, "D: %-20s=%x\n", "all_keyvalptr", sion_filedesc->all_keyvalptr));

  /* if (sion_filedesc->nfiles > 1) */
  /*   for(i=0;i<sion_filedesc->nfiles;i++) */
  /*     DPRINTFP((  level,desc,sion_filedesc->rank,"D: file[%d]=%s\n", i,    sion_filedesc->files[i])); */


  if(sion_filedesc->blocksizes !=NULL) {
    int maxprintblocks=sion_filedesc->lastchunknr; 
    if (maxprintblocks>20) maxprintblocks=20;
    for (i = 0; i <= maxprintblocks; i++) {
      DPRINTFP((level, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "blocksizes", i, sion_filedesc->blocksizes[i]));
    }
    if(maxprintblocks<sion_filedesc->lastchunknr) {
      DPRINTFP((level, desc, sion_filedesc->rank, "D:  %-20s[....]=...\n", "blocksizes"));
    }

  }

  if( (sion_filedesc->state==SION_FILESTATE_SEROPENMASTER) || (sion_filedesc->state==SION_FILESTATE_PAROPENMAPPEDMASTER) ) {
    for (i = 0; i < sion_filedesc->nfiles; i++) {
      DPRINTFP((level, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%s\n", "file:", i, sion_filedesc->multifiles[i]->fname));
    }
  }

  if (flag&_SION_DEBUG_PRINT_ALL) {

    if(sion_filedesc->all_chunksizes !=NULL) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "all_chunksizes", i, sion_filedesc->all_chunksizes[i]));
      }
    }

    if(sion_filedesc->all_globalranks !=NULL) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "all_globalranks", i, sion_filedesc->all_globalranks[i]));
      }
    }

    if(sion_filedesc->all_localranks !=NULL) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "all_localranks", i, sion_filedesc->all_localranks[i]));
      }
    }

    if(sion_filedesc->all_startpointers !=NULL) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "all_startpointers", i, sion_filedesc->all_startpointers[i]));
      }
    }

    if(sion_filedesc->all_currentpos !=NULL) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "all_currentpos", i, sion_filedesc->all_currentpos[i]));
      }
    }

    if(sion_filedesc->all_currentblocknr !=NULL) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "all_currentblocknr", i, sion_filedesc->all_currentblocknr[i]));
      }
    }

    if(sion_filedesc->all_blockcount !=NULL) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d]=%lld\n", "all_blockcount", i, sion_filedesc->all_blockcount[i]));
      }
    }

    if((sion_filedesc->all_blocksizes != NULL) && (sion_filedesc->all_blockcount != NULL)) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	int b;
	int maxprintblocks=sion_filedesc->all_blockcount[i]; 
	if (maxprintblocks>20) maxprintblocks=20;
	for (b = 0; b < maxprintblocks ; b++) {
	  DPRINTFP((2048, desc, sion_filedesc->rank, "D:  %-20s[%4d][%d]=%lld\n", "all_blocksizes", i, b, 
		    sion_filedesc->all_blocksizes[b*sion_filedesc->ntasks+i]));
	}
	if(maxprintblocks<sion_filedesc->all_blockcount[i]) {
	  DPRINTFP((level, desc, sion_filedesc->rank, "D:  %-20s[%4d][..]=...\n", "all_blocksizes", i));
	}
      }
    }
  }

  if( (sion_filedesc->state==SION_FILESTATE_SEROPENMASTER) || (sion_filedesc->state==SION_FILESTATE_PAROPENMAPPEDMASTER) ) {
    if (flag&_SION_DEBUG_PRINT_RECURSIVE) {
      for (i = 0; i < sion_filedesc->nfiles; i++) {
	DPRINTFP((level, desc, sion_filedesc->rank, "\n"));
	DPRINTFP((level, desc, sion_filedesc->rank, "*** DUMP of multifile[%d] ***\n",i));
	_sion_print_filedesc(sion_filedesc->multifiles[i], level, desc, flag&~(_SION_DEBUG_PRINT_RECURSIVE));
      }
    }
  }

  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_get_size_of_filedesc"
/*!\brief get size of internal data structure sion_filedesc
 *
 * @param  *sion_filedesc   sion file description struct (_sion_filedesc)
 * @param  *numbytes        int, number of bytes allcated for that data structure
 * @param  *numfds          int, number of open file descriptors
 */
int _sion_get_size_of_filedesc(_sion_filedesc *sion_filedesc, int *numbytes, int *numfds)
{
  int rc=SION_SUCCESS;
  int bytes=0, fds=0, help_bytes, help_fds, i;
  
  if(sion_filedesc==NULL) return(bytes);

  DPRINTFP((2, DFUNCTION, -1, "start sid=%d nfiles=%d ntasks=%d maxchunks=%d mapping_size=%d\n", sion_filedesc->sid, 
	    sion_filedesc->nfiles,sion_filedesc->ntasks,sion_filedesc->maxchunks,sion_filedesc->mapping_size));

  help_bytes=sizeof(_sion_filedesc);
  DPRINTFP((512, DFUNCTION, -1, " sizeof(sion_filedesc)=     %5d\n", help_bytes));
  bytes+=help_bytes;

  if( (sion_filedesc->blocksizes!=NULL)  ) {
    help_bytes=sion_filedesc->maxchunks*sizeof(sion_filedesc->blocksizes[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof blocksizes=         %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_chunksizes!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_chunksizes[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_chunksizes=     %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_globalranks!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_globalranks[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_globalranks=    %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->state!=SION_FILESTATE_SEROPENMASTER) && (sion_filedesc->state!=SION_FILESTATE_PAROPENMAPPEDMASTER) ) {
    if(sion_filedesc->fileptr!=NULL) {
      fds++;
    }
  }

  if( (sion_filedesc->all_localranks!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_localranks[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_localranks=     %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_startpointers!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_startpointers[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_startpointers=  %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_currentpos!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_currentpos[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_currentpos=     %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_currentblocknr!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_currentblocknr[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_currentblocknr= %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_coll_collector!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_coll_collector[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_coll_collector= %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_coll_collsize!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_coll_collsize[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_coll_collsize=  %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_blockcount!=NULL)  ) {
    help_bytes=sion_filedesc->ntasks*sizeof(sion_filedesc->all_blockcount[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_blockcount=     %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->all_blocksizes!=NULL)  ) {
    help_bytes=sion_filedesc->maxchunks * sion_filedesc->ntasks*sizeof(sion_filedesc->all_blocksizes[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof all_blocksizes=     %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->mapping!=NULL)  ) {
    help_bytes=sion_filedesc->mapping_size * 2 * sizeof(sion_filedesc->mapping[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof mapping=            %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->prefix!=NULL)  ) {
    help_bytes=strlen(sion_filedesc->prefix);
    DPRINTFP((512, DFUNCTION, -1, " sizeof prefix=             %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->fpbuffer!=NULL)  ) {
    help_bytes=sion_filedesc->fpbuffer_size;
    DPRINTFP((512, DFUNCTION, -1, " sizeof fpbuffer=           %5d\n", help_bytes));
    bytes+=help_bytes;
  }

  if( (sion_filedesc->buffer!=NULL)  ) {
    help_bytes=sion_filedesc->buffer_size;
    DPRINTFP((512, DFUNCTION, -1, " sizeof buffer=             %5d\n", help_bytes));
    bytes+=help_bytes;
  }


  if( (sion_filedesc->state==SION_FILESTATE_PAROPEN) || (sion_filedesc->state==SION_FILESTATE_SEROPENRANK) ) {
    if( (sion_filedesc->keyvalptr!=NULL)  ) {
      help_bytes=_sion_keyvalue_keymngr_key_get_sizeof(sion_filedesc->keyvalptr);
      DPRINTFP((512, DFUNCTION, -1, " sizeof keyvalptr=          %5d\n", help_bytes));
      bytes+=help_bytes;
    }
  }

  if( (sion_filedesc->state==SION_FILESTATE_SEROPEN) || (sion_filedesc->state==SION_FILESTATE_SEROPENMASTER) 
      || (sion_filedesc->state==SION_FILESTATE_PAROPENMAPPEDMASTER) || (sion_filedesc->state==SION_FILESTATE_PAROPENMAPPEDMANAGED)
      || (sion_filedesc->state==SION_FILESTATE_PAROPENMAPPED)  ) {

    if( (sion_filedesc->all_keyvalptr!=NULL)  ) {
      for (i = 0; i < sion_filedesc->ntasks; i++) {
	if(sion_filedesc->all_keyvalptr[i]!=NULL) {
	  help_bytes=_sion_keyvalue_keymngr_key_get_sizeof(sion_filedesc->all_keyvalptr[i]);
	  DPRINTFP((512, DFUNCTION, -1, " sizeof all_keyvalptr[%d]=  %5d\n", i, help_bytes));
	  bytes+=help_bytes;
	}
      }
    }
  }

  if( (sion_filedesc->state==SION_FILESTATE_SEROPENMASTER) || (sion_filedesc->state==SION_FILESTATE_PAROPENMAPPEDMASTER) ) {

    help_bytes=sion_filedesc->nfiles*sizeof(sion_filedesc->multifiles[0]);
    DPRINTFP((512, DFUNCTION, -1, " sizeof multifiles=         %5d\n", help_bytes));
    bytes+=help_bytes;

    for (i = 0; i < sion_filedesc->nfiles; i++) {
      
      rc=_sion_get_size_of_filedesc(sion_filedesc->multifiles[i], &help_bytes, &help_fds);
      DPRINTFP((512, DFUNCTION, -1, " sizeof multifile[%d]=       %5d + %1d\n", i, help_bytes, help_fds));
      bytes+=help_bytes;
      fds+=help_fds;
    }
  }

  *numbytes=bytes;*numfds=fds;
  DPRINTFP((2, DFUNCTION, -1, "leave total_size           =%5d + %d fds\n", bytes, fds));

  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_dup_filedesc"
/*!\brief duplicates a filedesc data structure, not copying/reopen active file pointers
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 * @retval      ptr to a copy of sion_filedesc, or NULL if fails
 */
_sion_filedesc* _sion_dup_filedesc(_sion_filedesc *sion_filedesc)
{
  _sion_filedesc *new_fd=NULL;
  
  if (sion_filedesc == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure  (sion_filedesc), aborting ...\n");
    return(NULL);
  }
  new_fd=_sion_alloc_filedesc();

  new_fd->dup_mode = SION_DESCSTATE_DUP;

  /* values which will not be copied */
  new_fd->fileptr    = NULL;
  new_fd->sid = -1;
  new_fd->dataptr  = NULL; /* not needed in a dup fd */
  new_fd->keyvalptr  = NULL; /* not needed in a dup fd */
  new_fd->usecoll     = 0;
  new_fd->maxchunks   = -1;

  /* copy scalar values */
  new_fd->fname      = strdup(sion_filedesc->fname);
  new_fd->rank       = sion_filedesc->rank;
  new_fd->lrank      = sion_filedesc->lrank;
  new_fd->globalrank = sion_filedesc->globalrank;

  new_fd->ntasks     = sion_filedesc->ntasks;
  new_fd->state      = sion_filedesc->state;
  new_fd->mode       = sion_filedesc->mode;
  new_fd->endianness = sion_filedesc->endianness;
  new_fd->swapbytes  = sion_filedesc->swapbytes;
  new_fd->fileversion        = sion_filedesc->fileversion;
  new_fd->filesionversion    = sion_filedesc->filesionversion;
  new_fd->filesionpatchlevel =  sion_filedesc->filesionpatchlevel;
  new_fd->fsblksize     =  sion_filedesc->fsblksize; 
  new_fd->lastchunknr   =  sion_filedesc->lastchunknr;
  new_fd->maxusedchunks = sion_filedesc->maxusedchunks;
  new_fd->startpos      = sion_filedesc->startpos;
  new_fd->chunksize     = sion_filedesc->chunksize;
  new_fd->chunksize_req = sion_filedesc->chunksize_req;

  new_fd->globalskip         = sion_filedesc->globalskip;
  new_fd->end_of_header      = sion_filedesc->end_of_header;
  new_fd->start_of_varheader = sion_filedesc->start_of_varheader;
  new_fd->start_of_data      = sion_filedesc->start_of_data;

  new_fd->currentpos     = sion_filedesc->currentpos;
  new_fd->currentblocknr = sion_filedesc->currentblocknr;
  new_fd->debug          = sion_filedesc->debug;

  new_fd->nfiles         = sion_filedesc->nfiles;
  new_fd->filenumber     = sion_filedesc->filenumber;
  new_fd->mapping_size   = sion_filedesc->mapping_size;

  new_fd->ntotaltasksinfile = sion_filedesc->ntotaltasksinfile;
  new_fd->nlocaltasksinfile = sion_filedesc->nlocaltasksinfile;
  new_fd->filemanagedbytask = sion_filedesc->filemanagedbytask;

  new_fd->ntasks =  sion_filedesc->ntasks;
  new_fd->flag1  =  sion_filedesc->flag1;
  new_fd->flag2  =  sion_filedesc->flag2;
  new_fd->fpbuffer_size =  sion_filedesc->fpbuffer_size;

  new_fd->usecache  =  sion_filedesc->usecache;
  new_fd->cachesize =  sion_filedesc->cachesize; 
  new_fd->cacheid   =  sion_filedesc->cacheid;
  strcpy(new_fd->cachefn,sion_filedesc->cachefn);

  new_fd->cachemode   = sion_filedesc->cachemode;      
  new_fd->usebuffer   = sion_filedesc->usebuffer;
  new_fd->buffer_size = sion_filedesc->buffer_size;
  new_fd->buffer_ptr  = sion_filedesc->buffer_ptr;
  new_fd->compress    = sion_filedesc->compress;
  new_fd->keyvalmode  = sion_filedesc->keyvalmode;

  new_fd->collsize    = sion_filedesc->collsize;
  new_fd->coll_capability   = sion_filedesc->coll_capability;
  new_fd->collector   = sion_filedesc->collector;
  new_fd->colldebug   = sion_filedesc->colldebug;
  new_fd->collcmdused = sion_filedesc->collcmdused;
  new_fd->usebuddy    = sion_filedesc->usebuddy;
  new_fd->buddylevel  = sion_filedesc->buddylevel;
  new_fd->buddynr     = sion_filedesc->buddynr;
  new_fd->buddies     = sion_filedesc->buddies;
  new_fd->usehints    = sion_filedesc->usehints;
  new_fd->hinttype    = sion_filedesc->hinttype;
  new_fd->fileptr_exported = sion_filedesc->fileptr_exported;
  new_fd->collmergemode   = sion_filedesc->collmergemode;

  new_fd->prefix=NULL;
  if(sion_filedesc->prefix)   new_fd->prefix = strdup(sion_filedesc->prefix);

  new_fd->blocksizes = NULL;     /* vector of written bytes in each block for current task */
  new_fd->all_chunksizes = NULL;         /* only used if sion is opened from a single task for read  */
  new_fd->all_globalranks = NULL;        /* only used if sion is opened from a single task for read  */
  new_fd->all_localranks = NULL;         /* only used if sion is opened in mapped mode */
  new_fd->all_startpointers = NULL;      /* only used if sion is opened from a single task for read  */

  new_fd->all_currentpos = NULL;         /* only used if sion is opened from a single task for write  */
  new_fd->all_currentblocknr = NULL;     /* only used if sion is opened from a single task for write  */

  new_fd->all_coll_collector = NULL;     /* only used on rank 0 if usecoll=1 */
  new_fd->all_coll_collsize = NULL;      /* only used on rank 0 if usecoll=1 */

  new_fd->all_blockcount = NULL;         /* size, only used if sion is opened from a single task for read  */
  new_fd->all_blocksizes = NULL;         /* size*maxchunks, only used if sion is opened from a single task for read  */

  new_fd->all_keyvalptr = NULL;

  new_fd->mapping = NULL;
  new_fd->multifiles = NULL;

  new_fd->fpbuffer = NULL;
  new_fd->cacheptr = NULL;
  new_fd->buffer   = NULL;

  return (new_fd);
}
#undef DFUNCTION
