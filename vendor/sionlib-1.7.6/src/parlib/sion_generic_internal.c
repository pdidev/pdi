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
 * \brief Internal Functions(parallel)
 *
 * \date Sep 2, 2008
 * \author gstu0808
 *
 */

#define _XOPEN_SOURCE 700


#if defined(_SION_MSA_HOSTNAME_REGEX)
#include <regex.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_file.h"
#include "sion_filedesc.h"
#include "sion_fd.h"
#include "sion_metadata.h"
#include "sion_internal.h"
#include "sion_printts.h"
#include "sion_keyvalue.h"
#include "sion_flags.h"

#include "sion_cache.h" 
#include "sion_buffer.h" 
#include "sion_flags.h"
#include "sion_hints.h" 
#include "sion_generic_internal.h"
#include "sion_generic_collective.h"
#include "sion_generic_buddy.h"
#include "sion_internal_startptr.h"

/*!\brief Generic parallel open of one direct access file.
 *
 * @param  sid                   sion file handle
 * @param  fname                 filename to use
 * @param  flags_store           flags from already parsed file_mode (see also sion_internal.h)
 * @param  prefix                prefix to be used in case of multiple files
 * @param  numFiles              Number of files to open
 * @param  filenumber            file number
 * @param  chunksize             chunksize on this task
 * @param  fsblksize             blocksize of filesystem (must be equal on all processors)
 * @param  rank                  rank of the current process
 * @param  ntasks                number of processes
 * @param  globalrank            any global unique id for this task, will be stored in sion file, usefull if comm is not MPI_COMM_WORLD
 *                               typical: globalrank= rank in MPI_COMM_WORLD
 * @param  flag                  sion options flag
 * @param  fileptr               filepointer for this task
 * @param  sion_gendata          structure, containing references to commgroups and callbacks
 * @param  buddy_data            data describing the buddy checkpointing
 *
 * @return  sion file handle
 *          -1 if error occured
 */
int _sion_paropen_generic_one_file(
				   int    sid,
				   char  *fname,
				   _sion_flags_store *flags_store,
				   char  *prefix,
				   int   *numFiles,
				   int   *filenumber, 
				   sion_int64  *chunksize,
				   sion_int32  *fsblksize,
				   int    rank,
				   int    ntasks,
				   int   *globalrank,
				   int    flag,
				   FILE **fileptr,
				   _sion_generic_gendata *sion_gendata,
				   _sion_generic_buddy   *buddy_data )
{

  int       i, j;
  int       rc;
	  
  _sion_filedesc *sion_filedesc;
  _sion_fileptr  *sion_fileptr; 

  int       nfiles, filenum;
  sion_int64 lchunksize, lstartpointer, lglobalrank, new_fsblocksize, helpint64, apiflag;
  sion_int64 *sion_tmpintfield = NULL;
  sion_int32 *sion_tmpintfield_map   = NULL, helpint32;
  sion_int32 *sion_tmpintfield_buddy32 = NULL;
  sion_int64 *sion_tmpintfield_buddy64 = NULL;
  void *comm_group=NULL; 
  int do_open_file;

  _sion_flags_entry* flags_entry = NULL;

  if (flags_store->mask&_SION_FMODE_POSIX) apiflag=SION_FILE_FLAG_POSIX;
  else if (flags_store->mask&_SION_FMODE_SIONFWD) apiflag=SION_FILE_FLAG_SIONFWD;
  else                                     apiflag=SION_FILE_FLAG_ANSI;

  DPRINTFP((2, "_sion_paropen_generic_one_file", rank, "enter parallel open of file %s in mode %d #tasks=%d\n", fname, (int) flags_store->mask, ntasks));
  DPRINTFP((32, "_sion_paropen_generic_one_file", rank, "sizeof: int=%d long=%d longlong=%d sion_int32=%d sion_int64=%d\n", sizeof(int), sizeof(long),
	    sizeof(long long), sizeof(sion_int32), sizeof(sion_int64)));

  /* some shortcuts */
  nfiles  = *numFiles;
  filenum = *filenumber;

  /* select local communicator */
  if(flag& _SION_INTERNAL_FLAG_NORMAL )        comm_group=sion_gendata->comm_data_local;
  if(flag& _SION_INTERNAL_FLAG_BUDDY_NORMAL )  comm_group=sion_gendata->comm_data_local;
  if(flag& _SION_INTERNAL_FLAG_BUDDY_SEND )    comm_group=buddy_data->buddy_send.commgroup;
  if(flag& _SION_INTERNAL_FLAG_BUDDY_COLL )    comm_group=buddy_data->buddy_coll.commgroup;
  if(flag& _SION_INTERNAL_FLAG_BUDDY_READ )    comm_group=buddy_data->groups[buddy_data->currentgroup]->commgroup;

  /* decide if file has to be opened physically  */
  do_open_file=1;
  if (flag&_SION_INTERNAL_FLAG_BUDDY_SEND)                 do_open_file=0; 
  if ( (flag&_SION_INTERNAL_FLAG_BUDDY_COLL) && (rank>0) ) do_open_file=0; 
  if ( (flag&_SION_INTERNAL_FLAG_BUDDY_READ) && (rank>0) ) do_open_file=0; 
  /* WF: todo decide if data is local, these tasks can read data directly? */
  DPRINTFP((2, "_sion_paropen_generic_one_file", rank, "do_open_file=%d\n", do_open_file));

  sion_filedesc = _sion_alloc_filedesc();
  if (sion_filedesc == NULL) {
    _sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_ABORT,"_sion_paropen_omp: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n",
		     (unsigned long) sizeof(sion_filedesc));
  }
  _sion_init_filedesc(sion_filedesc);
  sion_filedesc->fname       = strdup(fname); /* Set the filename */

  _sion_reassignvcd(sid,sion_filedesc, SION_FILEDESCRIPTOR);
  sion_filedesc->sid=sid;

  /* Allocate memory for storing MAXCHUNKS chunksize infos in internal structure */
  _sion_realloc_filedesc_blocklist(sion_filedesc, MAXCHUNKS);
  sion_filedesc->lastchunknr   = 0;                             /* Set the current number of chunks */
  sion_filedesc->currentblocknr = 0;                            /* Set the current block number */

  if (flags_store->mask&_SION_FMODE_WRITE) {
    /* **************** WRITE mode **************** */

    DPRINTFP((32, "_sion_paropen_generic_one_file", rank, " starting open for write #tasks=%d\n", ntasks));

    /* check parameter */
    if (ntasks<0) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen: wrong number of tasks specific: ntasks=%d (<0), returning ...\n", (int) ntasks));
    }

    /* check parameter */
    if ((chunksize != NULL) && (*chunksize<0)) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen: ((chunksize != NULL) && (*chunksize<0)), returning ...\n"));
    }

    /* check parameter */
    if ((flag & _SION_INTERNAL_FLAG_NORMAL ) && (globalrank != NULL) && (*globalrank<0)) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen: ((globalrank != NULL) && (*globalrank<0)), returning ...\n"));
    }
	    
    sion_filedesc->state      = SION_FILESTATE_PAROPEN;
    sion_filedesc->mode       = SION_FILEMODE_WRITE;
    sion_filedesc->endianness = _sion_get_endianness_with_flags(flags_store->mask); /* Endianness */
    sion_filedesc->swapbytes  = 0;                       /* Endianness, swapping bytes */
    sion_filedesc->fsblksize  = *fsblksize;
    sion_filedesc->rank       = rank;
    sion_filedesc->globalrank = *globalrank;
    sion_filedesc->ntasks     = ntasks;
    sion_filedesc->nfiles     = nfiles;
    sion_filedesc->filenumber = filenum;
    sion_filedesc->prefix     = strdup(prefix);
    sion_filedesc->compress   = flags_store->mask&_SION_FMODE_COMPRESS;
    sion_filedesc->usecoll       = (flags_store->mask&_SION_FMODE_COLLECTIVE)>0;
    sion_filedesc->collmergemode = (flags_store->mask&_SION_FMODE_COLLECTIVE_MERGE)>0;
    sion_filedesc->collmsa       = !!_sion_flags_get(flags_store, "collmsa");
    sion_filedesc->usebuddy      = (flags_store->mask&_SION_FMODE_BUDDY)>0;
    if(sion_filedesc->usebuddy) {
      sion_filedesc->buddylevel    = atoi(_sion_flags_get(flags_store,"buddy")->val);
      if (sion_filedesc->buddylevel==0) sion_filedesc->buddylevel=1; /* default */
    } 
	    
    /* open file on rank 0, first time to create file and get fsblksize if necessary */
    if (rank == 0) {
      sion_fileptr = _sion_file_open(fname,apiflag|SION_FILE_FLAG_WRITE|SION_FILE_FLAG_CREATE,0);
      if (!sion_fileptr) {
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_generic: cannot open %s for writing, aborting ...\n", fname));
      }
      if(*fsblksize<=-1) {
	/* check with fstat fsblksize */
	new_fsblocksize=(sion_int64) _sion_file_get_opt_blksize(sion_fileptr);
	if((new_fsblocksize<0) || (new_fsblocksize>SION_MAX_FSBLOCKSIZE)) new_fsblocksize=SION_DEFAULT_FSBLOCKSIZE;
      }
      _sion_file_close(sion_fileptr);
    }
    sion_gendata->apidesc->barrier_cb(comm_group);
    /* printf("WF: rank=%2d after first barrier fsblksize=%d\n",rank,(int) *fsblksize); */

    /* distribute new fsblksize */
    if(*fsblksize==-1) {
      sion_gendata->apidesc->bcastr_cb(&new_fsblocksize, comm_group, _SION_INT64, 1, 0);
      *fsblksize=new_fsblocksize;
      sion_filedesc->fsblksize = *fsblksize;
      /* printf("WF: rank=%2d after bcast fsblksize=%d new_fsblocksize=%d\n",rank,(int) *fsblksize,(int) new_fsblocksize); */
      DPRINTFP((32, "_sion_paropen_generic_one_file", rank, "setting fsblksize to %lld\n", new_fsblocksize));
    }

    /* check for buffer, needed at this point to set flag before writing header */
    _sion_cache_check_env(sion_filedesc);
    _sion_buffer_check_env(sion_filedesc);

    /* check for keyval parameter, needed at this point to set flag before writing header (flag1) */
    if (rank == 0) {
      _sion_keyval_check_env(sion_filedesc, flags_store->mask);
    }
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->keyvalmode, comm_group, _SION_INT32, 1, 0);

    /* check for collective options */
    if (rank == 0) {
      if ((flags_entry = _sion_flags_get(flags_store, "collsize"))) {
	sion_filedesc->collsize = atoi(flags_entry->val);
      }
      _sion_coll_check_env(sion_filedesc);
    }

    if ( 
	( flag&_SION_INTERNAL_FLAG_BUDDY_NORMAL )
	|| ( flag&_SION_INTERNAL_FLAG_BUDDY_SEND )
	|| ( flag&_SION_INTERNAL_FLAG_BUDDY_COLL )
	 )
      {
	/* overwrite collective info if not set*/
	if(!sion_filedesc->usecoll) {
	  sion_filedesc->usecoll=1;
	  sion_filedesc->collsize=sion_filedesc->ntasks;
	};
      }
    
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->usecoll,  comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->collsize, comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->collmergemode, comm_group, _SION_INT32, 1, 0);

    /* check if API support coalescing I/O */
    if(sion_filedesc->usecoll) {
      if(sion_gendata->apidesc->level!=SION_GENERIC_API_LEVEL_FULL) {
	_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_WARN,"sion_paropen_generic: requested coalescing I/O but API does not support this mode, falling back to individual mode ...\n");
	sion_filedesc->usecoll=0;
      }
    }

    /* check for hints options */
    if (rank == 0) {
      _sion_hints_check_env(sion_filedesc);
    }
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->usehints, comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->hinttype, comm_group, _SION_INT32, 1, 0);

    /*                                                                      */ DPRINTFTS(rank, "before alloc");
    if (rank == 0) {
      /* memory allocation for internal fields */
      _sion_alloc_filedesc_arrays(sion_filedesc);
      if(sion_filedesc->usecoll)  _sion_alloc_filedesc_coll_arrays(sion_filedesc);
    }
    /*                                                                      */ DPRINTFTS(rank, "after alloc");

    /* collect data and init startpointers on PE 0 */
    lchunksize  = (sion_int64) *chunksize;
    lglobalrank = (sion_int64) *globalrank;
    sion_filedesc->chunksize_req=lchunksize;
    DPRINTFP((32, "_sion_paropen_generic_one_file", rank, "lchunksize=%lld lglobalrank=%lld\n", lchunksize,lglobalrank));
	    
    /*                                                                      */ DPRINTFTS2(rank, "before gather");
    sion_gendata->apidesc->gatherr_cb(&lchunksize, sion_filedesc->all_chunksizes, comm_group, _SION_INT64, 1, 0);
    sion_gendata->apidesc->gatherr_cb(&lglobalrank, sion_filedesc->all_globalranks, comm_group, _SION_INT64, 1, 0);

    /* check capability of tasks */
    if(sion_filedesc->usecoll) {
      sion_filedesc->coll_capability=sion_gendata->apidesc->get_capability_cb(comm_group);
      sion_gendata->apidesc->gatherr_cb(&sion_filedesc->coll_capability, sion_filedesc->all_coll_capability, comm_group, _SION_INT32, 1, 0);
    }
	    
    /*                                                                      */ DPRINTFTS2(rank, "after gather");
    if (rank == 0) {
      /*                                                                      */ DPRINTFTS(rank, "before calculate");
      DPRINTFP((32, "_sion_paropen_generic_one_file", rank, "chunksizes[%d - 1]=%ld\n", ntasks,(long) sion_filedesc->all_chunksizes[ntasks - 1]));
      if (!sion_filedesc->usecoll) _sion_calculate_startpointers(sion_filedesc);
      else {
	if (sion_filedesc->collmergemode) _sion_calculate_startpointers_collective_merge(sion_filedesc);
        else if (sion_filedesc->collmsa) _sion_calculate_startpointers_collective_msa(sion_filedesc);
	else                               _sion_calculate_startpointers_collective(sion_filedesc);
      }
      /*                                                                      */ DPRINTFTS(rank, "after calculate");
    }

    /*                                                                      */ DPRINTFTS(rank, "before open");
    /* open not file on non-collector task if buddy-checkpointing */
    if(do_open_file) {
	    
      /* open file on all ranks */
      sion_fileptr = _sion_file_open(fname,apiflag|SION_FILE_FLAG_WRITE,0);
      if (!sion_fileptr) {
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_generic: cannot open %s for writing, aborting ...\n", fname));
      }
      /* store data in static data structure (sid)  */
      sion_filedesc->fileptr = sion_fileptr;
    }
    sion_gendata->apidesc->barrier_cb(comm_group);
    /*                                                                      */ DPRINTFTS(rank, "after open");
	    	      

    /* write header */
    if (rank == 0) {

      /* apply hint for first meta data block */
      _sion_apply_hints(sion_filedesc,SION_HINTS_ACCESS_TYPE_METADATABLOCK1);

      /*                                                                      */ DPRINTFTS(rank, "before writeh");
      _sion_write_header(sion_filedesc);
      /*                                                                      */ DPRINTFTS(rank, "after writeh");

      /* needed for writing pointer to var part of metadata at the end of the file */
      sion_filedesc->end_of_header = _sion_file_get_position(sion_filedesc->fileptr);
      sion_filedesc->start_of_data = sion_filedesc->all_startpointers[0];
      /*set max. file size */
      lstartpointer =  sion_filedesc->all_startpointers[ntasks - 1]
	+ sion_filedesc->all_chunksizes[ntasks - 1];
      /*                                                                      */ DPRINTFTS(rank, "before setp(0)");
      _sion_file_flush(sion_filedesc->fileptr);
      _sion_file_set_position(sion_filedesc->fileptr, lstartpointer);
      /*                                                                      */ DPRINTFTS(rank, "after setp(0)");

    }

    /* distribute start_pos */
    /*                                                                        */ DPRINTFTS(rank, "before scatter");
    sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_startpointers, &sion_filedesc->startpos, comm_group, _SION_INT64, 1, 0);
    /*                                                                        */ DPRINTFTS(rank, "after scatter");

    /* distribute chunksize */
    /*                                                                        */ DPRINTFTS(rank, "before scatter");
    sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_chunksizes, &sion_filedesc->chunksize, comm_group, _SION_INT64, 1, 0);
    /*                                                                        */ DPRINTFTS(rank, "after scatter");

    /* distribute information for collective operations */
    if(sion_filedesc->usecoll) {
      sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_coll_collsize, &sion_filedesc->collsize, comm_group, _SION_INT32, 1, 0);
      sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_coll_collector, &sion_filedesc->collector, comm_group, _SION_INT32, 1, 0);

      _sion_free_filedesc_coll_arrays(sion_filedesc);
    }

    /* distribute globalskip */
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->globalskip, comm_group, _SION_INT64, 1, 0);

    DPRINTFP((32, "_sion_paropen_generic_one_file", rank, " start position is %10lld %10.4f MB chunksize=%10lld %10.4f MB\n", 
	      sion_filedesc->startpos, sion_filedesc->startpos / 1024.0 / 1024.0,
	      sion_filedesc->chunksize, sion_filedesc->chunksize / 1024.0 / 1024.0
	      ));

    /* set filepointer on each task */
    /*                                                                      */ DPRINTFTS(rank, "before setp");
    sion_gendata->apidesc->barrier_cb(comm_group);
    if(do_open_file) {
      _sion_file_flush(sion_filedesc->fileptr);
      _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->startpos);
    }
    sion_filedesc->currentpos = sion_filedesc->startpos;
    /* given by calculate startpointers ... 
       sion_filedesc->chunksize = (sion_int64) *chunksize;
    */
    sion_gendata->apidesc->barrier_cb(comm_group);

    /* apply hint for first chunk */
    _sion_apply_hints(sion_filedesc,SION_HINTS_ACCESS_TYPE_CHUNK);

    /*                                                                      */ DPRINTFTS(rank, "after setp");
    DPRINTFP((32, "_sion_paropen_generic_one_file", rank, " ending open for write #tasks=%d filepos=%lld\n", ntasks, _sion_file_get_position(sion_filedesc->fileptr)));

  }
  else if (flags_store->mask&_SION_FMODE_READ) {
    /* **************** READ mode **************** */
    if (rank == 0)
      DPRINTFP((32, "_sion_paropen_generic_one_file", rank, " starting open for read #tasks=%d\n", ntasks));

    /* open not file on non-collector task if buddy-checkpointing */
    if(do_open_file) {
      /*                                                                      */ DPRINTFTS(rank, "before openR");
      sion_fileptr = _sion_file_open(fname,apiflag|SION_FILE_FLAG_READ,0);
      /*                                                                      */ DPRINTFTS(rank, "after openR");
      if (!sion_fileptr) {
	DPRINTFP((32, "_sion_paropen_generic_one_file", rank, " cannot open %s for reading, aborting ...\n", fname));
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_one_file: cannot open %s for reading, aborting ...\n", fname));
      }
    } else {
      sion_fileptr=NULL;
    }
    sion_gendata->apidesc->barrier_cb(comm_group);

    /* store data in static data structure (sid)  */
    sion_filedesc->fileptr = sion_fileptr;
    sion_filedesc->rank   = rank;
    sion_filedesc->ntasks = ntasks;
    sion_filedesc->state  = SION_FILESTATE_PAROPEN;
    sion_filedesc->mode   = SION_FILEMODE_READ;
    sion_filedesc->nfiles = nfiles;
    sion_filedesc->collmsa       = !!_sion_flags_get(flags_store, "collmsa");
    sion_filedesc->usebuddy      = (flags_store->mask&_SION_FMODE_BUDDY)>0;
    if(sion_filedesc->usebuddy) {
      sion_filedesc->buddylevel    = atoi(_sion_flags_get(flags_store,"buddy")->val);
      if (sion_filedesc->buddylevel==0) sion_filedesc->buddylevel=1; /* default */
    } 

    /* creating of mapping from file ranks to rank used in buddy read */
    if ( flag&_SION_INTERNAL_FLAG_BUDDY_READ ) {

      /* overwrite collective info if not set*/
      if(!sion_filedesc->usecoll) {
	sion_filedesc->usecoll=1;
	sion_filedesc->collsize=sion_filedesc->ntasks;
      }

      DPRINTFP((32, "_sion_paropen_generic_one_file", rank, " create buddy mapping ntasks=%d filentasks=%d\n",ntasks,sion_filedesc->ntasks));

      if (rank == 0) {
	sion_tmpintfield_buddy32 = (sion_int32 *) malloc(ntasks * sizeof(sion_int32));
	if (sion_tmpintfield_buddy32 == NULL) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_one_file: cannot allocate temporary memory of size %lu (sion_tmpintfield_buddy), aborting ...\n",
				  (unsigned long) ntasks * sizeof(sion_int32)));
	}
	for (j = 0; j < ntasks; j++)  sion_tmpintfield_buddy32[j]=-1;
	sion_tmpintfield_buddy64 = (sion_int64 *) malloc(ntasks * sizeof(sion_int64));
	if (sion_tmpintfield_buddy64 == NULL) {
          free(sion_tmpintfield_buddy32);
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_one_file: cannot allocate temporary memory of size %lu (sion_tmpintfield_buddy), aborting ...\n",
				  (unsigned long) ntasks * sizeof(sion_int64)));
	}
	for (j = 0; j < ntasks; j++)  sion_tmpintfield_buddy64[j]=-1;
	sion_tmpintfield_map = (sion_int32 *) malloc(ntasks * sizeof(sion_int32));
	if (sion_tmpintfield_map == NULL) {
          free(sion_tmpintfield_buddy32);
          free(sion_tmpintfield_buddy64);
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_one_file: cannot allocate temporary memory of size %lu (sion_tmpintfield_map), aborting ...\n",
				  (unsigned long) ntasks * sizeof(sion_int32)));
	}
	for (j = 0; j < ntasks; j++)  sion_tmpintfield_map[j]=-1;

      }
      helpint32=buddy_data->groups[buddy_data->currentgroup]->filelrank;
      sion_gendata->apidesc->gatherr_cb(&helpint32, sion_tmpintfield_map, comm_group, _SION_INT32, 1, 0);

      
      if (rank == 0) {
	for (j = 0; j < ntasks; j++)
	  DPRINTFP((64, "_sion_paropen_generic_one_file", rank, " buddy map[%d]=%d\n", j, (int) sion_tmpintfield_map[j]));
      }
      
    }

    if (rank == 0) {
      rc = _sion_read_header_fix_part(sion_filedesc); /* overwrites sion_filedesc->ntasks */
      if (rc!=SION_SUCCESS) {
        free(sion_tmpintfield_buddy32);
        free(sion_tmpintfield_buddy64);
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_one_file: cannot read header from file %s, aborting ...\n", fname));
      }
      DPRINTFP((32, "_sion_paropen_generic_one_file", rank,
		" read, after read of fix header part endianness=0x%x blksize=%d ntasks=%d\n", sion_filedesc->endianness, sion_filedesc->fsblksize, sion_filedesc->ntasks));

      /*                                                                      */ DPRINTFTS(rank, "before alloc");
      /* memory allocation */
      _sion_alloc_filedesc_arrays(sion_filedesc);
      /*                                                                      */ DPRINTFTS(rank, "after alloc");

      rc = _sion_read_header_var_part(sion_filedesc);
      if (rc!=SION_SUCCESS) {
        free(sion_tmpintfield_buddy32);
        free(sion_tmpintfield_buddy64);
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_one_file: cannot read header from file %s, aborting ...\n", fname));
      }
	      
      if ((flags_entry = _sion_flags_get(flags_store, "collsize"))) {
	sion_filedesc->collsize = atoi(flags_entry->val);
      }
      _sion_coll_check_env(sion_filedesc);
      if(sion_filedesc->usecoll)  _sion_alloc_filedesc_coll_arrays(sion_filedesc);
	      
      /* collective */
      if (!sion_filedesc->usecoll) _sion_calculate_startpointers(sion_filedesc);
      else {
	if (sion_filedesc->collmergemode) _sion_calculate_startpointers_collective_merge(sion_filedesc);
        else if (sion_filedesc->collmsa) _sion_calculate_startpointers_collective_msa(sion_filedesc);
	else                               _sion_calculate_startpointers_collective(sion_filedesc);
      }
      /*                                                                      */ DPRINTFTS(rank, "after calculate");
	      
      /* check for keyval parameter, needed at this point to set flag before writing header (flag1) */
      _sion_keyval_check_env(sion_filedesc, flags_store->mask);
	      
    } /* rank==0 */

    /* distribute keyvalmode */
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->keyvalmode, comm_group, _SION_INT32, 1, 0);

    /* distribute collective options */
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->usecoll, comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->collsize, comm_group, _SION_INT32, 1, 0);


    DPRINTFP((32, "_sion_paropen_generic_one_file", rank," usecoll=%d\n", sion_filedesc->usecoll));

    if(sion_filedesc->usecoll) {
	      
      if (! (flag&_SION_INTERNAL_FLAG_BUDDY_READ) ) {
	sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_coll_collsize, &sion_filedesc->collsize, comm_group, _SION_INT32, 1, 0);
	sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_coll_collector, &sion_filedesc->collector, comm_group, _SION_INT32, 1, 0);
      } else {

	/* remap data */
	if(rank==0) for (j = 0; j < ntasks; j++) if(sion_tmpintfield_map[j]>=0) sion_tmpintfield_buddy32[j]=sion_filedesc->all_coll_collsize[sion_tmpintfield_map[j]];
	sion_gendata->apidesc->scatterr_cb(sion_tmpintfield_buddy32, &sion_filedesc->collsize, comm_group, _SION_INT32, 1, 0);
	/* remap data */
	if(rank==0) for (j = 0; j < ntasks; j++) if(sion_tmpintfield_map[j]>=0) sion_tmpintfield_buddy32[j]=sion_filedesc->all_coll_collector[sion_tmpintfield_map[j]];
	sion_gendata->apidesc->scatterr_cb(sion_tmpintfield_buddy32, &sion_filedesc->collector, comm_group, _SION_INT32, 1, 0);
      }


      _sion_free_filedesc_coll_arrays(sion_filedesc);
    }

    /* distribute globalskip */
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->globalskip, comm_group, _SION_INT64, 1, 0);

    /* broadcast information read from file */
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->endianness,  comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->swapbytes,   comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->fsblksize,   comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->ntasks,      comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->fileversion, comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->nfiles,      comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->filenumber,  comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->flag1,       comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->flag2,       comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->maxusedchunks, comm_group, _SION_INT32, 1, 0);

    DPRINTFP((32, "_sion_paropen_generic_one_file", rank,
	      " read, after read of maxusedchunks=%d maxchunks=%d (%d)\n", sion_filedesc->maxusedchunks,sion_filedesc->maxchunks, MAXCHUNKS));
    if (sion_filedesc->maxusedchunks > MAXCHUNKS)  _sion_realloc_filedesc_blocklist(sion_filedesc, sion_filedesc->maxusedchunks);
    /*                                                                      */ DPRINTFTS(rank, "after bcast");

    /* scatter per task information read from file */
    /*                                                                      */ DPRINTFTS(rank, "before scatter");
    if (! (flag&_SION_INTERNAL_FLAG_BUDDY_READ) ) {
      sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_chunksizes, &sion_filedesc->chunksize, comm_group, _SION_INT64, 1, 0);
      sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_startpointers, &sion_filedesc->startpos, comm_group, _SION_INT64, 1, 0);
      sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_globalranks, &helpint64, comm_group, _SION_INT64, 1, 0);sion_filedesc->globalrank=(sion_int32) helpint64;
    } else {
	/* remap data */
      if(rank==0) for (j = 0; j < ntasks; j++) if(sion_tmpintfield_map[j]>=0) sion_tmpintfield_buddy64[j]=sion_filedesc->all_chunksizes[sion_tmpintfield_map[j]];
      sion_gendata->apidesc->scatterr_cb(sion_tmpintfield_buddy64, &sion_filedesc->chunksize, comm_group, _SION_INT64, 1, 0);
	/* remap data */
      if(rank==0) for (j = 0; j < ntasks; j++) if(sion_tmpintfield_map[j]>=0) sion_tmpintfield_buddy64[j]=sion_filedesc->all_startpointers[sion_tmpintfield_map[j]];
      sion_gendata->apidesc->scatterr_cb(sion_tmpintfield_buddy64, &sion_filedesc->startpos, comm_group, _SION_INT64, 1, 0);
      /* remap data */
      if(rank==0) for (j = 0; j < ntasks; j++) if(sion_tmpintfield_map[j]>=0) sion_tmpintfield_buddy64[j]=sion_filedesc->all_globalranks[sion_tmpintfield_map[j]];
      sion_gendata->apidesc->scatterr_cb(sion_tmpintfield_buddy64, &helpint64, comm_group, _SION_INT64, 1, 0);sion_filedesc->globalrank=(sion_int32) helpint64;
    }

    /*                                                                      */ DPRINTFTS(rank, "after scatter");

    /* read number of blocks for each task */
    if (rank == 0) {
      sion_tmpintfield = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
      if (sion_tmpintfield == NULL) {
        free(sion_tmpintfield_buddy32);
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_generic_one_file: cannot allocate temporary memory of size %lu (sion_tmpintfield), aborting ...\n",
				(unsigned long) ntasks * sizeof(sion_int64)));
      }
      _sion_read_header_var_part_blockcount_to_field(sion_filedesc, sion_filedesc->ntasks, sion_tmpintfield);

      for (j = 0; j < sion_filedesc->ntasks; j++)
	DPRINTFP((2048, "_sion_paropen_generic_one_file", rank, " read, blockcount on task %02d is %10ld\n", j, (long) sion_tmpintfield[j]));
    }

    /* and distribute them */
    /*                                                                      */ DPRINTFTS(rank, "before scatter");
    if (! (flag&_SION_INTERNAL_FLAG_BUDDY_READ) ) {
      sion_gendata->apidesc->scatterr_cb(sion_tmpintfield, &helpint64, comm_group, _SION_INT64, 1, 0);
    } else {
      /* remap data */
      if(rank==0) for (j = 0; j < ntasks; j++) if(sion_tmpintfield_map[j]>=0) sion_tmpintfield_buddy64[j]=sion_tmpintfield[sion_tmpintfield_map[j]];
      sion_gendata->apidesc->scatterr_cb(sion_tmpintfield_buddy64, &helpint64, comm_group, _SION_INT64, 1, 0);
    }
    /*                                                                      */ DPRINTFTS(rank, "after scatter");
    sion_filedesc->lastchunknr = helpint64-1;
    DPRINTFP((32, "_sion_paropen_generic_one_file", rank, "  lastchunknr on task %02d is %10ld\n", rank, (long) sion_filedesc->lastchunknr));

    for (i = 0; i < sion_filedesc->maxusedchunks; i++) {
      if (rank == 0) _sion_read_header_var_part_nextblocksizes_to_field(sion_filedesc, sion_filedesc->ntasks, sion_tmpintfield);
      /*                                                                      */ DPRINTFTS(rank, "before scatter");
      if (! (flag&_SION_INTERNAL_FLAG_BUDDY_READ) ) {
	sion_gendata->apidesc->scatterr_cb(sion_tmpintfield, &helpint64, comm_group, _SION_INT64, 1, 0);
      } else {
	/* remap data */
	if(rank==0) for (j = 0; j < ntasks; j++) if(sion_tmpintfield_map[j]>=0) sion_tmpintfield_buddy64[j]=sion_tmpintfield[sion_tmpintfield_map[j]];
	sion_gendata->apidesc->scatterr_cb(sion_tmpintfield_buddy64, &helpint64, comm_group, _SION_INT64, 1, 0);
      }
      /*                                                                      */ DPRINTFTS(rank, "after scatter");
      sion_filedesc->blocksizes[i] = helpint64;
    }

#define BGFLUSH
#ifdef BGFLUSH
    if(do_open_file) {
      _sion_file_flush(sion_filedesc->fileptr);
    }
#endif

    /*                                                                      */ DPRINTFTS(rank, "before setp");
    sion_gendata->apidesc->barrier_cb(comm_group);
    if(do_open_file) {
      _sion_file_purge(sion_filedesc->fileptr);
      _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->startpos);
    }
    sion_filedesc->currentpos     = sion_filedesc->startpos;
    sion_filedesc->currentblocknr = 0;

    /* OUTPUT parameters */
    *fsblksize  = sion_filedesc->fsblksize;
    *chunksize  = sion_filedesc->chunksize;
    *globalrank = sion_filedesc->globalrank;

    /* free tmp field */
    if(sion_tmpintfield) free(sion_tmpintfield);
    if(sion_tmpintfield_map) free(sion_tmpintfield_map);
    if(sion_tmpintfield_buddy32) free(sion_tmpintfield_buddy32);
    if(sion_tmpintfield_buddy64) free(sion_tmpintfield_buddy64);

    sion_gendata->apidesc->barrier_cb(comm_group);
    /*                                                                      */ DPRINTFTS(rank, "after setp");
    /* end of read */
  } else {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_multi_mpi: unknown file mode"));
  }

  if(do_open_file) { 
    if(fileptr!=NULL) {
      if(sion_filedesc->fileptr->flags&SION_FILE_FLAG_ANSI) {
	*fileptr=sion_filedesc->fileptr->fileptr;
	sion_filedesc->fileptr_exported=1;
      } else {
	*fileptr=NULL;
	sion_filedesc->fileptr_exported=0;
      }
    }
  } else {
    if(fileptr!=NULL) *fileptr=NULL;
    sion_filedesc->fileptr_exported=0;
  }

  if (rank == 0) {
    /* not needed for rest of sionlib function calls  */
    _sion_free_filedesc_arrays(sion_filedesc);
  }

  _sion_print_filedesc(sion_filedesc, 512, "_sion_paropen_generic_one_file", 1);

  DPRINTFP((32, "_sion_paropen_generic_one_file", rank, " start position on task %02d is at end of sion_paropen_generic %10lld\n", rank,
	    _sion_file_get_position(sion_filedesc->fileptr)));

  DPRINTFP((2, "_sion_paropen_generic_one_file", rank, "leave parallel open of file %s in mode 0x%lx #tasks=%d\n", fname, (long) flags_store->mask, ntasks));

  return (sid);


}



/*!\brief Internal function to close parallel opened SION file
 *
 * @param  sid  reference to file description struct (_sion_filedesc)
 * @param  rank  rank
 * @param  ntasks  number of tasks
 * @param  mapping_size  number of global tasks
 * @param  mapping  mapping
 * @param  flag          flag describing the role if the process in buddy
 *                       checkpointing
 * @param  sion_gendata  generic data struct
 * @param  buddy_data    data describing the buddy checkpointing
 *
 * @return  SION_SUCCESS if successful
 */
int _sion_parclose_generic(int sid,
			   int rank,
			   int ntasks,
			   int mapping_size,
			   sion_int32 *mapping,
			   int    flag,
			   _sion_generic_gendata *sion_gendata,
			   _sion_generic_buddy   *buddy_data)
{

  int       rc = SION_SUCCESS;
  int       blknum, lrank;
  sion_int64 helpint64;
  sion_int64 *sion_tmpintfield = NULL;
  _sion_filedesc *sion_filedesc;
  void *comm_group=NULL; 
  int do_close_file;

  if ((_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: invalid sion_filedesc, aborting %d ...\n", sid));
  }

  if (sion_filedesc->state != SION_FILESTATE_PAROPEN) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: sion file with sid=%d was not opened by a sion_paropen\n", sid));
  }

  DPRINTFP((2, "_sion_parclose_generic", rank, "enter parallel close  sid=%d\n", sid));

  /* select local communicator */
  if(flag& _SION_INTERNAL_FLAG_NORMAL )        comm_group=sion_gendata->comm_data_local;
  if(flag& _SION_INTERNAL_FLAG_BUDDY_SEND )    comm_group=buddy_data->buddy_send.commgroup;
  if(flag& _SION_INTERNAL_FLAG_BUDDY_COLL )    comm_group=buddy_data->buddy_coll.commgroup;
  if(flag& _SION_INTERNAL_FLAG_BUDDY_READ )    comm_group=buddy_data->groups[buddy_data->currentgroup]->commgroup;

  /* decide if file has to be opened physically  */
  do_close_file=1;
  if (flag&_SION_INTERNAL_FLAG_BUDDY_SEND)                 do_close_file=0; 
  if ( (flag&_SION_INTERNAL_FLAG_BUDDY_COLL) && (rank>0) ) do_close_file=0; 
  if ( (flag&_SION_INTERNAL_FLAG_BUDDY_READ) && (rank>0) ) do_close_file=0; 

  /* READ MODE: close file on all tasks  */
  if (sion_filedesc->mode == SION_FILEMODE_READ) {
    if (sion_filedesc->state != SION_FILESTATE_CLOSE) {

      _sion_print_filedesc(sion_filedesc, 512, "_sion_parclose_generic", 1);
      DPRINTFP((32, "_sion_parclose_generic", rank, " parallel close (read mode)  sid=%d, call fclose on file\n", sid));

      if(do_close_file) {
	_sion_file_close(sion_filedesc->fileptr);
      }
      sion_filedesc->fileptr = NULL;
      sion_filedesc->state = SION_FILESTATE_CLOSE;
    }
  }
  else {
    /* WRITE MODE: collect data from all tasks, write metadata, and close file on all tasks  */

    /* _sion_buffer_flush(sion_filedesc); */ /* clear internal buffer */
    _sion_flush_block(sion_filedesc);

    if (sion_filedesc->usebuffer) {
      _sion_buffer_flush(sion_filedesc);
    }

    _sion_print_filedesc(sion_filedesc, 512, "_sion_parclose_generic", 1);

    /* close file on all other task, except 0  */
    if (rank != 0) {
      if (sion_filedesc->state != SION_FILESTATE_CLOSE) {
        DPRINTFP((32, "_sion_parclose_generic", rank, " parallel close (write mode)  sid=%d, call fclose on file\n", sid));
	if(do_close_file) {
	  _sion_file_close(sion_filedesc->fileptr);
	}
        sion_filedesc->fileptr = NULL;
        sion_filedesc->state = SION_FILESTATE_CLOSE;
      }
    }

    sion_gendata->apidesc->barrier_cb(comm_group);

    DPRINTFP((32, "_sion_parclose_generic", rank, " parallel close sid=%d: lastchunknr=%d globalskip=%lld\n", sid, sion_filedesc->lastchunknr,
              sion_filedesc->globalskip));
    for (blknum = 0; blknum <= sion_filedesc->lastchunknr; blknum++) {
      DPRINTFP((1024, "_sion_parclose_generic", rank, " parallel close sid=%d: local block %02d -> %10lld bytes\n", sid, blknum,
                sion_filedesc->blocksizes[blknum]));
    }

    if (rank == 0) {
      sion_tmpintfield = (sion_int64 *) malloc(sion_filedesc->ntasks * sizeof(sion_int64));
      if (sion_tmpintfield == NULL) {
	return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield), aborting ...\n",
				(unsigned long) sion_filedesc->ntasks * sizeof(sion_int64)));
      }
    }

    /* gather number of blocks of each tasks, and search maxusedchunks */
    /*                                                                      */ DPRINTFTS2(rank, "before gather");
    helpint64 = sion_filedesc->lastchunknr + 1;
    sion_gendata->apidesc->gatherr_cb(&helpint64, sion_tmpintfield, comm_group, _SION_INT64, 1, 0);

    if (rank == 0) {
      sion_filedesc->maxusedchunks = -1;
      for (blknum = 0; blknum < sion_filedesc->ntasks; blknum++)
	if (sion_tmpintfield[blknum] > sion_filedesc->maxusedchunks)
	  sion_filedesc->maxusedchunks = (int) sion_tmpintfield[blknum];
    }
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc->maxusedchunks, comm_group, _SION_INT32, 1, 0);
    /*                                                                      */ DPRINTFTS2(rank, "after gather");

    /* calculate and set start_of_varheader */
    sion_filedesc->start_of_varheader = sion_filedesc->start_of_data + sion_filedesc->maxusedchunks * sion_filedesc->globalskip;

    /* write rest of first meta data block on rank 0 */
    if (rank == 0) {
      _sion_write_header_var_info(sion_filedesc);

      _sion_write_header_var_part_blockcount_from_field(sion_filedesc,sion_filedesc->ntasks,sion_tmpintfield);

    }

    /* collect chunksizes of each block from each task and write it to file */
    for (blknum = 0; blknum < sion_filedesc->maxusedchunks; blknum++) {
      if (blknum <= sion_filedesc->lastchunknr) {
        helpint64 = sion_filedesc->blocksizes[blknum];
      }
      else {
        helpint64 = 0;
      }

      /*                                                                      */ DPRINTFTS2(rank, "before gather");
      sion_gendata->apidesc->gatherr_cb(&helpint64, sion_tmpintfield, comm_group, _SION_INT64, 1, 0);
      /*                                                                      */ DPRINTFTS2(rank, "after gather");

      if (rank == 0) {
        for (lrank = 0; lrank < ntasks; lrank++)
          DPRINTFP((2048, "_sion_parclose_generic", rank, " parallel close sid=%d: write total chunksize for block %d: %2lld rank=%d\n", sid, blknum,
                    sion_tmpintfield[lrank], lrank));

	_sion_write_header_var_part_nextblocksizes_from_field(sion_filedesc,sion_filedesc->ntasks,sion_tmpintfield);

      }
    }

    /* write mapping to file if more than one physical file is used, mapping_size is the number of global tasks */
    if (mapping != NULL) {
      _sion_write_header_var_part_mapping(sion_filedesc, mapping_size, mapping);
    }

    /* close file on task 0  */
    if (rank == 0) {
      DPRINTFP((32, "_sion_parclose_generic", rank, " parallel close (write mode)  sid=%d, call fclose on file\n", sid));
      if(do_close_file) {
	_sion_file_close(sion_filedesc->fileptr);
      }
      sion_filedesc->fileptr = NULL;
      sion_filedesc->state = SION_FILESTATE_CLOSE;

      /* free tmp field */
      if(sion_tmpintfield) free(sion_tmpintfield);
    }

  }                             /* write */

  _sion_free_filedesc(sion_filedesc);
  sion_filedesc = NULL;


  DPRINTFP((2, "_sion_parclose_generic", rank, "leave parallel close  sid=%d\n", sid));

  return (rc);
}

/*!\brief change chunksize for an already opened SION file (write)
 *
 * @return 1 if successful
 */
int _sion_parreinit_generic(
			    int sid,
			    sion_int64 chunksize,
			    int rank,
			    int ntasks,
			    _sion_generic_gendata *sion_gendata)
{

  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;
  sion_int64 lchunksize, lstartpointer, lglobalrank;
  void *comm_group=NULL; 

  if ((_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parreinit_generic: invalid sion_filedesc, aborting %d ...\n", sid));
  }
	  
  if (sion_filedesc->state != SION_FILESTATE_PAROPEN) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parreinit_generic: sion file with sid=%d was not opened by a sion_paropen\n", sid));
  }
	  
  DPRINTFP((2, "_sion_parreinit_generic", sion_filedesc->rank, "enter parallel reinit  sid=%d\n", sid));
  
  comm_group=sion_gendata->comm_data_local;

  if (sion_filedesc->mode == SION_FILEMODE_READ) {
        return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parreinit_generic: sion file with sid=%d only allowed for files openend for write\n", sid));
  }

  /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "before alloc");
  if (sion_filedesc->rank == 0) {
    /* memory allocation for internal fields */
    _sion_alloc_filedesc_arrays(sion_filedesc);
  }
  /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "after alloc");

  /* collect new chunksize data and init startpointers on PE 0 */
  lchunksize  = (sion_int64) chunksize;
  lglobalrank = (sion_int64) sion_filedesc->globalrank;
 
  /*                                                                      */ DPRINTFTS2(sion_filedesc->rank, "before gather");
  sion_gendata->apidesc->gatherr_cb(&lchunksize, sion_filedesc->all_chunksizes, comm_group, _SION_INT64, 1, 0);
  sion_gendata->apidesc->gatherr_cb(&lglobalrank, sion_filedesc->all_globalranks, comm_group, _SION_INT64, 1, 0);

  /*                                                                      */ DPRINTFTS2(sion_filedesc->rank, "after gather");

  if(sion_filedesc->usecoll)  _sion_alloc_filedesc_coll_arrays(sion_filedesc);

  if (sion_filedesc->rank == 0) {
    /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "before calculate");
    if (!sion_filedesc->usecoll) _sion_calculate_startpointers(sion_filedesc);
    else                         _sion_calculate_startpointers_collective(sion_filedesc);
    /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "after calculate");
  }

  /* write header again */
  if (sion_filedesc->rank == 0) {
    
    /* apply hint for first meta data block */
    _sion_apply_hints(sion_filedesc,SION_HINTS_ACCESS_TYPE_METADATABLOCK1);

    _sion_file_flush(sion_filedesc->fileptr);
    lstartpointer=0;
    _sion_file_set_position(sion_filedesc->fileptr, lstartpointer);
    
    /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "before writeh");
    _sion_write_header(sion_filedesc);
    /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "after writeh");
    
    /* needed for writing pointer to var part of metadata at the end of the file */
    sion_filedesc->end_of_header = _sion_file_get_position(sion_filedesc->fileptr);
    sion_filedesc->start_of_data = sion_filedesc->all_startpointers[0];

    /*set max. file size */
    lstartpointer =  sion_filedesc->all_startpointers[sion_filedesc->ntasks - 1]
      + sion_filedesc->all_chunksizes[sion_filedesc->ntasks - 1];
    /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "before setp(0)");
    _sion_file_flush(sion_filedesc->fileptr);
    _sion_file_set_position(sion_filedesc->fileptr, lstartpointer);
    /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "after setp(0)");
    
  }

  /* distribute start_pos */
  /*                                                                        */ DPRINTFTS(sion_filedesc->rank, "before scatter");
  sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_startpointers, &sion_filedesc->startpos, comm_group, _SION_INT64, 1, 0);
  /*                                                                        */ DPRINTFTS(sion_filedesc->rank, "after scatter");

  /* distribute information for collective operations */
  if(sion_filedesc->usecoll) {
    sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_coll_collsize, &sion_filedesc->collsize, comm_group, _SION_INT32, 1, 0);
    sion_gendata->apidesc->scatterr_cb(sion_filedesc->all_coll_collector, &sion_filedesc->collector, comm_group, _SION_INT32, 1, 0);
    
    _sion_free_filedesc_coll_arrays(sion_filedesc);
  }

  /* distribute globalskip */
  sion_gendata->apidesc->bcastr_cb(&sion_filedesc->globalskip, comm_group, _SION_INT64, 1, 0);

  DPRINTFP((32, "_sion_parreinit_generic", sion_filedesc->rank, " start position is %10lld %10.4f MB\n", 
	    sion_filedesc->startpos, sion_filedesc->startpos / 1024.0 / 1024.0));

  /* set filepointer on each task */
  /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "before setp");
  sion_gendata->apidesc->barrier_cb(comm_group);
  _sion_file_flush(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->startpos);
  sion_filedesc->currentpos = sion_filedesc->startpos;
  sion_filedesc->chunksize = (sion_int64) chunksize;
  sion_gendata->apidesc->barrier_cb(comm_group);
  
  /* apply hint for first chunk */
  _sion_apply_hints(sion_filedesc,SION_HINTS_ACCESS_TYPE_CHUNK);

  if (sion_filedesc->rank == 0) {
    /* not needed for rest of sionlib function calls  */
    _sion_free_filedesc_arrays(sion_filedesc);
  }
  
  /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "after setp");
  DPRINTFP((32, "_sion_parreinit_generic", sion_filedesc->rank, " ending open for write #tasks=%d filepos=%lld\n", 
	    sion_filedesc->ntasks, _sion_file_get_position(sion_filedesc->fileptr)));

  DPRINTFP((2, "_sion_parreinit_generic", sion_filedesc->rank, "leave parallel reinit of file %s in  #tasks=%d\n", 
	    sion_filedesc->fname, sion_filedesc->ntasks));

  return (rc);


}


/*!\brief collect mapping information on rank 0 of first file, mapping=NULL for all others
 *
 * @return 1 if successful
 */
#define DFUNCTION "_sion_generic_collect_mapping"
int _sion_generic_collect_mapping( _sion_filedesc *sion_filedesc,
				   int            *mapping_size,
				   sion_int32    **mapping ) {
    int rc=SION_SUCCESS;
    int t;
    _sion_generic_gendata *sion_gendata;
    _sion_generic_apidesc *sion_apidesc;
    sion_int32 lpos[2], *receivemap=NULL, iamreceiver, receiver = -1;


    sion_gendata=sion_filedesc->dataptr;
    sion_apidesc=sion_gendata->apidesc;

    *mapping = NULL;  *mapping_size = 0;
 
    if ((sion_filedesc->mode == SION_FILEMODE_WRITE) && (sion_filedesc->nfiles > 1)) {
      /* collect mapping to files to task 0 */
      
      /* mapping data will be collected by master of first physical file */
      if((sion_filedesc->filenumber==0) && (sion_filedesc->rank==0)) {
	/* allocate data */
	*mapping_size=sion_gendata->gsize;
	*mapping = (sion_int32 *) malloc(*mapping_size * 2 * sizeof(sion_int32));
	if (*mapping == NULL) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_parclose: Cannot allocate memory for mapping"));
	}
      } 
      
      /* gather info about send about global rank of master of first file on grank 0 */
      if(sion_gendata->grank==0) {
	receivemap = (sion_int32 *) malloc(sion_gendata->gsize * sizeof(sion_int32));
	if (receivemap == NULL) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_parclose: Cannot allocate memory for receivemap"));
	}
      }
    
      if((sion_filedesc->filenumber==0) && (sion_filedesc->rank==0)) iamreceiver=sion_gendata->grank;
      else                                             iamreceiver=-1;
      sion_apidesc->gatherr_cb(&iamreceiver, receivemap, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
      if(sion_gendata->grank==0) {
      for(t=0;t<sion_gendata->gsize;t++) {
	if(receivemap[t]>=0) {
	  receiver=receivemap[t];
	  break;
	}
      }
      DPRINTFP((1, DFUNCTION, sion_gendata->grank, "receiver of mapping grank=%d\n", receiver));
    }
      sion_apidesc->bcastr_cb(&receiver, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
      
      /* receive global rank of master of first file on grank 0 */
      lpos[0] = sion_filedesc->filenumber;
      lpos[1] = sion_filedesc->rank;
      sion_apidesc->gatherr_cb(&lpos, *mapping, sion_gendata->comm_data_global, _SION_INT32, 2, receiver);
    }

    if(receivemap!=NULL) free(receivemap);
    
    return(rc);
}
#undef DFUNCTION

/*  END OF _sion_parclose_generic */

/*!\brief Splits a Communicator in numfiles different communicators
 *
 * \param[inout] comm communication context to be renumbered
 * \param[in] flags file open flags
 *
 */
int _sion_generic_renumber_collmsa(_sion_generic_gendata *comm, _sion_flags_store *flags)
{
  int grank = comm->grank;
  int gsize = comm->gsize;

  DPRINTFP((2, __func__, grank, "enter\n"));

  if (0 != strcmp(comm->apidesc->name, "SIONlib_MPI_API")) {
    return _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_RETURN, "MSA Collectives: not supported for generic API \"%s\"\n", comm->apidesc->name);
  }

  _sion_filedesc fd;
  _sion_init_filedesc(&fd);
  fd.ntasks = gsize;
  _sion_flags_entry *flags_entry = _sion_flags_get(flags, "collsize");
  if (flags_entry) {
    fd.collsize = atoi(flags_entry->val);
  }
  _sion_coll_check_env(&fd);
  sion_int32 collsize = fd.collsize;
  if (collsize < 2) {
    return _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_RETURN, "MSA Collectives: size of collective groups should be 2 or more, but is %d\n", collsize);
  }
  sion_int32 n_groups = gsize / collsize + ((gsize % collsize) ? 1 : 0);

  if (!fd.usecoll) {
    return _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_RETURN, "MSA Collectives: usecoll == false\n");
  }

  int is_candidate = _sion_generic_is_candidate(comm);
  int n_candidates = 0;
  int candidates_before = 0;
  // bring your own Allreduce and Exscan
  {
    int *candidates = NULL;
    if (0 == comm->grank) {
      candidates = calloc(gsize, sizeof(int));
      if (!candidates) {
        _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_ABORT, "MSA Collectives: malloc returned NULL\n");
      }
    }

    comm->apidesc->gatherr_cb(&is_candidate, candidates, comm->comm_data_global, _SION_INT32, 1, 0);

    if (0 == comm->grank) {
      for (size_t i = 0; i < gsize; i++) {
        int tmp = candidates[i];
        candidates[i] = n_candidates;
        n_candidates += tmp;
      }
    }

    comm->apidesc->bcastr_cb(&n_candidates, comm->comm_data_global, _SION_INT32, 1, 0);
    comm->apidesc->scatterr_cb(candidates, &candidates_before, comm->comm_data_global, _SION_INT32, 1, 0);

    if (0 == comm->grank) {
      free(candidates);
    }
  }

  if ((n_candidates < n_groups) || (n_candidates < comm->numfiles)) {
    return _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_RETURN, "MSA Collectives: insufficient number of candidates %d, number of groups %d, number of files %d\n", n_groups, comm->numfiles);

  }

  int groups_per_file = n_groups / comm->numfiles;
  int is_collector = is_candidate && (candidates_before < n_groups);
  if (is_collector) {
    int rank_collector = candidates_before;
    comm->filenumber = rank_collector % comm->numfiles;
    comm->lrank = (rank_collector / comm->numfiles) * collsize;
  } else {
    int collectors_before = (candidates_before < n_groups) ? candidates_before : n_groups;
    int rank_sender = grank - collectors_before;
    int group_number = rank_sender / (collsize - 1);
    comm->filenumber = group_number % comm->numfiles;
    comm->lrank = (group_number / comm->numfiles) * collsize + rank_sender % (collsize - 1) + 1;
  }
  comm->lsize = collsize * (groups_per_file + ((comm->filenumber < n_groups % comm->numfiles) ? 1 : 0));
  if (comm->filenumber == comm->numfiles - 1) {
    comm->lsize += gsize - n_groups * collsize;
  }

  DPRINTFP((32, __func__, grank, "MSA Collectives: global rank %d of %d, is candidate %d, is collector %d, file no %d, local rank %d, local size %d\n", grank, gsize, is_candidate, is_collector, comm->filenumber, comm->lrank, comm->lsize));
  DPRINTFP((2, __func__, grank, "exit\n"));
  return SION_SUCCESS;
}

int _sion_generic_is_candidate(_sion_generic_gendata *comm) {
#if defined(_SION_MSA_DEEP_EST_SDV)
  char hostname[1024];
  if (0 == gethostname(hostname, 1024)) {
    if (0 == strncmp("knl", hostname, 3)) {
      return 0;
    } else {
      return 1;
    }
  } else {
    return 0;
  }
#elif defined(_SION_MSA_HOSTNAME_REGEX)
  char *regex;
  regex_t compiled;
  int compile_error;
  if ((regex = _sion_getenv("SION_MSA_COLLECTOR_HOSTNAME_EREGEX"))) {
    compile_error = regcomp(&compiled, regex, REG_EXTENDED|REG_ICASE|REG_NOSUB);
  } else if ((regex = _sion_getenv("SION_MSA_COLLECTOR_HOSTNAME_REGEX"))) {
    compile_error = regcomp(&compiled, regex, REG_ICASE|REG_NOSUB);
  } else {
    return 1;
  }
  if (compile_error) {
    char error_msg[1024];
    size_t error_size = regerror(compile_error, &compiled, error_msg, 1024);
    regfree(&compiled);
    _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_ABORT, "MSA Collectives: error compiling regex \"%s\": %s%s\n", regex, error_msg, (error_size > 1024) ? "..." : "");
  }

  char hostname[1024];
  int hostname_error = gethostname(hostname, 1023);
  hostname[1023] = '\0';
  if (hostname_error) {
    regfree(&compiled);
    _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_ABORT, "MSA Collectives: error getting host name\n");
  }

  int exec_status = regexec(&compiled, hostname, 0, NULL, 0);
  if (exec_status == 0) {
    regfree(&compiled);
    return 1;
  } else if (exec_status == REG_NOMATCH) {
    regfree(&compiled);
    return 0;
  } else {
    char error_msg[1024];
    size_t error_size = regerror(exec_status, &compiled, error_msg, 1024);
    regfree(&compiled);
    _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_ABORT, "MSA Collectives: error matching regex \"%s\": %s%s\n", regex, error_msg, (error_size > 1024) ? "..." : "");
    return 0; // NOT REACHED
  }
#elif defined(_SION_MSA_TEST)
  return comm->grank %2;
#else
  return 1;
#endif
}
