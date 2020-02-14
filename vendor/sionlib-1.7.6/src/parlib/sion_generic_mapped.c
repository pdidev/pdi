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
 */

#define _XOPEN_SOURCE 700

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
#include "sion_internal_startptr.h"
#include "sion_printts.h"

#include "sion_buffer.h"

#include "sion_filedesc.h"
#include "sion_keyvalue.h"

#include "sion_generic_internal.h"
#include "sion_generic_collective.h"

#define _SION_CONST_MAX_INT32 2147483647

/*!\brief Generic parallel open of one direct access file.
          Mapping sion files to environment with less tasks.
 * @param  sid              sion file handle
 * @param  fname            filename to use
 * @param  file_mode_flags  flags from already parsed file_mode (see also sion_internal.h)
 * @param  prefix           prefix to be used in case of multiple files
 * @param  numFiles         Number of files to open
 * @param  nlocaltasks      FIXME
 * @param  globalranks      FIXME
 * @param  chunksizes       chunksizes for all tasks
 * @param  mapping_filenrs  FIXME
 * @param  mapping_lranks   FIXME
 * @param  fsblksize        blocksize of filesystem (must be equal on all processors)
 * @param  rank             rank of the current process
 * @param  ntasks           number of processes
 * @param  flag             sion options flag
 * @param  fileptr          filepointer for this task
 * @param  sion_gendata     FIXME
 *
 * @return  sion file handle
 *          -1 if error occured
 */

int _sion_paropen_mapped_generic(
				 int sid,
				 char  *fname,
				 sion_int64 file_mode_flags,
				 char  *prefix,
				 int   *numFiles,
				 int   *nlocaltasks,
				 int   **globalranks, 
				 sion_int64  **chunksizes,
				 int   **mapping_filenrs, 
				 int   **mapping_lranks,
				 sion_int32  *fsblksize,
				 int    rank,
				 int    ntasks,
				 int    flag,
				 FILE **fileptr,
				 _sion_generic_gendata *sion_gendata) {
  int       rc=SION_SUCCESS;
  int       filenr, root, task;

  _sion_filedesc *sion_filedesc_sub,*sion_filedesc_master;
  _sion_fileptr  *sion_fileptr = NULL;
  sion_int64 helpint64, apiflag, new_fsblocksize, lstartpointer;
  /* sion_int64 lchunksize, lstartpointer, lglobalrank, helpint64; */
  sion_int32 *sion_count = NULL;
  sion_int32 helpint32;
  sion_int64 *sion_tmpintfield2 = NULL;
  sion_int64 *sion_tmpintfield3 = NULL;

  /* for write */
  int *lfilecounter,*lfilemanager, ltask, tmpsize, blknum;

  if (file_mode_flags&_SION_FMODE_POSIX) apiflag=SION_FILE_FLAG_POSIX; 
  else if (file_mode_flags&_SION_FMODE_SIONFWD) apiflag=SION_FILE_FLAG_SIONFWD;
  else                                   apiflag=SION_FILE_FLAG_ANSI; 
  
  DPRINTFP((2, "_sion_paropen_mapped_generic", rank, "enter parallel open  sid=%d\n", sid));
  
  sion_filedesc_master = _sion_alloc_filedesc();
  if (sion_filedesc_master == NULL) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate filedescriptor structure of size %lu (sion_filedesc_master), aborting ...\n", 
                            (unsigned long) sizeof(sion_filedesc_master)));
  }
  
  _sion_init_filedesc(sion_filedesc_master);
  sion_filedesc_master->fname       = strdup(fname); /* Set the filename */
  sion_filedesc_master->state     = SION_FILESTATE_PAROPENMAPPEDMASTER;


  if (file_mode_flags&_SION_FMODE_WRITE) {
    /* **************** WRITE mode **************** */
    
    DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " starting open for write #tasks=%d\n", ntasks));


    for(ltask=0;ltask<*nlocaltasks;ltask++) {
      DPRINTFP((128, "_sion_paropen_mapped_generic", rank, "input: %2d: f=%d gtask=%2d ltask=%d  --> %d\n", ltask, 
		(int) (*mapping_filenrs)[ltask], 
		(int) (*globalranks)[ltask],
		(int) (*mapping_lranks)[ltask],
		(int) (*chunksizes)[ltask]
		));
    }
    
    
    /* init and allocate master filedesc */
    sion_filedesc_master->mode      = SION_FILEMODE_WRITE;
    _sion_reassignvcd(sid,sion_filedesc_master, SION_FILEDESCRIPTOR);
    sion_filedesc_master->sid=sid;
    sion_filedesc_master->nfiles=*numFiles;

    sion_filedesc_master->multifiles = (_sion_filedesc **) malloc(sion_filedesc_master->nfiles * sizeof(_sion_filedesc*));
    if (sion_filedesc_master->multifiles == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure vector of size %lu (chunksizes), aborting ...\n", 
                              (unsigned long) sion_filedesc_master->nfiles * sizeof(_sion_filedesc*)));
    }

    /* loop over all files:
       - collect chunksizes, ... on task 0 of file 
       - calculate startpointers
       - distribute info
    */   

    lfilecounter = (int *) malloc(*numFiles * sizeof(int));
    if (lfilecounter == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (lfilecounter), aborting ...\n",
                              (unsigned long) *numFiles * sizeof(int)));
    }
    lfilemanager = (int *) malloc(*numFiles * sizeof(int));
    if (lfilemanager == NULL) {
      free(lfilecounter);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (lfilemanager), aborting ...\n",
                              (unsigned long) *numFiles * sizeof(int)));
    }

    sion_count = (sion_int32 *) malloc(ntasks * sizeof(sion_int32));
    if (sion_count == NULL) {
      free(lfilemanager);
      free(lfilecounter);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (sion_count), aborting ...\n",
                              (unsigned long) ntasks * sizeof(sion_int32)));
    }

 
    /* get number of local tasks writing to a physical file */
    for(filenr=0;filenr<*numFiles;filenr++) lfilecounter[filenr]=lfilemanager[filenr]=0;
    for(ltask=0;ltask<*nlocaltasks;ltask++) {
      filenr=(*mapping_filenrs)[ltask];
      if((filenr<0) || (filenr>=*numFiles)){
        free(sion_count);
        free(lfilemanager);
        free(lfilecounter);
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: on task %d: wrong file number index (%d) in mapping for task %d, aborting ...\n",
                                rank,filenr,ltask));
      }
      lfilecounter[filenr]++;
      if((*mapping_lranks)[ltask]==0) {
        lfilemanager[filenr]=1;
      }
    }

    sion_filedesc_master->mapping_size=0;

    /* check for keyval parameter also on master */
    _sion_keyval_check_env(sion_filedesc_master, file_mode_flags);

    /* LOOP over files */
    for(filenr=0;filenr<*numFiles;filenr++) {
      
      DPRINTFP((4, "_sion_paropen_mapped_generic", rank, " starting init of SION #%d of %d (%d local tasks) \n", filenr,*numFiles,lfilecounter[filenr]));

      /* determine which task handle this file */
      root=-1;
      helpint32 = lfilemanager[filenr];
      sion_gendata->apidesc->gatherr_cb(&helpint32, sion_count, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
      if (rank == 0) {
        for(task=0;task<ntasks;task++) {
          if(sion_count[task]==1) root=task;
        }
        helpint64=root; 
      }
      sion_gendata->apidesc->bcastr_cb(&helpint64, sion_gendata->comm_data_global, _SION_INT64,1 , 0);  root=helpint64;
      if(root<0){
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: on task %d: local rank 0 of file number index (%d) will not used by any tasks, aborting ...\n",
                                rank,filenr));
      }

      DPRINTFP((32, "_sion_paropen_mapped_generic", rank, "  file #%d will be managed by task %d \n", filenr,root));

      /* allocate and initialise internal data structure with default values (NULL and -1) */
      sion_filedesc_sub = _sion_alloc_filedesc();
      if (sion_filedesc_sub == NULL) {
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n", 
                                (unsigned long) sizeof(sion_filedesc_sub)));
      }
      sion_filedesc_master->multifiles[filenr]=sion_filedesc_sub;

      _sion_init_filedesc(sion_filedesc_sub);
      
      sion_filedesc_sub->fname     = (sion_gendata->apidesc->get_multi_filename_cb?sion_gendata->apidesc->get_multi_filename_cb:_sion_get_multi_filename)(fname, filenr); /* Set the filename */
      if(rank == root ){
        sion_filedesc_sub->state      = SION_FILESTATE_PAROPENMAPPEDMANAGED;

      } else {
        sion_filedesc_sub->state      = SION_FILESTATE_PAROPENMAPPED;
      }
      sion_filedesc_sub->mode       = SION_FILEMODE_WRITE;
      sion_filedesc_sub->endianness = _sion_get_endianness_with_flags(file_mode_flags); /* Endianness */
      sion_filedesc_sub->fsblksize  = *fsblksize;
      sion_filedesc_sub->swapbytes  = 0;                       /* Endianness, swapping bytes */
      sion_filedesc_sub->nfiles     = *numFiles;
      sion_filedesc_sub->filenumber = filenr;
      sion_filedesc_sub->prefix     = strdup(prefix);
      sion_filedesc_sub->compress   = file_mode_flags&_SION_FMODE_COMPRESS;
      sion_filedesc_sub->usecoll    = file_mode_flags&_SION_FMODE_COLLECTIVE;
      sion_filedesc_sub->filemanagedbytask = root;

      /* check for keyval parameter */
      _sion_keyval_check_env(sion_filedesc_sub, file_mode_flags);
      
      /* allocate memory for storing MAXCHUNKS chunksize infos in internal structure */
      _sion_realloc_filedesc_blocklist(sion_filedesc_sub, MAXCHUNKS);

      /* the local datastructure for one sion file contains only info for a subset of tasks writing at all to this file.
         All parameter of this local data structures all set set as if the file consists only of the local maintained 
         tasks of the file. The data structure element all_localranks contains a mapping from local rank number 
         to the overall rank in this file. The data structure element all_globalranks contains the corresponding globalrank 
         number of all tasks of all files.
      */
      
      /* get number of tasks writing to this file */
      helpint32 = lfilecounter[filenr];
      sion_filedesc_sub->nlocaltasksinfile  = helpint32;
      sion_gendata->apidesc->gatherr_cb(&helpint32, sion_count, sion_gendata->comm_data_global, _SION_INT32, 1, root);
      if (rank == root) {
        helpint64=0;
        for(task=0;task<ntasks;task++) helpint64 += sion_count[task];
      }
      sion_gendata->apidesc->bcastr_cb(&helpint64, sion_gendata->comm_data_global, _SION_INT64, 1, root);
      sion_filedesc_sub->ntotaltasksinfile  = helpint64;
      sion_filedesc_master->mapping_size+=helpint64;
      DPRINTFP((32, "_sion_paropen_mapped_generic", rank, "mapping size increased to %d\n",sion_filedesc_master->mapping_size));

      /* root stores in sion_count the number of local task of this file for each task */

      DPRINTFP((32, "_sion_paropen_mapped_generic", rank, "  file #%d (%s) will store chunks from %d tasks (%d local) \n", 
                filenr,sion_filedesc_sub->fname,
                sion_filedesc_sub->ntotaltasksinfile,sion_filedesc_sub->nlocaltasksinfile));

      /* check and distribute new fsblksize */
      if(rank == root) {
        DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " create file #%d (%s) \n", filenr,sion_filedesc_sub->fname));
        sion_fileptr = _sion_file_open(sion_filedesc_sub->fname,apiflag|SION_FILE_FLAG_WRITE|SION_FILE_FLAG_CREATE,0);
        if (!sion_fileptr) {
          return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_generic: cannot open %s for writing, aborting ...\n", fname));
        }
        if(sion_filedesc_sub->fsblksize<=-1) {
          /* check with fstat fsblksize */
          new_fsblocksize=(sion_int64) _sion_file_get_opt_blksize(sion_fileptr);
          if((new_fsblocksize<0) || (new_fsblocksize>SION_MAX_FSBLOCKSIZE)) new_fsblocksize=SION_DEFAULT_FSBLOCKSIZE;
          DPRINTFP((32, "_sion_paropen_mapped_generic", rank, "  found new_fsblksize of %d\n", (int) new_fsblocksize));
        }
	if (sion_filedesc_sub->usebuffer) {
	  _sion_buffer_flush(sion_filedesc_sub);
	}
        _sion_file_close(sion_fileptr);
      }
      sion_gendata->apidesc->barrier_cb(sion_gendata->comm_data_global);
      if(sion_filedesc_sub->fsblksize==-1) {
        sion_gendata->apidesc->bcastr_cb(&new_fsblocksize, sion_gendata->comm_data_global, _SION_INT64, 1, root);
        sion_filedesc_sub->fsblksize = new_fsblocksize;
        DPRINTFP((32, "_sion_paropen_mapped_generic", rank, "  setting fsblksize to %d\n", (int) new_fsblocksize));
      }


     
      /* collect local ranks/chunksizes */
      tmpsize=3*sion_filedesc_sub->nlocaltasksinfile;
      sion_tmpintfield2 = (sion_int64 *) malloc(tmpsize * sizeof(sion_int64));
      if (sion_tmpintfield2 == NULL) {
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield2), aborting ...\n",
                                (unsigned long) tmpsize * sizeof(sion_int64)));
      }
      if (rank == root) {
        tmpsize=3*sion_filedesc_sub->ntotaltasksinfile;
        sion_tmpintfield3 = (sion_int64 *) malloc(tmpsize * sizeof(sion_int64));
        if (sion_tmpintfield3 == NULL) {
          free(sion_tmpintfield2);
          return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield3), aborting ...\n",
                                  (unsigned long) tmpsize * sizeof(sion_int64)));
        }
      } else sion_tmpintfield3 = NULL;
      
      { int p,lfilenr;
        p=0;
        for(ltask=0;ltask<*nlocaltasks;ltask++) {
          lfilenr=(int) (*mapping_filenrs)[ltask];
          if(lfilenr==filenr) {
            sion_tmpintfield2[p*3+0]=(sion_int64) (*mapping_lranks)[ltask];
            sion_tmpintfield2[p*3+1]=(sion_int64) (*globalranks)[ltask];
            sion_tmpintfield2[p*3+2]=(sion_int64) (*chunksizes)[ltask];
            DPRINTFP((64, "_sion_paropen_mapped_generic", rank, " prepare %d --> lrank=%d %d %d\n", p,(int) sion_tmpintfield2[p*3+0],(int) sion_tmpintfield2[p*3+1],(int) sion_tmpintfield2[p*3+2]));
            
            p++;
          }
        }
      }
      /*      for(ltask=0;ltask<3*sion_filedesc_sub->nlocaltasks;ltask++) {
              DPRINTFP((64, "_sion_paropen_mapped_generic", rank, "  debug in %d --> %d\n", ltask, (int) sion_tmpintfield2[ltask] ));
              }*/
      if (rank == root)  for(task=0;task<ntasks;task++) sion_count[task]*=3;
      sion_gendata->apidesc->gathervr_cb(sion_tmpintfield2, sion_tmpintfield3, sion_gendata->comm_data_global, _SION_INT64,sion_count,3*sion_filedesc_sub->nlocaltasksinfile, root);
      if (rank == root)  for(task=0;task<ntasks;task++) sion_count[task]/=3;
      /*      if (rank == root) {
              for(ltask=0;ltask<3*sion_filedesc_sub->ntasks;ltask++) {
              DPRINTFP((64, "_sion_paropen_mapped_generic", rank, "  debug out %d --> %d\n", ltask, (int) sion_tmpintfield3[ltask] ));
              }

              } */
      
      if (rank == root) {
        int lrank;

        /* memory allocation for internal fields (large size) */
        sion_filedesc_sub->ntasks=sion_filedesc_sub->ntotaltasksinfile;
        _sion_alloc_filedesc_arrays(sion_filedesc_sub);
   
        /* sort data into filedesc vectors */
        for(ltask=0;ltask<sion_filedesc_sub->ntotaltasksinfile;ltask++) {
          lrank=(int) sion_tmpintfield3[ltask*3+0];
          DPRINTFP((64, "_sion_paropen_mapped_generic", rank, " sort in ltask=%d --> lrank=%d\n", ltask,lrank));
          sion_filedesc_sub->all_globalranks[lrank]=sion_tmpintfield3[ltask*3+1];
          sion_filedesc_sub->all_chunksizes[lrank] =sion_tmpintfield3[ltask*3+2];
        }

        /* calculate startpointers */
        _sion_calculate_startpointers(sion_filedesc_sub);
	_sion_print_filedesc(sion_filedesc_sub, 512, "start of paropen_mapped", 1);
      }

      /* open file on all ranks */
      /*                                                                      */ DPRINTFTS(rank, "before open");
      sion_fileptr = _sion_file_open(sion_filedesc_sub->fname,apiflag|SION_FILE_FLAG_WRITE,0);
      if (!sion_fileptr) {
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_generic: cannot open %s for reading, aborting ...\n", fname));
      }
      sion_gendata->apidesc->barrier_cb(sion_gendata->comm_data_global);
      /*                                                                      */ DPRINTFTS(rank, "after open");
      
      /* store data in static data structure (sid)  */
      sion_filedesc_sub->fileptr = sion_fileptr;
      
      DPRINTFP((64, "_sion_paropen_mapped_generic", rank, " store for file %s fileprt=%x\n",sion_filedesc_sub->fname, sion_fileptr ));

      /* write header */
      if (rank == root) {
        
        /*                                                                      */ DPRINTFTS(rank, "before writeh");
        /* writes serial metadata, all_globalranks, all_chunksizes */
        _sion_write_header(sion_filedesc_sub);
        /*                                                                      */ DPRINTFTS(rank, "after writeh");
        
        /* needed for writing pointer to var part of metadata at the end of the file */
        sion_filedesc_sub->end_of_header = _sion_file_get_position(sion_filedesc_sub->fileptr);
        sion_filedesc_sub->start_of_data = sion_filedesc_sub->all_startpointers[0];

        /*set file pointer to end of file (max. file size with one chunk) */
        lstartpointer =  sion_filedesc_sub->all_startpointers[sion_filedesc_sub->ntasks - 1]
          + sion_filedesc_sub->all_chunksizes[sion_filedesc_sub->ntasks - 1];
        /*                                                                      */ DPRINTFTS(rank, "before setp(0)");
        _sion_file_flush(sion_filedesc_sub->fileptr);
        _sion_file_set_position(sion_filedesc_sub->fileptr, lstartpointer);
        /*                                                                      */ DPRINTFTS(rank, "after setp(0)");
        
        /* free on root some of the large fields */
        _sion_free_filedesc_all_globalranks(sion_filedesc_sub);
        
        /* and realloc with less memory */
        sion_filedesc_sub->ntasks=sion_filedesc_sub->nlocaltasksinfile;
        _sion_alloc_filedesc_all_globalranks(sion_filedesc_sub);
        _sion_alloc_filedesc_all_localranks(sion_filedesc_sub);
        
      }
            
      if(rank!=root) {
        /* allocate internal arrays (small size) */
        sion_filedesc_sub->ntasks=sion_filedesc_sub->nlocaltasksinfile;
        _sion_alloc_filedesc_arrays(sion_filedesc_sub);
        _sion_alloc_filedesc_all_localranks(sion_filedesc_sub);
      }

      /* allocate memory for keyval if necessary (small size) */
      if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE) {
	_sion_alloc_filedesc_all_keyvalptr(sion_filedesc_sub);
      }

      /* store data in internal data structure (rank info) */
      for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
        sion_filedesc_sub->all_localranks[ltask]  = sion_tmpintfield2[ltask*3+0];
        sion_filedesc_sub->all_globalranks[ltask] = sion_tmpintfield2[ltask*3+1];
      }
      
      /* distribute start_pointer and chunk sizes */
      if (rank == root) {
        int lrank;
        /* get data out of filedesc vectors */
        for(ltask=0;ltask<sion_filedesc_sub->ntotaltasksinfile;ltask++) {
          lrank=(int) sion_tmpintfield3[ltask*3+0];
          sion_tmpintfield3[ltask*3+1]=sion_filedesc_sub->all_startpointers[lrank];
          sion_tmpintfield3[ltask*3+2]=sion_filedesc_sub->all_chunksizes[lrank]; /* possible changed by calculate_startpointers */
          DPRINTFP((64, "_sion_paropen_mapped_generic", rank, " sort out ltask=%d --> lrank=%d startptr=%ld chunksize=%ld\n",
		    ltask,lrank,(long) sion_tmpintfield3[ltask*3+1],sion_tmpintfield3[ltask*3+2]));
        }
      }
      if (rank == root)  for(task=0;task<ntasks;task++) sion_count[task]*=3;
      sion_gendata->apidesc->scattervr_cb(sion_tmpintfield2, sion_tmpintfield3, sion_gendata->comm_data_global, _SION_INT64,sion_count,3*sion_filedesc_sub->nlocaltasksinfile, root);
      if (rank == root)  for(task=0;task<ntasks;task++) sion_count[task]/=3;

      if (rank == root) {
        /* free on root the last large fields */
        _sion_free_filedesc_all_startpointers(sion_filedesc_sub);
        _sion_free_filedesc_all_chunksizes(sion_filedesc_sub);
        /* and realloc with less memory */
        _sion_alloc_filedesc_all_startpointers(sion_filedesc_sub);
        _sion_alloc_filedesc_all_chunksizes(sion_filedesc_sub);
      }

      for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
        sion_filedesc_sub->all_startpointers[ltask]  = sion_tmpintfield2[ltask*3+1];
        sion_filedesc_sub->all_chunksizes[ltask]     = sion_tmpintfield2[ltask*3+2];
        DPRINTFP((64, "_sion_paropen_mapped_generic", rank, " get startpointer[%d] --> %ld\n", ltask, sion_filedesc_sub->all_startpointers[ltask]));
      }

      /* distribute globalskip */
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->globalskip, sion_gendata->comm_data_global, _SION_INT64, 1, root);
         

      /* set filepointer on each task */
      /*                                                                      */ DPRINTFTS(rank, "before setp");
      sion_gendata->apidesc->barrier_cb(sion_gendata->comm_data_global);

      /* initalize current positions */
      DPRINTFP((64, "_sion_paropen_mapped_generic", rank, " allocating block arrays of size %d\n", sion_filedesc_sub->ntasks));
      _sion_alloc_filedesc_block_arrays(sion_filedesc_sub);
      for (ltask = 0; ltask < sion_filedesc_sub->nlocaltasksinfile; ltask++) {
        sion_filedesc_sub->all_blockcount[ltask] = 1;
        sion_filedesc_sub->all_currentpos[ltask] = sion_filedesc_sub->all_startpointers[ltask];
	DPRINTFP((64, "_sion_paropen_mapped_generic", rank, " set all_currentpos[%d]=%d\n",ltask,sion_filedesc_sub->all_currentpos[ltask] ));
        sion_filedesc_sub->all_currentblocknr[ltask] = 0;
        sion_filedesc_sub->all_blocksizes[0 * sion_filedesc_sub->nlocaltasksinfile + ltask] = 0;
      }
      
      /* set position to first block of rank 0 */
      sion_filedesc_sub->rank           = 0;
      sion_filedesc_sub->globalrank     = sion_filedesc_sub->all_globalranks[sion_filedesc_sub->rank]; 
      sion_filedesc_sub->chunksize      = sion_filedesc_sub->all_chunksizes[0];
      sion_filedesc_sub->startpos       = sion_filedesc_sub->all_startpointers[0];
      sion_filedesc_sub->currentpos     = sion_filedesc_sub->startpos;
      sion_filedesc_sub->lastchunknr    = 0;
      sion_filedesc_sub->currentblocknr = 0; 

      _sion_file_purge(sion_fileptr);
      _sion_file_set_position(sion_fileptr, sion_filedesc_sub->currentpos);
      
      sion_gendata->apidesc->barrier_cb(sion_gendata->comm_data_global);
    
      if(sion_tmpintfield2) free(sion_tmpintfield2);
      if(sion_tmpintfield3) free(sion_tmpintfield3);
      
      DPRINTFP((4, "_sion_paropen_mapped_generic", rank, " ending   init of SION #%d of %d \n", filenr,*numFiles));
      
    } /* for each file */

    
    /* set master to first file, first available rank  */
    

    /* lookup file which contains current rank and set master */
    { int lfile=-1, lrank=-1, blknum; 
      sion_filedesc_sub=NULL;
      for(filenr=0;filenr<*numFiles;filenr++) {
        sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];
	lfile=filenr;
	lrank=0;
        if(sion_filedesc_sub->nlocaltasksinfile>0) {
          lfile=filenr;
          lrank=0;
          break;
        }
      }

      if((sion_filedesc_sub) && (lrank>=0) && (lfile>=0)) {
        _sion_realloc_filedesc_blocklist(sion_filedesc_master, MAXCHUNKS);

	sion_filedesc_master->ntotaltasksinfile = sion_filedesc_master->mapping_size;
        sion_filedesc_master->globalrank = sion_filedesc_sub->all_globalranks[lrank];
        sion_filedesc_master->rank       = lrank;
        sion_filedesc_master->fsblksize  = sion_filedesc_sub->fsblksize;
        sion_filedesc_master->swapbytes  = sion_filedesc_sub->swapbytes;
        sion_filedesc_master->keyvalmode = sion_filedesc_sub->keyvalmode;
        sion_filedesc_master->filenumber = lfile;

        /* set info for current rank and position */
        sion_filedesc_master->chunksize      = sion_filedesc_sub->all_chunksizes[lrank];
        sion_filedesc_master->startpos       = sion_filedesc_sub->all_startpointers[lrank];
        sion_filedesc_master->currentpos     = sion_filedesc_master->startpos;
        sion_filedesc_master->globalskip     = sion_filedesc_sub->globalskip;

        sion_filedesc_master->currentblocknr = 0;
        sion_filedesc_master->lastchunknr    = sion_filedesc_sub->all_blockcount[lrank]-1;
     
        sion_filedesc_master->maxusedchunks  = sion_filedesc_sub->maxusedchunks;

        for (blknum = 0; blknum < sion_filedesc_sub->all_blockcount[lrank]; blknum++) {
          sion_filedesc_master->blocksizes[blknum] = sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->nlocaltasksinfile * blknum + lrank];
        }
        
        /* set file pointer */
        sion_filedesc_master->fileptr = sion_filedesc_master->multifiles[lfile]->fileptr;
        
        /* set position */
        _sion_file_flush(sion_filedesc_master->fileptr);

        DPRINTFP((32,"sion_paropen_mapped_generic:",rank,"set startpointer to %d\n",(int) sion_filedesc_master->currentpos));
        _sion_file_set_position(sion_filedesc_master->fileptr, sion_filedesc_master->currentpos);
      
        if(fileptr!=NULL) {
          if(sion_filedesc_master->fileptr->flags&&SION_FILE_FLAG_ANSI) {
            *fileptr=sion_filedesc_master->fileptr->fileptr;
            sion_filedesc_master->fileptr_exported=1;
          } else {
            *fileptr=NULL;
            sion_filedesc_master->fileptr_exported=0;
          }
        }

      }
    }
    _sion_print_filedesc(sion_filedesc_master, 512, "_sion_paropen_mapped_generic", 1);
    
    if(sion_count) free(sion_count);
    
    if(lfilecounter) free(lfilecounter);
    if(lfilemanager) free(lfilemanager);
    
    /* ******************** end of WRITE ************************* */

  } else if (file_mode_flags&_SION_FMODE_READ) {

    /* ******************** start of READ ************************* */
    int         mapping_size,mapsid=-1, rootcounter;
    int         lnfiles,lntasks;
    sion_int32  lfsblksize;
    FILE       *lfileptr;
    sion_int32 *mapping;

    DPRINTFP((1, "_sion_paropen_mapped_generic", rank, "read mode ntasks=%d\n",ntasks));

    for(ltask=0;ltask<*nlocaltasks;ltask++) {
      DPRINTFP((128, "_sion_paropen_mapped_generic", rank, "input: %2d: gtask=%2d \n", ltask, (int) (*globalranks)[ltask]));
    }
    
    /* init and allocate master filedesc */
    sion_filedesc_master->mode      = SION_FILEMODE_READ;
    _sion_reassignvcd(sid,sion_filedesc_master, SION_FILEDESCRIPTOR);
    sion_filedesc_master->sid=sid;

    /* allocate temp fields */
    sion_count = (sion_int32 *) malloc(ntasks * sizeof(sion_int32));
    if (sion_count == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (sion_count), aborting ...\n",
                              (unsigned long) ntasks * sizeof(sion_int32)));
    }
    lfilemanager=NULL;
    
    /* 1. Read mapping info from file */

    if(rank == 0) {
      /* open and get mapping of sion file */
      DPRINTFP((1, "_sion_paropen_mapped_generic", rank, "before open\n"));
      mapsid=_sion_open_read(fname,_SION_FMODE_READ|_SION_FMODE_ANSI,_SION_READ_MASTER_ONLY_OF_MULTI_FILES,
			     &lntasks,&lnfiles,NULL,&lfsblksize,NULL,&lfileptr);
      if(mapsid>=0) {
	DPRINTFP((1, "_sion_paropen_mapped_generic", rank, "after open\n"));
	rc=sion_get_mapping(mapsid,&mapping_size,&mapping,numFiles);
	DPRINTFP((1, "_sion_paropen_mapped_generic", rank, "sion file %d files #tasks=%d rc=%d\n", *numFiles,mapping_size,rc));

	/* allocate vector to distribute info about which task is managing which file */
	lfilemanager = (int *) malloc(*numFiles * sizeof(int));
	if (lfilemanager == NULL) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (lfilemanager), aborting ...\n",
				  (unsigned long) *numFiles * sizeof(int)));
	}

	sion_filedesc_master->keyvalmode=sion_get_keyval_mode(mapsid);
	
	if(*numFiles>1) {
	  /* 1a. search filemanager for files, granks with lrank=0 for each file in mapping (tasks reading first sion task of multi file)  */
	  for(task=0;task<mapping_size;task++) {
	    if(mapping[task*2+1]==0) {
	      lfilemanager[mapping[task*2+0]]=task;
	      DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " manager of file %d is grank=%d\n",mapping[task*2+0],task));
	    }
	  }
	  sion_filedesc_master->mapping_size=mapping_size;

	} else {
	  /* only one file */
	  sion_filedesc_master->mapping_size=lntasks;
	  lfilemanager[0]=0;
	}
	
      } else {
	*numFiles=-1;
        free(sion_count);
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot find file %s , aborting ...\n",fname));
      }
    }

    /* broadcast information about mapping (number of global sion tasks) */
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc_master->mapping_size,  sion_gendata->comm_data_global, _SION_INT32, 1, 0);
    DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " mapping_size is=%d\n",sion_filedesc_master->mapping_size));

    /* and close file directly */
    if(rank == 0) {
      if (mapsid>=0) _sion_close_sid(mapsid);       /* frees also mapping vector */
    }

    if(*nlocaltasks<0) {
      /* 2. If not globalranks parameter given */
      /* 2.a. compute distribution */
      /* --> simple blockwise distribution, later improvements with task-to-file relation */
      int tasks_per_task = sion_filedesc_master->mapping_size / ntasks;
      int startpos       = rank*tasks_per_task;
      int endpos         = (rank+1)*tasks_per_task;
      if(rank+1==ntasks) {
	/* remaining tasks are added to last communicator */
	if(endpos<sion_filedesc_master->mapping_size) endpos=sion_filedesc_master->mapping_size;
      }
      *nlocaltasks=endpos-startpos;
      DPRINTFP((32,"sion_paropen_mapped_generic:",rank,"pre-compute distribution: startpos=%d endpos=%d nlocaltasks=%d\n",startpos, endpos, (int) *nlocaltasks ));

      if (globalranks == NULL) {
        free(lfilemanager);
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"pre-compute distribution: cannot allocate globalranks, not pointer parameter given, aborting ...\n"));
      }
      if (mapping_filenrs == NULL) {
        free(lfilemanager);
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"pre-compute distribution: cannot allocate mapping_filenrs, not pointer parameter given, aborting ...\n"));
      }
      if (mapping_lranks == NULL) {
        free(lfilemanager);
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"pre-compute distribution: cannot allocate mapping_lranks, not pointer parameter given, aborting ...\n"));
      }
      if ((*globalranks) != NULL) {
        free(lfilemanager);
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"pre-compute distribution: cannot allocate globalranks, memory allready allocated (unknown size), aborting ...\n"));
      }
      if ((*mapping_filenrs) != NULL) {
        free(lfilemanager);
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"pre-compute distribution: cannot allocate mapping_filenrs, memory allready allocated (unknown size), aborting ...\n"));
      }
      if ((*mapping_lranks) != NULL) {
        free(lfilemanager);
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"pre-compute distribution: cannot allocate mapping_lranks, memory allready allocated (unknown size), aborting ...\n"));
      }
      
      
      DPRINTFP((32,"sion_paropen_mapped_generic:",rank,"allocate globalranks field for parameter size=%d\n",(int) *nlocaltasks ));
      *globalranks = (sion_int32 *) malloc(*nlocaltasks * sizeof(sion_int32));
      if (*globalranks == NULL) {
        free(lfilemanager);
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate memory of size %lu (globalranks), aborting ...\n", (unsigned long) sizeof(sion_int32)));
      }
      /* init globalranks vector */
      ltask=0;
      for(task=startpos;task<endpos;task++) {
	(*globalranks)[ltask]=task;
	DPRINTFP((32,"sion_paropen_mapped_generic:",rank,"set *globalranks[%d]=%d\n",(int) ltask,(*globalranks)[ltask]));
	ltask++;
      }
      
    }

    /* broadcast keyvalmode from file  */
    sion_gendata->apidesc->bcastr_cb(&sion_filedesc_master->keyvalmode,  sion_gendata->comm_data_global, _SION_INT32, 1, 0);
    DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " keyvalmode is=%d\n",sion_filedesc_master->keyvalmode));

    /* check for keyval parameter also on master */
    _sion_keyval_check_env(sion_filedesc_master, file_mode_flags);

    /* 4. bcast numfiles to all */
    if (rank == 0) helpint64=*numFiles; 
    sion_gendata->apidesc->bcastr_cb(&helpint64, sion_gendata->comm_data_global, _SION_INT64, 1, 0);  *numFiles=helpint64;
    sion_filedesc_master->nfiles=*numFiles;

    /* allocate on each task filedesc structure for each multifile */
    sion_filedesc_master->multifiles = (_sion_filedesc **) malloc(sion_filedesc_master->nfiles * sizeof(_sion_filedesc*));
    if (sion_filedesc_master->multifiles == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure vector of size %lu (chunksizes), aborting ...\n", 
                              (unsigned long) sion_filedesc_master->nfiles * sizeof(_sion_filedesc*)));
    }

    /* 5. Loop over files */
    rootcounter=0;  /* for round robin */
    for(filenr=0;filenr<sion_filedesc_master->nfiles;filenr++) {
      
      DPRINTFP((4, "_sion_paropen_mapped_generic", rank, " starting init of SION #%d of %d\n", filenr,*numFiles));

#ifdef _SION_FIND_ROOT_LRANK_NULL_DO_NOT_USE    
      {
	int grankroot;
	/* WARNING: this algorithm has some problems if no rank reads lrank-0 from file */
	/* bcast grank with lrank=0 for this file */
	grankroot=-1;
	if (rank == 0) {
	  helpint64=lfilemanager[filenr]; 
	}
	sion_gendata->apidesc->bcastr_cb(&helpint64, sion_gendata->comm_data_global, _SION_INT64, 1, 0);  
	grankroot=helpint64;

	/* search for grankroot in my globalranks */
	helpint32=-1;
	for(task=0;task<*nlocaltasks;task++) {
	  if((*globalranks)[task]==grankroot) helpint32=1;
	}

	/* gather on task 0 vector containing -1 or ranknumber for each task to determine which task is handling this file */
	sion_gendata->apidesc->gatherr_cb(&helpint32, sion_count, sion_gendata->comm_data_global, _SION_INT32, 1, 0);
	if (rank == 0) {
	  for(task=0;task<ntasks;task++) {
	    if(sion_count[task]==1) {
	      root=task;
	      break;
	    }
	  }
	  helpint32=root; 
	}
      }
#endif

      /* round robin distribution of master role for a multi-file */
      rootcounter=( (rootcounter+1) % ntasks);
      helpint64=rootcounter;
      
      /* bcast root number for this file */
      sion_gendata->apidesc->bcastr_cb(&helpint64, sion_gendata->comm_data_global, _SION_INT64, 1, 0);  
      root=helpint64;
      DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " manager of file %d is rank=%d\n",filenr,root));

      /* allocate and initialise internal data structure with default values (NULL and -1) */
      sion_filedesc_sub = _sion_alloc_filedesc();
      if (sion_filedesc_sub == NULL) {
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n", 
                                (unsigned long) sizeof(sion_filedesc_sub)));
      }
      sion_filedesc_master->multifiles[filenr]=sion_filedesc_sub;
      _sion_init_filedesc(sion_filedesc_sub);
      sion_filedesc_sub->fname     = (sion_gendata->apidesc->get_multi_filename_cb?sion_gendata->apidesc->get_multi_filename_cb:_sion_get_multi_filename)(fname, filenr); /* Set the filename */
      if(rank == root ){
        sion_filedesc_sub->state      = SION_FILESTATE_PAROPENMAPPEDMANAGED;
      } else {
        sion_filedesc_sub->state      = SION_FILESTATE_PAROPENMAPPED;
      }
      sion_filedesc_sub->mode       = SION_FILEMODE_READ;

      /* open file on each task */
      DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " starting open for read on file %s\n", sion_filedesc_sub->fname));
      /*                                                                      */ DPRINTFTS(rank, "before open"); 
      sion_filedesc_sub->fileptr=NULL;
     if (rank == root) {
	sion_fileptr = _sion_file_open(sion_filedesc_sub->fname,apiflag|SION_FILE_FLAG_READ,0);
	if (!sion_fileptr) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot open %s for reading, aborting ...\n", fname));
	}
	sion_filedesc_sub->fileptr = sion_fileptr;
      }
      sion_gendata->apidesc->barrier_cb(sion_gendata->comm_data_global); 
      /*                                                                      */ DPRINTFTS(rank, "after  open root");

      /* get meta data of file (1st Block ) on managing task */
      if (rank == root) {
	rc = _sion_read_header_fix_part(sion_filedesc_sub);
	if (rc!=SION_SUCCESS) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot read header from file %s, aborting ...\n", fname));
	}
	DPRINTFP((32, "_sion_paropen_mapped_generic", rank,
		  " read, after read of fix header part endianness=0x%x blksize=%d ntasks=%d\n", sion_filedesc_sub->endianness, sion_filedesc_sub->fsblksize, sion_filedesc_sub->ntasks));

	/*                                                                      */ DPRINTFTS(rank, "before alloc");
	/* memory allocation (large fields, all sion tasks of file) */
	_sion_alloc_filedesc_arrays(sion_filedesc_sub);
	/*                                                                      */ DPRINTFTS(rank, "after alloc");
	
	rc = _sion_read_header_var_part(sion_filedesc_sub);
	if (rc != SION_SUCCESS) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot read header from file %s, aborting ...\n", fname));
	}
	      
	_sion_coll_check_env(sion_filedesc_sub);
	if(sion_filedesc_sub->usecoll)  _sion_alloc_filedesc_coll_arrays(sion_filedesc_sub);
	      
	/* collective */
	if (!sion_filedesc_sub->usecoll) _sion_calculate_startpointers(sion_filedesc_sub);
	else                             _sion_calculate_startpointers_collective(sion_filedesc_sub);
	/*                                                                      */ DPRINTFTS(rank, "after calculate");

	/* check for keyval parameter  */
	_sion_keyval_check_env(sion_filedesc_sub, file_mode_flags);

      }

      /* distribute globalskip */
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->globalskip,  sion_gendata->comm_data_global, _SION_INT64, 1, root);

      /* broadcast information read from file */
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->endianness,  sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->swapbytes,   sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->fsblksize,   sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->ntasks,      sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_filedesc_sub->ntotaltasksinfile=sion_filedesc_sub->ntasks;
      sion_filedesc_sub->nlocaltasksinfile=-1; /* has to be computed after distribution of globalranks */
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->fileversion, sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->nfiles,      sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->filenumber,  sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->flag1,       sion_gendata->comm_data_global, _SION_INT64, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->flag2,       sion_gendata->comm_data_global, _SION_INT64, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->maxusedchunks,      sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->start_of_varheader, sion_gendata->comm_data_global, _SION_INT64, 1, root);
      DPRINTFP((32, "_sion_paropen_mapped_generic", rank,
		" read, after read of maxusedchunks=%d maxchunks=%d (%d) start_of_varheader=%d\n", sion_filedesc_sub->maxusedchunks,sion_filedesc_sub->maxchunks, MAXCHUNKS,(int) sion_filedesc_sub->start_of_varheader ));
      if (sion_filedesc_sub->maxusedchunks > sion_filedesc_sub->maxchunks)  _sion_realloc_filedesc_blocklist(sion_filedesc_sub, sion_filedesc_sub->maxusedchunks);
      /*                                                                      */ DPRINTFTS(rank, "after bcast");

      /* allocate temp field to receive data of all tasks in file; Data
         distribution will be done by data sieving: meta data about all sion
         tasks in file will be sent to all task and meta data about local sion
         tasks will be selected from these fields */
      tmpsize=sion_filedesc_sub->ntotaltasksinfile;
      sion_tmpintfield2 = (sion_int64 *) malloc(tmpsize * sizeof(sion_int64));
      if (sion_tmpintfield2 == NULL) {
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield2), aborting ...\n",
                                (unsigned long) tmpsize * sizeof(sion_int64)));
      }

      /* bcast GLOBALRANKS number in this file */
      if(rank==root) for(task=0;task<sion_filedesc_sub->ntotaltasksinfile;task++) sion_tmpintfield2[task]=sion_filedesc_sub->all_globalranks[task];
      sion_gendata->apidesc->bcastr_cb(sion_tmpintfield2,   sion_gendata->comm_data_global, _SION_INT64, sion_filedesc_sub->ntotaltasksinfile, root);
     
      /* compare globalrank number list with list of globalrank to open on this task */
      sion_filedesc_sub->nlocaltasksinfile=0;
      for(task=0;task<sion_filedesc_sub->ntotaltasksinfile;task++) {
	DPRINTFP((64, "_sion_paropen_mapped_generic", rank, "   scan for gtask=%d\n", (int) sion_tmpintfield2[task]));
	for(ltask=0;ltask<*nlocaltasks;ltask++) {
	  if( (int) sion_tmpintfield2[task]==(*globalranks)[ltask]) {
	    sion_tmpintfield2[task]=-1 * (sion_tmpintfield2[task] + 1); /* set mask (set to negative value) */
	    sion_filedesc_sub->nlocaltasksinfile++;                    
	  }
	}
      }
      DPRINTFP((32, "_sion_paropen_mapped_generic", rank, " found %d globalranks for file %s\n", sion_filedesc_sub->nlocaltasksinfile, sion_filedesc_sub->fname));

      /*                                                                      */ DPRINTFTS(rank, "before open non-root");
      /* if really necessary (not needed when on this task no globalranks are stored in this file)  */
      if ( (rank != root) && (sion_filedesc_sub->nlocaltasksinfile>0) ) {
	sion_fileptr = _sion_file_open(sion_filedesc_sub->fname,apiflag|SION_FILE_FLAG_READ,0);
	if (!sion_fileptr) {
	  return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot open %s for reading, aborting ...\n", fname));
	}
	sion_filedesc_sub->fileptr = sion_fileptr;
      }
      /* sion_gendata->apidesc->barrier_cb(sion_gendata->comm_data_global);  */
      /*                                                                      */ DPRINTFTS(rank, "after  open non-root");


      /* allocate internal arrays (small size, only sion tasks needed by this task) */
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
	/* free on root some of the large fields */
        _sion_free_filedesc_all_globalranks(sion_filedesc_sub);
	/* and realloc with less memory */
        sion_filedesc_sub->ntasks=sion_filedesc_sub->nlocaltasksinfile;
        _sion_alloc_filedesc_all_globalranks(sion_filedesc_sub);
        _sion_alloc_filedesc_all_localranks(sion_filedesc_sub);
	/* all_chunksizes will follow later */
      } else {
        sion_filedesc_sub->ntasks=sion_filedesc_sub->nlocaltasksinfile;
        _sion_alloc_filedesc_arrays(sion_filedesc_sub);
        _sion_alloc_filedesc_all_localranks(sion_filedesc_sub);
      }

      /* store position of local tasks in this file */
      ltask=0;
      for(task=0;task<sion_filedesc_sub->ntotaltasksinfile;task++) {
	if(sion_tmpintfield2[task]<0) {
	  sion_filedesc_sub->all_localranks[ltask]=task;
	  sion_filedesc_sub->all_globalranks[ltask]= (-1 * sion_tmpintfield2[task]) - 1;
	  DPRINTFP((64, "_sion_paropen_mapped_generic", rank, "   all_globalranks[%d]=%d all_localranks[%d]=%d\n", 
		    ltask,(int) sion_filedesc_sub->all_globalranks[ltask],ltask,(int) sion_filedesc_sub->all_localranks[ltask]));
	  ltask++;
	}
      }

      /* bcast CHUNKSIZES */
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
	for(task=0;task<sion_filedesc_sub->ntotaltasksinfile;task++) sion_tmpintfield2[task]=sion_filedesc_sub->all_chunksizes[task];
      }
      sion_gendata->apidesc->bcastr_cb(sion_tmpintfield2,   sion_gendata->comm_data_global, _SION_INT64, sion_filedesc_sub->ntotaltasksinfile, root);
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
	_sion_free_filedesc_all_chunksizes(sion_filedesc_sub);          /* free on root large field */
        _sion_alloc_filedesc_all_chunksizes(sion_filedesc_sub);         /* and realloc with less memory */
      }
      /* store in local field */
      for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
	task=sion_filedesc_sub->all_localranks[ltask];
	sion_filedesc_sub->all_chunksizes[ltask]=sion_tmpintfield2[task];
      }

      /* bcast STARTPOINTERS */
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
	for(task=0;task<sion_filedesc_sub->ntotaltasksinfile;task++) sion_tmpintfield2[task]=sion_filedesc_sub->all_startpointers[task];
      }
      sion_gendata->apidesc->bcastr_cb(sion_tmpintfield2,   sion_gendata->comm_data_global, _SION_INT64, sion_filedesc_sub->ntotaltasksinfile, root);
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
        _sion_free_filedesc_all_startpointers(sion_filedesc_sub);          /* free on root large field */
        _sion_alloc_filedesc_all_startpointers(sion_filedesc_sub);         /* and realloc with less memory */
      }
      /* store in local field */
      for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
	task=sion_filedesc_sub->all_localranks[ltask];
	sion_filedesc_sub->all_startpointers[ltask]=sion_tmpintfield2[task];
      }
      
      /* allocate memory for block info */
      _sion_alloc_filedesc_block_arrays(sion_filedesc_sub);

      /* bcast BLOCKCOUNT */
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
        sion_filedesc_sub->ntasks=sion_filedesc_sub->ntotaltasksinfile;
	_sion_read_header_var_part_blockcount_to_field(sion_filedesc_sub, sion_filedesc_sub->ntotaltasksinfile, sion_tmpintfield2);
        sion_filedesc_sub->ntasks=sion_filedesc_sub->nlocaltasksinfile;
	for(task=0;task<sion_filedesc_sub->ntotaltasksinfile;task++) 
	  DPRINTFP((64, "_sion_paropen_mapped_generic", rank, "   got from file blockcount[%d]=%d\n",task,(int) sion_tmpintfield2[task])); 
      }
      sion_gendata->apidesc->bcastr_cb(sion_tmpintfield2,   sion_gendata->comm_data_global, _SION_INT64, sion_filedesc_sub->ntotaltasksinfile, root);
      /* store in local field */
      for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
	task=sion_filedesc_sub->all_localranks[ltask];
	sion_filedesc_sub->all_blockcount[ltask]=sion_tmpintfield2[task];
	DPRINTFP((64, "_sion_paropen_mapped_generic", rank, "   store blockcount task=%d -> ltask=%d cnt=%d\n",task,ltask,(int) sion_tmpintfield2[task])); 

      }

      /* bcast BLOCKSIZES */
      for (blknum = 0; blknum < sion_filedesc_sub->maxusedchunks; blknum++) {
	if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
	  sion_filedesc_sub->ntasks=sion_filedesc_sub->ntotaltasksinfile;
	  _sion_read_header_var_part_nextblocksizes_to_field(sion_filedesc_sub, sion_filedesc_sub->ntotaltasksinfile, sion_tmpintfield2);
	  sion_filedesc_sub->ntasks=sion_filedesc_sub->nlocaltasksinfile;
	}
	sion_gendata->apidesc->bcastr_cb(sion_tmpintfield2,   sion_gendata->comm_data_global, _SION_INT64, sion_filedesc_sub->ntotaltasksinfile, root);
	/* store in local field */
	for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
	  task=sion_filedesc_sub->all_localranks[ltask];
	  sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->nlocaltasksinfile * blknum + ltask]=sion_tmpintfield2[task];
	}
      }

      /* distribute keyval options */
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->keyvalmode, sion_gendata->comm_data_global, _SION_INT32, 1, root);

      /* distribute collective options */
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->usecoll, sion_gendata->comm_data_global, _SION_INT32, 1, root);
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->collsize, sion_gendata->comm_data_global, _SION_INT32, 1, root);
      
      if(sion_filedesc_sub->usecoll) {

	if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
	  for(task=0;task<sion_filedesc_sub->ntotaltasksinfile;task++) 
	    sion_tmpintfield2[task]= (sion_int64) (sion_filedesc_sub->all_coll_collector[task]*_SION_CONST_MAX_INT32) + sion_filedesc_sub->collsize;
	}
	sion_gendata->apidesc->bcastr_cb(sion_tmpintfield2,   sion_gendata->comm_data_global, _SION_INT64, sion_filedesc_sub->ntotaltasksinfile, root);
	if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
	  _sion_free_filedesc_coll_arrays(sion_filedesc_sub);          /* free on root large field */
	}
	_sion_alloc_filedesc_all_chunksizes(sion_filedesc_sub);         /* and alloc with less memory on all tasks */

	/* store in local field */
	DPRINTFP((64, "_sion_paropen_mapped_generic", rank, "   store collsize+collector for %d elements\n",sion_filedesc_sub->nlocaltasksinfile)); 
	for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
	  task=sion_filedesc_sub->all_localranks[ltask];
	  sion_filedesc_sub->all_coll_collsize[sion_filedesc_sub->nlocaltasksinfile * blknum + ltask]=(sion_int32) sion_tmpintfield2[task]%_SION_CONST_MAX_INT32;
	  sion_filedesc_sub->all_coll_collector[sion_filedesc_sub->nlocaltasksinfile * blknum + ltask]= (sion_int32)
	    (sion_tmpintfield2[task]-sion_filedesc_sub->all_coll_collsize[sion_filedesc_sub->nlocaltasksinfile * blknum + ltask])/_SION_CONST_MAX_INT32;
	}
      }

      /* init rest of data structure */
      if(sion_filedesc_sub->nlocaltasksinfile>0) {
	for (ltask = 0; ltask < sion_filedesc_sub->nlocaltasksinfile; ltask++) {
	  sion_filedesc_sub->all_currentpos[ltask]     = sion_filedesc_sub->all_startpointers[ltask];
	  sion_filedesc_sub->all_currentblocknr[ltask] = 0;
	}
	sion_filedesc_sub->rank           = 0;
	sion_filedesc_sub->globalrank     = sion_filedesc_sub->all_globalranks[sion_filedesc_sub->rank]; 
	sion_filedesc_sub->chunksize      = sion_filedesc_sub->all_chunksizes[0];
	sion_filedesc_sub->startpos       = sion_filedesc_sub->all_startpointers[0];
	sion_filedesc_sub->currentpos     = sion_filedesc_sub->startpos;
	sion_filedesc_sub->currentblocknr = 0; 
	sion_filedesc_sub->lastchunknr    = sion_filedesc_sub->all_blockcount[sion_filedesc_sub->rank]-1;

	/* allocate memory for keyval if necessary (small size) */
	if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE) {
	  _sion_alloc_filedesc_all_keyvalptr(sion_filedesc_sub);
	}


	_sion_file_purge(sion_fileptr);
	_sion_file_set_position(sion_fileptr, sion_filedesc_sub->currentpos);
      }
      if(sion_tmpintfield2) free(sion_tmpintfield2);

    } /* filenr */

    /* set master to first file, first available rank  */

    /* lookup file which contains current rank and set master */
    { 
      int lfile=-1,lrank=-1, blknum; 
      sion_filedesc_sub=NULL;
      for(filenr=0;filenr<sion_filedesc_master->nfiles;filenr++) {
        sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];
        if(sion_filedesc_sub->nlocaltasksinfile>0) {
          lfile=filenr;
          lrank=0;
          break;
        }
      }

      DPRINTFP((32,"sion_paropen_mapped_generic",rank,"init to first local rank in first file  (%d, %d)\n",lfile,lrank));

      if((sion_filedesc_sub) && (lrank>=0) && (lfile>=0)) {
	DPRINTFP((32,"sion_paropen_mapped_generic",rank,"sion_filedesc_master->mapping_size=%d\n",sion_filedesc_master->mapping_size));

	sion_filedesc_master->ntotaltasksinfile = sion_filedesc_master->mapping_size;
	sion_filedesc_master->endianness = sion_filedesc_sub->endianness;
        sion_filedesc_master->swapbytes  = sion_filedesc_sub->swapbytes;
        sion_filedesc_master->globalrank = sion_filedesc_sub->all_globalranks[lrank];
	sion_filedesc_master->fileversion        = sion_filedesc_sub->fileversion;
	sion_filedesc_master->filesionversion    = sion_filedesc_sub->filesionversion;
	sion_filedesc_master->filesionpatchlevel = sion_filedesc_sub->filesionpatchlevel;
        sion_filedesc_master->rank       = lrank;
        sion_filedesc_master->fsblksize  = sion_filedesc_sub->fsblksize;
        sion_filedesc_master->filenumber = lfile;

        /* set info for current rank and position */
        sion_filedesc_master->chunksize      = sion_filedesc_sub->all_chunksizes[lrank];
        sion_filedesc_master->startpos       = sion_filedesc_sub->all_startpointers[lrank];
        sion_filedesc_master->currentpos     = sion_filedesc_master->startpos;
        sion_filedesc_master->globalskip     = sion_filedesc_sub->globalskip;

        sion_filedesc_master->currentblocknr = 0;
        sion_filedesc_master->lastchunknr    = sion_filedesc_sub->all_blockcount[lrank]-1;
        
	sion_filedesc_master->start_of_varheader = sion_filedesc_sub->start_of_varheader;


	/* set maxusedchunks to maxusedchunks of all files */
	sion_filedesc_master->maxusedchunks  = sion_filedesc_sub->maxusedchunks;
	for(filenr=0;filenr<sion_filedesc_sub->nfiles;filenr++) {
	  if (sion_filedesc_master->maxusedchunks < sion_filedesc_master->multifiles[filenr]->maxusedchunks)
	    sion_filedesc_master->maxusedchunks  = sion_filedesc_master->multifiles[filenr]->maxusedchunks;
	}
	_sion_realloc_filedesc_blocklist(sion_filedesc_master, sion_filedesc_master->maxusedchunks);
	for (blknum = 0; blknum < sion_filedesc_sub->all_blockcount[lrank]; blknum++) {
	  sion_filedesc_master->blocksizes[blknum] = sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->nlocaltasksinfile * blknum + lrank];
	}

       
        /* set file pointer */
        sion_filedesc_master->fileptr = sion_filedesc_master->multifiles[lfile]->fileptr;
        
        /* set position */
        _sion_file_flush(sion_filedesc_master->fileptr);

        DPRINTFP((32,"sion_paropen_mapped_generic",rank,"set startpointer to %d\n",(int) sion_filedesc_master->currentpos));
        _sion_file_set_position(sion_filedesc_master->fileptr, sion_filedesc_master->currentpos);
      
      }
    } /* block */

      /* OUTPUT parameters */
    if(fileptr!=NULL) {
      if ( (sion_filedesc_master->fileptr!=NULL) && (sion_filedesc_master->fileptr->flags&&SION_FILE_FLAG_ANSI) ) {
	*fileptr=sion_filedesc_master->fileptr->fileptr;
	sion_filedesc_master->fileptr_exported=1;
      } else {
	*fileptr=NULL;
	sion_filedesc_master->fileptr_exported=0;
      }
    }  
      
    {
      sion_int64 *helpptr_chunksize       = NULL;
      sion_int32 *helpptr_mapping_filenrs = NULL;
      sion_int32 *helpptr_mapping_lranks  = NULL;
	

      if (chunksizes != NULL) {
	if ((*chunksizes) == NULL) {
	  DPRINTFP((32,"sion_paropen_mapped_generic:",rank,"allocate chunksizes field for parameter size=%d\n",(int) *nlocaltasks ));
	  helpptr_chunksize = (sion_int64 *) malloc(*nlocaltasks * sizeof(sion_int64));
	  if (helpptr_chunksize == NULL) {
	    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure of size %lu (chunksizes), aborting ...\n", (unsigned long) sizeof(sion_int64)));
	  }
	  *chunksizes = helpptr_chunksize;
	} else {
	  helpptr_chunksize = *chunksizes;
	}
      }

      if (mapping_filenrs != NULL) {
	if ((*mapping_filenrs) == NULL) {
	  DPRINTFP((32,"sion_paropen_mapped_generic:",rank,"allocate mapping_filenrs field for parameter size=%d\n",(int) *nlocaltasks ));
	  helpptr_mapping_filenrs = (sion_int32 *) malloc(*nlocaltasks * sizeof(sion_int32));
	  if (helpptr_mapping_filenrs == NULL) {
	    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure of size %lu (mapping_filenrs), aborting ...\n", (unsigned long) sizeof(sion_int32)));
	  }
	  *mapping_filenrs = helpptr_mapping_filenrs;
	} else {
	  helpptr_mapping_filenrs = *mapping_filenrs;
	}
      }

      if (mapping_lranks != NULL) {
	if ((*mapping_lranks) == NULL) {
	  DPRINTFP((32,"sion_paropen_mapped_generic:",rank,"allocate mapping_lranks field for parameter size=%d\n",(int) *nlocaltasks ));
	  helpptr_mapping_lranks = (sion_int32 *) malloc(*nlocaltasks * sizeof(sion_int32));
	  if (helpptr_mapping_lranks == NULL) {
	    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure of size %lu (mapping_lranks), aborting ...\n", (unsigned long) sizeof(sion_int32)));
	  }
	  *mapping_lranks = helpptr_mapping_lranks;
	} else {
	  helpptr_mapping_lranks = *mapping_lranks;
	}
      }


      if(helpptr_chunksize || helpptr_mapping_filenrs || helpptr_mapping_lranks) {
	for(filenr=0;filenr<sion_filedesc_master->nfiles;filenr++) {
	  sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];
	  for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile;ltask++) {
	    for(task=0;task<*nlocaltasks;task++) {
	      if((*globalranks)[task]==sion_filedesc_sub->all_globalranks[ltask]) {
		DPRINTFP((32,"sion_paropen_mapped_generic:",rank," set chunksizes[%d]=%4d (filenr=%d, ltask=%d lrank=%d)\n",
			  (int) task, (int) sion_filedesc_sub->all_chunksizes[ltask],filenr,ltask, sion_filedesc_sub->all_localranks[ltask]));
		if(helpptr_chunksize)         helpptr_chunksize[task]       = sion_filedesc_sub->all_chunksizes[ltask];
		if(helpptr_mapping_filenrs)   helpptr_mapping_filenrs[task] = filenr;
		if(helpptr_mapping_lranks)    helpptr_mapping_lranks[task]  = sion_filedesc_sub->all_localranks[ltask];
	      }
	    }
	  }
	}
      }

    }	/* block */

    _sion_print_filedesc(sion_filedesc_master, 512, "_sion_paropen_mapped_generic", _SION_DEBUG_PRINT_ALL|_SION_DEBUG_PRINT_RECURSIVE);

     
    if(sion_count) free(sion_count);
      
    if(lfilemanager) free(lfilemanager);

    

  } else {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_multi_mpi: unknown file mode"));
  }

  
  return (sid);
}

int _sion_parclose_mapped_generic(   int    sid,
				     int    rank,
				     int    ntasks,
				     _sion_generic_gendata *sion_gendata ) {
  int rc=SION_SUCCESS;
  int lfile, grank, lrank, blknum, filenr, tmpsize, root, ltask, mappingroot, task;
  _sion_filedesc *sion_filedesc_master;
  _sion_filedesc *sion_filedesc_sub;
  sion_int64 *sion_tmpintfield_send = NULL;
  sion_int64 *sion_tmpintfield_lrank_recv = NULL;
  sion_int64 *sion_tmpintfield_data_recv = NULL;
  sion_int64 *sion_tmpintfield_data = NULL;
  sion_int32 *mapping = NULL;
  sion_int32 *sion_tmpint32field_send = NULL;
  sion_int32 *sion_tmpint32field_data_recv = NULL;
  sion_int32 *sion_count = NULL;
  sion_int32 helpint32;

  DPRINTFP((2, "_sion_parclose_mapped_generic", rank, "enter parallel close  sid=%d\n", sid));

  if ((_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc_master = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: invalid sion_filedesc_master, aborting %d ...\n", sid));
  }

#ifdef SION_DEBUG
  {
    int numbytes, numfds;
    sion_get_sizeof(sid, &numbytes, &numfds);
    DPRINTFP((2, "_sion_parclose_mapped_generic", rank, "internal data size of sid %2d (%d bytes, %d fds) \n", sid,numbytes, numfds));
  }
#endif  

  if ((sion_filedesc_master->state != SION_FILESTATE_PAROPENMAPPED)
      && (sion_filedesc_master->state != SION_FILESTATE_PAROPENMAPPEDMASTER) 
      && (sion_filedesc_master->state != SION_FILESTATE_PAROPENMAPPEDMANAGED) 
      ) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc_master->rank,"sion_parclose_mapped: invalid file open state (!PAROPENMAPPED), aborting %d ...", sid));
  }

  /* READ MODE: close files on all tasks  */
  if (sion_filedesc_master->mode == SION_FILEMODE_READ) {

    _sion_print_filedesc(sion_filedesc_master, 512, "_sion_parclose_mapped_generic", _SION_DEBUG_PRINT_ALL|_SION_DEBUG_PRINT_RECURSIVE);

    if(sion_filedesc_master->state == SION_FILESTATE_PAROPENMAPPEDMASTER) {


      /* pointer to keyval structure -> all_keyvalptr */
      if(sion_filedesc_master->keyvalmode!=SION_KEYVAL_NONE) {
	lfile=sion_filedesc_master->filenumber;
	lrank=sion_filedesc_master->rank; /* index to local list */
	if((lrank>=0) && (lfile>=0)) {	  
	  sion_filedesc_sub=sion_filedesc_master->multifiles[lfile];
	  sion_filedesc_sub->all_keyvalptr[lrank] = sion_filedesc_master->keyvalptr;
	}
      }

      /* loop again over files to close and collect mapping data*/
      for(filenr=0;filenr<sion_filedesc_master->nfiles;filenr++) {
    	sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];
	
	DPRINTFP((32, "_sion_parclose_mapped_generic", rank, " parallel close (read mode)  call fclose on file %s (fileptr=%x)\n", sion_filedesc_sub->fname,sion_filedesc_sub->fileptr));
	if (sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc_sub->keyvalptr = NULL;
	if(sion_filedesc_sub->fileptr!=NULL) {
	  _sion_file_close(sion_filedesc_sub->fileptr);
	  sion_filedesc_sub->fileptr = NULL;
	}
	sion_filedesc_sub->state = SION_FILESTATE_CLOSE;
	if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE && (sion_filedesc_sub->keyvalptr!=NULL) && (sion_filedesc_sub->all_keyvalptr!=NULL)) 
	  sion_filedesc_sub->all_keyvalptr[sion_filedesc_sub->rank] = sion_filedesc_sub->keyvalptr;

	_sion_free_filedesc(sion_filedesc_sub);
      }
    }
    _SION_SAFE_FREE(mapping, NULL);
    _SION_SAFE_FREE(sion_filedesc_master->multifiles, NULL);

    /* the top-level keyvalptr is a pointer to one of the all_keyvalptr of the subfiles, which are already freed  */
    if(sion_filedesc_master->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc_master->keyvalptr = NULL;

    _sion_free_filedesc(sion_filedesc_master);
    sion_filedesc_master = NULL;

  } else {
    /* WRITE MODE: gather data from all tasks and close files on all tasks  */

    /* update meta data of current rank on master */
    _sion_flush_block(sion_filedesc_master);
    
    if (sion_filedesc_master->usebuffer) {
      _sion_buffer_flush(sion_filedesc_master);
    }

    _sion_print_filedesc(sion_filedesc_master, 512, "_sion_parclose_mapped_generic", _SION_DEBUG_PRINT_ALL|_SION_DEBUG_PRINT_RECURSIVE);

    /* transfer meta data to corresponding sub datastructure */
    lfile=sion_filedesc_master->filenumber;
    lrank=sion_filedesc_master->rank; /* index to local list */
    sion_filedesc_sub=sion_filedesc_master->multifiles[lfile];

    /* pointer to keyval structure */
    if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE)  sion_filedesc_sub->keyvalptr = sion_filedesc_master->keyvalptr;

    sion_filedesc_sub->currentpos     = sion_filedesc_master->currentpos;
    sion_filedesc_sub->currentblocknr = sion_filedesc_master->currentblocknr;
    sion_filedesc_sub->lastchunknr    = sion_filedesc_master->lastchunknr;

    /* pointer to keyval structure -> all_keyvalptr */
    if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE)  sion_filedesc_sub->all_keyvalptr[lrank] = sion_filedesc_sub->keyvalptr;

    DPRINTFP((4, "_sion_parclose_mapped_generic", rank, "on file %d: sub,maxchunk=%d master,maxchunk=%d \n", lfile,sion_filedesc_sub->maxchunks, sion_filedesc_master->maxchunks));
    if(sion_filedesc_sub->maxchunks  < sion_filedesc_master->maxchunks) {
      _sion_realloc_filedesc_blocklist(sion_filedesc_sub, sion_filedesc_master->maxchunks);
    }

    /* store data of current rank on sub datastructure */
    DPRINTFP((4, "_sion_parclose_mapped_generic", rank, "store current information lrank=%d lastchunknr=%d\n", lrank,sion_filedesc_sub->lastchunknr));
    sion_filedesc_sub->all_currentpos[lrank]     = sion_filedesc_sub->currentpos;
    sion_filedesc_sub->all_currentblocknr[lrank] = sion_filedesc_sub->lastchunknr;
    sion_filedesc_sub->all_blockcount[lrank]     = sion_filedesc_sub->lastchunknr + 1;
    for (blknum = 0; blknum <= sion_filedesc_sub->lastchunknr; blknum++) {
      sion_filedesc_sub->blocksizes[blknum] = sion_filedesc_master->blocksizes[blknum];
      sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->ntasks * blknum + lrank] = sion_filedesc_master->blocksizes[blknum];
    }

    sion_count = (sion_int32 *) malloc(ntasks * sizeof(sion_int32));
    if (sion_count == NULL) {
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_paropen_mapped_generic: cannot allocate temporary memory of size %lu (sion_count), aborting ...\n",
                              (unsigned long) ntasks * sizeof(sion_int32)));
    }

    /* loop over all files */
    for(filenr=0;filenr<sion_filedesc_master->nfiles;filenr++) {

      sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];
      DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " starting close for file %d: %s \n", filenr,sion_filedesc_sub->fname));

      if (sion_filedesc_sub->usebuffer) {
	_sion_buffer_flush(sion_filedesc_sub);
      }

      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPED) {
        DPRINTFP((32, "_sion_parclose_mapped_generic", rank, ">parallel close (write mode, not managed)  call fclose on file %s (fileptr=%x)\n", sion_filedesc_sub->fname,sion_filedesc_sub->fileptr));
        _sion_file_close(sion_filedesc_sub->fileptr);
        DPRINTFP((32, "_sion_parclose_mapped_generic", rank, "<parallel close (write mode, not managed)  call fclose on file %s (fileptr=%x)\n", sion_filedesc_sub->fname,sion_filedesc_sub->fileptr));
        sion_filedesc_sub->fileptr = NULL;
        sion_filedesc_sub->state = SION_FILESTATE_CLOSE;

      }

      sion_gendata->apidesc->barrier_cb(sion_gendata->comm_data_global);

      /* allocate send field */
      tmpsize=sion_filedesc_sub->nlocaltasksinfile;
      sion_tmpintfield_send = (sion_int64 *) malloc(tmpsize * sizeof(sion_int64));
      if (sion_tmpintfield_send == NULL) {
	return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield_send), aborting ...\n",
				(unsigned long) tmpsize * sizeof(sion_int64)));
      }
      DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " file %d: allocate send field for all local tasks of file (%d) --> %x\n", filenr, tmpsize, sion_tmpintfield_send));

      root=sion_filedesc_sub->filemanagedbytask;
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {

        /* allocate receive fields */
        tmpsize=sion_filedesc_sub->ntotaltasksinfile;
        sion_tmpintfield_lrank_recv = (sion_int64 *) malloc(tmpsize * sizeof(sion_int64));
        if (sion_tmpintfield_lrank_recv == NULL) {
          return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield_lrank_recv), aborting ...\n",
                                  (unsigned long) tmpsize * sizeof(sion_int64)));
        }
        sion_tmpintfield_data_recv = (sion_int64 *) malloc(tmpsize * sizeof(sion_int64));
        if (sion_tmpintfield_data_recv == NULL) {
          free(sion_tmpintfield_lrank_recv);
          return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield_data_recv), aborting ...\n",
                                  (unsigned long) tmpsize * sizeof(sion_int64)));
        }
        sion_tmpintfield_data = (sion_int64 *) malloc(tmpsize * sizeof(sion_int64));
        if (sion_tmpintfield_data == NULL) {
          free(sion_tmpintfield_data_recv);
          return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic: cannot allocate temporary memory of size %lu (sion_tmpintfield_data), aborting ...\n",
                                  (unsigned long) tmpsize * sizeof(sion_int64)));
        }
        DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " file %d: allocate fields for all tasks of file (%d)\n", filenr,tmpsize));


      } else sion_tmpintfield_lrank_recv=sion_tmpintfield_data_recv=sion_tmpintfield_data=NULL;

      /* get number of tasks writing to this file */
      helpint32 = sion_filedesc_sub->nlocaltasksinfile;
      sion_gendata->apidesc->gatherr_cb(&helpint32, sion_count, sion_gendata->comm_data_global, _SION_INT32, 1, root);
      
      /* collect local rank mapping */
      for (ltask = 0; ltask < sion_filedesc_sub->nlocaltasksinfile; ltask++) {
        sion_tmpintfield_send[ltask]=sion_filedesc_sub->all_localranks[ltask];
      }
      DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " file %d: before call of gathervr_cb %x %x %x %d %d %d\n",
		filenr,sion_tmpintfield_lrank_recv, sion_tmpintfield_send, sion_gendata->comm_data_global, _SION_INT64, sion_filedesc_sub->nlocaltasksinfile, root));

      sion_gendata->apidesc->gathervr_cb(sion_tmpintfield_send, sion_tmpintfield_lrank_recv, sion_gendata->comm_data_global, _SION_INT64, sion_count, sion_filedesc_sub->nlocaltasksinfile, root);
      
      /* collect number of written chunks (blocks) for each file task */
      for (ltask = 0; ltask < sion_filedesc_sub->nlocaltasksinfile; ltask++) {
	DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " send all_blockcount[%d]: %d\n", ltask, sion_filedesc_sub->all_blockcount[ltask]));
        sion_tmpintfield_send[ltask]=sion_filedesc_sub->all_blockcount[ltask];
      }
      sion_gendata->apidesc->gathervr_cb(sion_tmpintfield_send, sion_tmpintfield_data_recv, sion_gendata->comm_data_global, _SION_INT64, sion_count, sion_filedesc_sub->nlocaltasksinfile, root);
      
      /* search maxusedchunks */
      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
        sion_filedesc_sub->maxusedchunks = -1;
        for (ltask = 0; ltask < sion_filedesc_sub->ntotaltasksinfile; ltask++) {
          if (sion_tmpintfield_data_recv[ltask] > sion_filedesc_sub->maxusedchunks) {
            sion_filedesc_sub->maxusedchunks = (int) sion_tmpintfield_data_recv[ltask];
	  }

	}
      }
      sion_gendata->apidesc->bcastr_cb(&sion_filedesc_sub->maxusedchunks, sion_gendata->comm_data_global, _SION_INT32, 1, root);
      DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " file %d: maxusedchunks=%d\n", filenr,sion_filedesc_sub->maxusedchunks));

      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {

        /* sort data  */
        for(ltask=0;ltask<sion_filedesc_sub->ntotaltasksinfile;ltask++) {
          lrank=(int) sion_tmpintfield_lrank_recv[ltask];
          DPRINTFP((64, "_sion_parclose_mapped_generic", rank, " sort in ltask=%d --> lrank=%d blkcount=%d\n",
		    ltask,lrank,sion_tmpintfield_data_recv[ltask]));
          sion_tmpintfield_data[lrank]=sion_tmpintfield_data_recv[ltask];
          
        }
        /* calculate and set start_of_varheader */
        sion_filedesc_sub->start_of_varheader = sion_filedesc_sub->start_of_data + sion_filedesc_sub->maxusedchunks * sion_filedesc_sub->globalskip;
        
        /* adjust ntasks to total number so that internal write routines work correctly */
        sion_filedesc_sub->ntasks=sion_filedesc_sub->ntotaltasksinfile;

        /* write rest of first meta data block on rank 0 */
        _sion_write_header_var_info(sion_filedesc_sub);
        
        _sion_write_header_var_part_blockcount_from_field(sion_filedesc_sub,sion_filedesc_sub->ntotaltasksinfile,sion_tmpintfield_data);
      }

      for (blknum = 0; blknum < sion_filedesc_sub->maxusedchunks; blknum++) {
   
        DPRINTFP((32, "_sion_parclose_mapped_generic", rank, " collect blocksize step %d of %d\n", blknum+1,sion_filedesc_sub->maxusedchunks));
        
        /* collect number of written chunks (blocks) for each file task */
        DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"sion_filedesc_sub->nlocaltasksinfile=%d\n",sion_filedesc_sub->nlocaltasksinfile));
        for (ltask = 0; ltask < sion_filedesc_sub->nlocaltasksinfile; ltask++) {
          sion_tmpintfield_send[ltask]=sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->nlocaltasksinfile * blknum + ltask];
          DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"sort in blksize[%d][%d]=%d\n",blknum, ltask, sion_tmpintfield_send[ltask]));
        }
        sion_gendata->apidesc->gathervr_cb(sion_tmpintfield_send, sion_tmpintfield_data_recv, sion_gendata->comm_data_global, _SION_INT64, sion_count, sion_filedesc_sub->nlocaltasksinfile, root);
        
        
        if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {
          
          /* sort data  */
          for(ltask=0;ltask<sion_filedesc_sub->ntotaltasksinfile;ltask++) {
            lrank=(int) sion_tmpintfield_lrank_recv[ltask];
            sion_tmpintfield_data[lrank]=sion_tmpintfield_data_recv[ltask];
            DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"prepare blksize[%d][%d]=%ld\n",blknum,lrank,sion_tmpintfield_data[lrank]));
          }
          
          _sion_write_header_var_part_nextblocksizes_from_field(sion_filedesc_sub,sion_filedesc_sub->ntotaltasksinfile,sion_tmpintfield_data);
          
        }

      }

      if(sion_tmpintfield_lrank_recv) free(sion_tmpintfield_lrank_recv);
      if(sion_tmpintfield_data_recv) free(sion_tmpintfield_data_recv);
      if(sion_tmpintfield_data) free(sion_tmpintfield_data);
      if(sion_tmpintfield_send) free(sion_tmpintfield_send);

      DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " ending close for file %d: %s \n", filenr,sion_filedesc_sub->fname));

    } /* for */

    /* for mapping table */
    mappingroot=sion_filedesc_master->multifiles[0]->filemanagedbytask;
    DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"mappingroot=%d rank=%d\n", mappingroot, rank ));
    if(rank==mappingroot) {
      DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"allocate mapping of size %d\n",sion_filedesc_master->mapping_size));
      mapping = (sion_int32 *) malloc(sion_filedesc_master->mapping_size * 2 * sizeof(sion_int32));
      if (mapping == NULL) {
        free(sion_count);
	return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_ABORT,"_sion_parclose_generic_mapped: Cannot allocate memory for mapping"));
      }
    }

    /* loop again over files to close and collect mapping data */
    for(filenr=0;filenr<sion_filedesc_master->nfiles;filenr++) {
      sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];

      /* allocate send field */
      tmpsize=2 * sion_filedesc_sub->nlocaltasksinfile;
      sion_tmpint32field_send = (sion_int32 *) malloc(tmpsize * sizeof(sion_int32));
      if (sion_tmpint32field_send == NULL) {
        free(mapping);
	return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic_mapped: cannot allocate temporary memory of size %lu (sion_tmpintfield_send), aborting ...\n",
				(unsigned long) tmpsize * sizeof(sion_int32)));
      }

      /* get number of tasks writing to this file */
      helpint32 = sion_filedesc_sub->nlocaltasksinfile;
      sion_gendata->apidesc->gatherr_cb(&helpint32, sion_count, sion_gendata->comm_data_global, _SION_INT32, 1, mappingroot);

      DPRINTFP((4, "_sion_parclose_mapped_generic", rank, " file %d: allocate send field  for all local tasks of file (%d) --> %x\n", filenr, tmpsize, sion_tmpint32field_send));

      if(rank==mappingroot) {
	/* allocate receive fields */
	tmpsize=2 * sion_filedesc_sub->ntotaltasksinfile;
	sion_tmpint32field_data_recv = (sion_int32 *) malloc(tmpsize * sizeof(sion_int32));
	if (sion_tmpint32field_data_recv == NULL) {
	  return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_parclose_generic_mapped: cannot allocate temporary memory of size %lu (sion_tmpintfield_data_recv), aborting ...\n",
				  (unsigned long) tmpsize * sizeof(sion_int32)));
	}
      } else sion_tmpint32field_data_recv=NULL;

      /* collect local mapping information */
      for (ltask = 0; ltask < sion_filedesc_sub->nlocaltasksinfile; ltask++) {
	sion_tmpint32field_send[ltask*2+0]= (int) sion_filedesc_sub->all_localranks[ltask];
	sion_tmpint32field_send[ltask*2+1]= (int) sion_filedesc_sub->all_globalranks[ltask];
	DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"local lrank[%d]=%d grank[%d]=%d\n",ltask,(int) sion_filedesc_sub->all_localranks[ltask],ltask,(int) sion_filedesc_sub->all_globalranks[ltask]));
      }

      for(ltask=0;ltask<sion_filedesc_sub->nlocaltasksinfile*2;ltask++) {
	DPRINTFP((32,"_sion_parclose_mapped_generic:",rank," sion_tmpint32field_send: %d -> %d \n",ltask, sion_tmpint32field_send[ltask]));
      }

      if (rank == mappingroot)  for(task=0;task<ntasks;task++) sion_count[task]*=2;
      sion_gendata->apidesc->gathervr_cb(sion_tmpint32field_send, sion_tmpint32field_data_recv, sion_gendata->comm_data_global, _SION_INT32, sion_count, 2*sion_filedesc_sub->nlocaltasksinfile, mappingroot);
      if (rank == mappingroot)  for(task=0;task<ntasks;task++) sion_count[task]/=2;

      if(rank==mappingroot) {
	/* sort data into mapping vector */
	for(ltask=0;ltask<sion_filedesc_sub->ntotaltasksinfile*2;ltask++) {
	  DPRINTFP((32,"_sion_parclose_mapped_generic:",rank," sion_tmpint32field_send: %d -> %d \n",ltask, sion_tmpint32field_data_recv[ltask]));
	}
	for(ltask=0;ltask<sion_filedesc_sub->ntotaltasksinfile;ltask++) {
	  lrank=(int) sion_tmpint32field_data_recv[ltask*2+0];
	  grank=(int) sion_tmpint32field_data_recv[ltask*2+1];
	  DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"store mapping[%d]=(%d,%d)\n",grank,filenr,lrank));
	  mapping[grank*2+0]=filenr;
	  mapping[grank*2+1]=lrank;
	}
      }

      if(sion_tmpint32field_data_recv) free(sion_tmpint32field_data_recv);
      if(sion_tmpint32field_send) free(sion_tmpint32field_send);
    }

    /* loop again over files to close and collect mapping data*/
    for(filenr=0;filenr<sion_filedesc_master->nfiles;filenr++) {
      sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];

      if(sion_filedesc_sub->state == SION_FILESTATE_PAROPENMAPPEDMANAGED) {

	/* write mapping */
	if(filenr==0) {
	  DPRINTFP((32,"_sion_parclose_mapped_generic:",rank,"mapping size is %d\n",sion_filedesc_master->mapping_size));
	  _sion_write_header_var_part_mapping(sion_filedesc_sub, sion_filedesc_master->mapping_size, mapping);
	}

	DPRINTFP((32, "_sion_parclose_mapped_generic", rank, " parallel close (write mode, managed)  call fclose on file %s (fileptr=%x)\n", sion_filedesc_sub->fname,sion_filedesc_sub->fileptr));
	_sion_file_close(sion_filedesc_sub->fileptr);
	sion_filedesc_sub->fileptr = NULL;
      }

      sion_filedesc_sub->state = SION_FILESTATE_CLOSE;
      if (sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc_sub->keyvalptr = NULL;

      /* revert ntasks to local number so that free routines work correctly */
      sion_filedesc_sub->ntasks=sion_filedesc_sub->nlocaltasksinfile;

      _sion_free_filedesc(sion_filedesc_sub);
    }
    if (sion_gendata->apidesc->free_lcg_cb && sion_gendata->comm_data_local) {
      sion_gendata->apidesc->free_lcg_cb(sion_gendata->comm_data_local);
    }
    _SION_SAFE_FREE(mapping, NULL);
    _SION_SAFE_FREE(sion_count, NULL);
    _SION_SAFE_FREE(sion_filedesc_master->multifiles, NULL);

    if (sion_filedesc_master->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc_master->keyvalptr = NULL;
    _sion_free_filedesc(sion_filedesc_master);
    sion_filedesc_master = NULL;

  } /* write */

  DPRINTFP((2, "_sion_parclose_mapped_generic", rank, "leave parallel close  sid=%d\n", sid));
    
  return (rc);
}

/*  END OF _sion_parclose_generic */

