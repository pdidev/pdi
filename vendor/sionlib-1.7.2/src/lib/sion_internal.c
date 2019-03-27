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
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include <unistd.h>

#if defined(_SION_BGQ)
#include <stdio_ext.h>
#endif

#if defined(_SION_BGP)
#include <stdio_ext.h>
#endif

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_metadata.h"
#include "sion_filedesc.h"
#include "sion_tools.h"
#include "sion_fd.h"
#include "sion_file.h"
#include "sion_hints.h"
#include "sion_printts.h"
#include "sion_keyvalue.h"
#include "sion_buffer.h"
#include "sion_flags.h"
#include "sion_internal_startptr.h"

/* INTERNAL */

int _sion_open(const char *fname, const char *file_mode, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr)
{

  int     sid=SION_ID_UNDEF;
  _sion_flags_store* flags_store = NULL;

  DPRINTFP((1, "_sion_open", 0, "enter open of file %s in %s mode\n", fname, file_mode));

  flags_store = _sion_parse_flags(file_mode);
  if ( ! flags_store ) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_open: could not parse file mode in %s, aborting ...\n", file_mode));
  }

  if (flags_store->mask&_SION_FMODE_WRITE) {
    /* **************** WRITE mode **************** */

    sid=_sion_open_write(fname,flags_store->mask,ntasks,nfiles,chunksizes,fsblksize,globalranks,fileptr);

  } else {
    /* **************** READ mode **************** */
    sid=_sion_open_read(fname,flags_store->mask,_SION_READ_ALL_OF_MULTI_FILES,ntasks,nfiles,chunksizes,fsblksize,globalranks,fileptr);

  }

  _sion_flags_destroy_store(&flags_store);

  DPRINTFP((1, "_sion_open", 0, "leave open of file %s in %s mode sid=%d\n", fname, file_mode,sid));

  return sid;

}

/*!\brief internal sion serial open function for writing on one file
 *
 * @param[in]      fname            name of file, should equal on all tasks
 * @param[in,out]  file_mode_flags  like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in,out]  ntasks           number of tasks used to write this file
 * @param[in,out]  nfiles           number of physical files
 * @param[in,out]  chunksizes       chunksize for each task
 * @param[in,out]  fsblksize        blocksize of filesystem, must be equal on all processors
 * @param[in]      globalranks      rank numbers for which the file should be open;
 *                                  will be stored in sion file, useful if comm is not MPI_COMM_WORLD
 *                                  typical: globalrank= rank in MPI_COMM_WORLD
 * @param[out]     fileptr          filepointer for this task
 *
 * @retval              sid             sion file handle or -1 if error occured
 */
int _sion_open_write(const char *fname, sion_int64 file_mode_flags, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr)
{

  int             i, sid;
  _sion_filedesc *sion_filedesc;
  sion_int64      new_fsblocksize, apiflag;
  sion_int32      endianness = 0;
  _sion_fileptr  *sion_fileptr;

  /* check parameter */
  if (*ntasks<0) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: wrong number of tasks specific: ntasks=%d (<0), returning ...\n", *ntasks));
  }

  /* check parameter */
  if ((chunksizes==NULL) || (*chunksizes==NULL)) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: chunksizes seems not to be a pointer to an array, returning ...\n"));
  }

  /* check parameter */
  if ((globalranks==NULL) || (*globalranks==NULL)) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: globalranks seems not to be a pointer to an array, returning ...\n"));
  }

  /* check parameter */
  if (*nfiles>1) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: write with nfiles > 1 currently not supported (nfiles=%d), returning ...\n",(int) *nfiles));
  }

  /* allocate and initialise internal data structure with default values (NULL and -1) */
  sion_filedesc = _sion_alloc_filedesc();
  if (sion_filedesc == NULL) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n", (unsigned long) sizeof(sion_filedesc)));
  }
  _sion_init_filedesc(sion_filedesc);
  sion_filedesc->fname     = strdup(fname); /* Set the filename */

  /* New sion file handle */
  sid = _sion_newvcd(sion_filedesc, SION_FILEDESCRIPTOR);
  sion_filedesc->sid=sid;

  /* Allocate memory for storing MAXCHUNKS chunksize infos in internal structure */
  _sion_realloc_filedesc_blocklist(sion_filedesc, MAXCHUNKS);

  /* determine endianness */
  endianness = _sion_get_endianness_with_flags(file_mode_flags);

  DPRINTFP((1, "sion_open", 0, " it is a write operation #tasks=%d\n", *ntasks));

  sion_filedesc->state      = SION_FILESTATE_SEROPEN;  /* Serial state */
  sion_filedesc->mode       = SION_FILEMODE_WRITE;     /* Write mode */
  sion_filedesc->endianness = endianness;              /* Endianness */
  sion_filedesc->swapbytes  = 0;                       /* Endianness, swapping bytes */
  sion_filedesc->fsblksize  = *fsblksize;              /* Filesystem block size */
  sion_filedesc->ntasks     = *ntasks;                 /* Number of tasks using this file */
  sion_filedesc->nfiles     = 1;
  sion_filedesc->filenumber = 1;
  sion_filedesc->prefix     = strdup(fname);

  if (file_mode_flags&_SION_FMODE_POSIX) apiflag=SION_FILE_FLAG_POSIX;
  else                                   apiflag=SION_FILE_FLAG_ANSI;

  /* memory allocation for internal fields */
  _sion_alloc_filedesc_arrays(sion_filedesc);

  /* open file */
  sion_fileptr = _sion_file_open(fname,apiflag|SION_FILE_FLAG_WRITE|SION_FILE_FLAG_CREATE,0);
  if (!sion_fileptr) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot open %s for reading, aborting ...\n", fname));
  }
  sion_filedesc->fileptr = sion_fileptr;

  /* check fsblksize with fstat fsblksize */
  if(*fsblksize<=0) {
    new_fsblocksize=(sion_int64) _sion_file_get_opt_blksize(sion_fileptr);
    if((new_fsblocksize<0) || (new_fsblocksize>SION_MAX_FSBLOCKSIZE)) new_fsblocksize=SION_DEFAULT_FSBLOCKSIZE;
    *fsblksize = new_fsblocksize;
    sion_filedesc->fsblksize = *fsblksize;
    DPRINTFP((32, "_sion_paropen_multi_generic", 0, "setting fsblksize to %lld\n", new_fsblocksize));
  }

  /* initialize chunksizes and partnums */
  for (i = 0; i < *ntasks; i++) {
    sion_filedesc->all_chunksizes[i]  = (sion_int64) (*chunksizes)[i];
    sion_filedesc->all_globalranks[i] = (sion_int64) (*globalranks)[i];
  }

  /* check for keyval parameter, needed at this point to set flag before writing header (flag1) */
  _sion_keyval_check_env(sion_filedesc, file_mode_flags);
  if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) {
    _sion_alloc_filedesc_all_keyvalptr(sion_filedesc);
  }

  _sion_calculate_startpointers(sion_filedesc);

  /* write header */
  _sion_write_header(sion_filedesc);

  /* needed for writing pointer to var part of metadata at the end of the file */
  sion_filedesc->end_of_header = _sion_file_get_position(sion_fileptr);
  sion_filedesc->start_of_data = sion_filedesc->all_startpointers[0];

  /* initalize current positions */
  _sion_alloc_filedesc_block_arrays(sion_filedesc);
  for (i = 0; i < *ntasks; i++) {
    sion_filedesc->all_blockcount[i] = 1;
    sion_filedesc->all_currentpos[i] = sion_filedesc->all_startpointers[i];
    sion_filedesc->all_currentblocknr[i] = 0;
    sion_filedesc->all_blocksizes[0 * *ntasks + i] = 0;
  }

  /* set position to first block of rank 0 */
  sion_filedesc->rank           = 0;
  sion_filedesc->chunksize      = sion_filedesc->all_chunksizes[0];
  sion_filedesc->startpos       = sion_filedesc->all_startpointers[0];
  sion_filedesc->currentpos     = sion_filedesc->startpos;
  sion_filedesc->lastchunknr    = 0;
  sion_filedesc->currentblocknr = 0; 
  _sion_file_purge(sion_fileptr);
  _sion_file_set_position(sion_fileptr, sion_filedesc->currentpos);

  if(fileptr!=NULL) {
    if(sion_filedesc->fileptr->flags&&SION_FILE_FLAG_ANSI) {
      *fileptr=sion_filedesc->fileptr->fileptr;
      sion_filedesc->fileptr_exported=1;
    } else {
      *fileptr=NULL;
      sion_filedesc->fileptr_exported=0;
    }
  }

  _sion_print_filedesc(sion_filedesc, 512, "_sion_open_write", 1);

  return(sid);
}


/*!\brief internal sion serial open function for reading on one or more files
 *
 * @param[in]      fname            name of file, should equal on all tasks
 * @param[in,out]  file_mode_flags  like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in,out]  read_all         1 all files of a multi-file will be read, 0 only the master will read, incl. mapping
 * @param[in,out]  ntasks           number of tasks used to write this file
 * @param[in,out]  nfiles           number of physical files
 * @param[in,out]  chunksizes       chunksize for each task
 * @param[in,out]  fsblksize        blocksize of filesystem, must be equal on all processors
 * @param[in]      globalranks      rank numbers for which the file should be open;
 *                                  will be stored in sion file, useful if comm is not MPI_COMM_WORLD
 *                                  typical: globalrank= rank in MPI_COMM_WORLD
 * @param[out]     fileptr          filepointer for this task
 *
 * @return  sion file handle or -1 if error occured
 */

int _sion_open_read(const char *fname, sion_int64 file_mode_flags, int read_all, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr)
{

  int             rc, sid;
  _sion_filedesc *sion_filedesc;
  _sion_fileptr  *sion_fileptr; 
  sion_int64  apiflag;
  
  /* allocate and initialise internal data structure with default values (NULL and -1) */
  sion_filedesc = _sion_alloc_filedesc();
  if (sion_filedesc == NULL) {
    return(_sion_errorprint(SION_ID_UNDEF,SION_ID_UNDEF,"sion_open: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...\n", (unsigned long) sizeof(sion_filedesc)));
  }
  _sion_init_filedesc(sion_filedesc);
  sion_filedesc->fname     = strdup(fname); /* Set the filename */

  /* New sion file handle */
  sid = _sion_newvcd(sion_filedesc, SION_FILEDESCRIPTOR);
  sion_filedesc->sid=sid;

  if (file_mode_flags&_SION_FMODE_POSIX) apiflag=SION_FILE_FLAG_POSIX;
  else                                   apiflag=SION_FILE_FLAG_ANSI;

  DPRINTFP((1, "sion_open", 0, " it is a read operation\n"));

  /* open file */
  sion_fileptr = _sion_file_open(fname,apiflag|SION_FILE_FLAG_READ,0);
  if (!sion_fileptr) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot open %s for reading, aborting ...\n", fname));
  }
  sion_filedesc->fileptr = sion_fileptr;

  /* read part of header which does not depend on ntasks */
  rc = _sion_read_header_fix_part(sion_filedesc);
  if (rc!=SION_SUCCESS) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot read header from file %s, aborting ...\n", fname));
  }
  sion_filedesc->rank  = 0;
  sion_filedesc->state = SION_FILESTATE_SEROPEN;
  sion_filedesc->mode  = SION_FILEMODE_READ;

  /* memory allocation for internal fields */
  _sion_alloc_filedesc_arrays(sion_filedesc);
    
  /* read part of header which depends on ntasks */
  rc = _sion_read_header_var_part(sion_filedesc);
  if (rc!=SION_SUCCESS) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot read header (var part) from file %s, aborting ...\n", fname));
  }

  /* set up internal data */
  _sion_calculate_startpointers(sion_filedesc);

  /* allocate memory and read all blocksizes of all tasks from meta data 2 */
  _sion_alloc_filedesc_block_arrays(sion_filedesc);
  rc = _sion_read_header_var_part_blocksizes(sion_filedesc);
  if (rc!=SION_SUCCESS) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot read header (var part block sizes) from file %s, aborting ...\n", fname));
  }

  /* check for keyval parameter */
  _sion_keyval_check_env(sion_filedesc, file_mode_flags);
  if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) {
    _sion_alloc_filedesc_all_keyvalptr(sion_filedesc);
  }

  /* read complete mapping */
  if((sion_filedesc->nfiles>1) && (sion_filedesc->filenumber==0)) {

    if(read_all==_SION_READ_ALL_OF_MULTI_FILES) {
      sid=_sion_open_read_master(fname,file_mode_flags,ntasks,nfiles,chunksizes,fsblksize,globalranks,fileptr,sion_filedesc);
    } else {
      sid=_sion_open_read_single(fname,file_mode_flags,ntasks,nfiles,chunksizes,fsblksize,globalranks,fileptr,sion_filedesc);
      /* read mapping file, will be moved later to master file */
      _sion_read_header_var_part_mapping(sion_filedesc);       
      { int rank;
	for (rank = 0; rank < sion_filedesc->mapping_size; rank++) {
	  DPRINTFP((2048, "sion_open", 0, " mapping[%d] = %d , %d \n", rank,sion_filedesc->mapping[rank*2+0],sion_filedesc->mapping[rank*2+1]));
	}
      }
    }


  } else { 			/* not opened on master file */
    sid=_sion_open_read_single(fname,file_mode_flags,ntasks,nfiles,chunksizes,fsblksize,globalranks,fileptr,sion_filedesc);

  }   /* end single file */


  return(sid);
}

int _sion_open_read_single(const char *fname, sion_int64 file_mode_flags, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr, _sion_filedesc *sion_filedesc)
{

  int       i, sid, blknum;

  /* set info for current rank and position */
  sid=sion_filedesc->sid;
  sion_filedesc->chunksize      = sion_filedesc->all_chunksizes[sion_filedesc->rank];
  sion_filedesc->startpos       = sion_filedesc->all_startpointers[sion_filedesc->rank];
  sion_filedesc->currentpos     = sion_filedesc->startpos;
  sion_filedesc->currentblocknr = 0;
  sion_filedesc->lastchunknr    = sion_filedesc->all_blockcount[sion_filedesc->rank]-1;
  for (blknum = 0; blknum <= sion_filedesc->lastchunknr; blknum++) {
    sion_filedesc->blocksizes[blknum] = sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + sion_filedesc->rank];
  }
  _sion_file_flush(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);

  /* initalize current positions */
  for (i = 0; i < sion_filedesc->ntasks; i++) {
    sion_filedesc->all_currentpos[i] = sion_filedesc->all_startpointers[i];
    sion_filedesc->all_currentblocknr[i] = 0;
  }

      
  /* OUTPUT parameters: set parameter chunksizes and globalranks */
  *ntasks    = sion_filedesc->ntasks;
  *nfiles    = sion_filedesc->nfiles;
  *fsblksize = sion_filedesc->fsblksize;
      
  if (chunksizes != NULL) {
    sion_int64 *helpptr = NULL;
    if ((*chunksizes) == NULL) {
      helpptr = (sion_int64 *) malloc(*ntasks * sizeof(sion_int64));
      if (helpptr == NULL) {
	return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure of size %lu (chunksizes), aborting ...\n", (unsigned long) sizeof(sion_int64)));
      }
      *chunksizes = helpptr;
    } else {
      helpptr = *chunksizes;
    }
    for (i = 0; i < *ntasks; i++) {
      helpptr[i] = sion_filedesc->all_chunksizes[i];
    }
  }
  if (globalranks != NULL) {
    int      *helpptr = NULL;
    if ((*globalranks) == NULL) {
      helpptr = (int *) malloc(*ntasks * sizeof(int));
      if (helpptr == NULL) {
	return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot allocate memory of size %lu (globalranks), aborting ...\n", (unsigned long) (*ntasks) * sizeof(int)));
      }
      *globalranks = helpptr;
    } else {
      helpptr = *globalranks;
    }
    for (i = 0; i < (*ntasks); i++) {
      helpptr[i] = (int) sion_filedesc->all_globalranks[i];
    }
  }

  if(fileptr!=NULL) {
    if(sion_filedesc->fileptr->flags&&SION_FILE_FLAG_ANSI) {
      *fileptr=sion_filedesc->fileptr->fileptr;
      sion_filedesc->fileptr_exported=1;
    } else {
      *fileptr=NULL;
      sion_filedesc->fileptr_exported=0;
    }
  }

  _sion_print_filedesc(sion_filedesc, 512, "_sion_open_read_single", 1);

  return(sid);
}


int _sion_open_read_master(const char *fname, sion_int64 file_mode_flags, int *ntasks, int *nfiles, sion_int64 **chunksizes, sion_int32 *fsblksize, int **globalranks, FILE **fileptr, _sion_filedesc *sion_filedesc)
{

  int             i, sid, blknum;
  int             sid_master, subsid, lfile, lrank, filenr;
  _sion_filedesc *sion_filedesc_master,*sion_filedesc_sub;

  /* set info for current rank and position */
  sion_filedesc->chunksize      = sion_filedesc->all_chunksizes[sion_filedesc->rank];
  sion_filedesc->startpos       = sion_filedesc->all_startpointers[sion_filedesc->rank];
  sion_filedesc->currentpos     = sion_filedesc->startpos;
  sion_filedesc->currentblocknr = 0;
  sion_filedesc->lastchunknr    = sion_filedesc->all_blockcount[sion_filedesc->rank]-1;
  for (blknum = 0; blknum <= sion_filedesc->lastchunknr; blknum++) {
    sion_filedesc->blocksizes[blknum] = sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + sion_filedesc->rank];
  }
  _sion_file_flush(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);

  /* initalize current positions */
  for (i = 0; i < sion_filedesc->ntasks; i++) {
    sion_filedesc->all_currentpos[i] = sion_filedesc->all_startpointers[i];
    sion_filedesc->all_currentblocknr[i] = 0;
  }

  /* read mapping file, will be moved later to master file */
  _sion_read_header_var_part_mapping(sion_filedesc);       
  { int rank;
    for (rank = 0; rank < sion_filedesc->mapping_size; rank++) {
      DPRINTFP((32, "sion_open", 0, " mapping[%d] = %d , %d \n", rank,sion_filedesc->mapping[rank*2+0],sion_filedesc->mapping[rank*2+1]));
    }
  }

  /* create a master datastructure */
  sion_filedesc_master = _sion_alloc_filedesc();
  if (sion_filedesc_master == NULL) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot allocate filedescriptor structure of size %lu (sion_filedesc_master), aborting ...\n", 
			    (unsigned long) sizeof(sion_filedesc_master)));
  }
  _sion_init_filedesc(sion_filedesc_master);
  sion_filedesc_master->fname     = strdup(fname);                    /* Set the filename */
  sion_filedesc_master->state     = SION_FILESTATE_SEROPENMASTER;
  sion_filedesc_master->mode      = SION_FILEMODE_READ;

  /* New sion file handle */
  sid_master = _sion_newvcd(sion_filedesc_master, SION_FILEDESCRIPTOR);
  sion_filedesc_master->sid=sid_master;
	
  /* allocate vector for all datastructures */
  sion_filedesc_master->multifiles = (_sion_filedesc **) malloc(sion_filedesc->nfiles * sizeof(_sion_filedesc*));
  if (sion_filedesc_master->multifiles == NULL) {
    return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure vector of size %lu (sion_filedesc), aborting ...\n", 
			    (unsigned long) sion_filedesc->nfiles * sizeof(_sion_filedesc*)));
  }


  /* first file is already opened */
  sion_filedesc_master->multifiles[0]=sion_filedesc;
	
  for(filenr=1;filenr<sion_filedesc->nfiles;filenr++) {
    int sub_ntasts, sub_nfiles;
    sion_int32 sub_chunksize;
    FILE *sub_fileptr;
    char *multi_fname;
    multi_fname = _sion_get_multi_filename(fname, filenr);
    DPRINTFP((32, "sion_open", 0, "open now sub file = %d %s\n", filenr, multi_fname));
    subsid=_sion_open_read(multi_fname,
			   file_mode_flags, _SION_READ_ALL_OF_MULTI_FILES, &sub_ntasts, &sub_nfiles, NULL, &sub_chunksize, NULL, &sub_fileptr);
    sion_filedesc_sub=_sion_get_filedesc(subsid);
    sion_filedesc_master->multifiles[filenr]=sion_filedesc_sub;
    DPRINTFP((32, "sion_open", 0, "sub file = %d %s opened\n", filenr, multi_fname));
    free(multi_fname);
  }

  /* move mapping to master */
  sion_filedesc_master->mapping_size=sion_filedesc->mapping_size;sion_filedesc->mapping_size=-1;
  sion_filedesc_master->mapping=sion_filedesc->mapping;sion_filedesc->mapping=NULL;
	
  /* set rest of master data */
  sion_filedesc_master->rank       = 0;

  /* lookup file which contains current rank */
  lfile=sion_filedesc_master->mapping[sion_filedesc_master->rank*2+0];
  lrank=sion_filedesc_master->mapping[sion_filedesc_master->rank*2+1];
  sion_filedesc_sub=sion_filedesc_master->multifiles[lfile];

  sion_filedesc_master->ntasks     = sion_filedesc_master->mapping_size;
  sion_filedesc_master->endianness = sion_filedesc_sub->endianness;
  sion_filedesc_master->swapbytes  = sion_filedesc_sub->swapbytes;
  sion_filedesc_master->fileversion        = sion_filedesc_sub->fileversion;
  sion_filedesc_master->filesionversion    = sion_filedesc_sub->filesionversion;
  sion_filedesc_master->filesionpatchlevel = sion_filedesc_sub->filesionpatchlevel;
  sion_filedesc_master->fsblksize  = sion_filedesc_sub->fsblksize;
  sion_filedesc_master->swapbytes  = sion_filedesc_sub->swapbytes;
  sion_filedesc_master->nfiles     = sion_filedesc_sub->nfiles;
  sion_filedesc_master->flag1      = sion_filedesc_sub->flag1;
  sion_filedesc_master->flag2      = sion_filedesc_sub->flag2;
  sion_filedesc_master->keyvalmode = sion_filedesc_sub->keyvalmode;
  sion_filedesc_master->prefix     = strdup(sion_filedesc->prefix);
  sion_filedesc_master->filenumber = 0;

  /* set info for current rank and position */
  sion_filedesc_master->chunksize      = sion_filedesc_sub->all_chunksizes[lrank];
  sion_filedesc_master->startpos       = sion_filedesc_sub->all_startpointers[lrank];
  sion_filedesc_master->currentpos     = sion_filedesc_master->startpos;
  sion_filedesc_master->globalskip     = sion_filedesc_sub->globalskip;

  sion_filedesc_master->currentblocknr = 0;
  sion_filedesc_master->lastchunknr    = sion_filedesc_sub->all_blockcount[lrank]-1;
  sion_filedesc_master->start_of_varheader = sion_filedesc_sub->start_of_varheader;

  /* set maxusedchunks */
  sion_filedesc_master->maxusedchunks  = sion_filedesc->maxusedchunks;
  for(filenr=1;filenr<sion_filedesc->nfiles;filenr++) 
    if (sion_filedesc_master->maxusedchunks < sion_filedesc_master->multifiles[filenr]->maxusedchunks)
      sion_filedesc_master->maxusedchunks  = sion_filedesc_master->multifiles[filenr]->maxusedchunks;

  _sion_realloc_filedesc_blocklist(sion_filedesc_master, sion_filedesc_master->maxusedchunks);
  for (blknum = 0; blknum < sion_filedesc_sub->all_blockcount[lrank]; blknum++) {
    sion_filedesc_master->blocksizes[blknum] = sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->ntasks * blknum + lrank];
  }
	
  /* set file pointer */
  sion_filedesc_master->fileptr = sion_filedesc_master->multifiles[lfile]->fileptr;
	
  /* switch to master */
  sion_filedesc = sion_filedesc_master;
  sid           = sid_master;

  /* set position */
  _sion_file_flush(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);
	
  /* OUTPUT parameters: set parameter chunksizes and globalranks */
  *ntasks    = sion_filedesc->ntasks;
  *nfiles    = sion_filedesc->nfiles;
  *fsblksize = sion_filedesc->fsblksize;
  if (chunksizes != NULL) {
    sion_int64 *helpptr = NULL;
    if ((*chunksizes) == NULL) {
      helpptr = (sion_int64 *) malloc(*ntasks * sizeof(sion_int64));
      if (helpptr == NULL) {
	return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"cannot allocate filedescriptor structure of size %lu (chunksizes), aborting ...\n", (unsigned long) sizeof(sion_int64)));
      }
      *chunksizes = helpptr;
    } else {
      helpptr = *chunksizes;
    }
    for (i = 0; i < *ntasks; i++) {
      lfile=sion_filedesc_master->mapping[i*2+0]; lrank=sion_filedesc_master->mapping[i*2+1];
      helpptr[i] = sion_filedesc_master->multifiles[lfile]->all_chunksizes[lrank];
    }
  }
  if (globalranks != NULL) {
    int      *helpptr = NULL;
    if ((*globalranks) == NULL) {
      helpptr = (int *) malloc(*ntasks * sizeof(int));
      if (helpptr == NULL) {
	return(_sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"sion_open: cannot allocate memory of size %lu (globalranks), aborting ...\n", (unsigned long) (*ntasks) * sizeof(int)));
      }
      *globalranks = helpptr;
    }
    else {
      helpptr = *globalranks;
    }
    for (i = 0; i < (*ntasks); i++) {
      lfile=sion_filedesc_master->mapping[i*2+0]; lrank=sion_filedesc_master->mapping[i*2+1];
      helpptr[i] = sion_filedesc_master->multifiles[lfile]->all_globalranks[lrank];
    }
  }

  if(fileptr!=NULL) {
    if(sion_filedesc->fileptr->flags&&SION_FILE_FLAG_ANSI) {
      *fileptr=sion_filedesc->fileptr->fileptr;
      sion_filedesc->fileptr_exported=1;
    } else {
      *fileptr=NULL;
      sion_filedesc->fileptr_exported=0;
    }
  }

  _sion_print_filedesc(sion_filedesc, 512, "_sion_open_read_master", 1);

  return(sid);
}

int _sion_open_rank(const char *fname, const char *file_mode, sion_int64 *chunksize, sion_int32 *fsblksize, int *rank, FILE **fileptr)
{

  int             rc=SION_NOT_SUCCESS, sid=SION_ID_UNDEF;
  int             mapping_filenr, mapping_lrank;
  char           prefix = '\0';
  sion_int64     apiflag;
  _sion_filedesc *sion_filedesc;
  _sion_fileptr  *sion_fileptr; 
  _sion_flags_store* flags_store = NULL;

  /*                                                                      */ DPRINTFTS(*rank, "before open rank");
  DPRINTFP((1, "_sion_open_rank", *rank, "enter open of file %s in %s mode\n", fname, file_mode));

  /* allocate and initialise internal data structure with default values (NULL and -1) */
  sion_filedesc = _sion_alloc_filedesc();
  if (sion_filedesc == NULL) {
    return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"_sion_open_rank: cannot allocate filedescriptor structure of size %lu (sion_filedesc), aborting ...", (unsigned long) sizeof(sion_filedesc)));
  }
  _sion_init_filedesc(sion_filedesc);
  sion_filedesc->fname = strdup(fname); /* Set the filename */

  /* New sion file handle */
  sid = _sion_newvcd(sion_filedesc, SION_FILEDESCRIPTOR);
  sion_filedesc->sid=sid;

  flags_store = _sion_parse_flags(file_mode);
  if ( ! flags_store ) {
    return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"sion_paropen_mpi: could not parse file mode in %s, aborting ...", file_mode));
  }

  if (flags_store->mask&_SION_FMODE_POSIX) apiflag=SION_FILE_FLAG_POSIX;
  else                                   apiflag=SION_FILE_FLAG_ANSI;

  if (flags_store->mask&_SION_FMODE_WRITE) {
    /* **************** WRITE mode **************** */

    DPRINTFP((1, "_sion_open_rank", *rank, " it is a write operation\n"));

    _sion_flags_destroy_store(&flags_store);
    return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"_sion_open_rank: %s for writing currently not supported, aborting ...", fname));

  }
  _sion_flags_destroy_store(&flags_store);

  /* **************** READ mode **************** */
/*                                                                      */ DPRINTFTS(*rank, "start open read");
  DPRINTFP((1, "_sion_open_rank", *rank, " it is a read operation rank=%d\n",*rank));

  /* open file */
  sion_fileptr = _sion_file_open(fname,apiflag|SION_FILE_FLAG_READ,0);
  if (!sion_fileptr) {
    return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"_sion_open_rank: cannot open %s for reading, aborting ...", fname));
  }
  sion_filedesc->fileptr = sion_fileptr;
  /*                                                                      */ DPRINTFTS(*rank, "start after open file 1");
  rc = _sion_read_header_fix_part(sion_filedesc);
  /*                                                                      */ DPRINTFTS(*rank, "start after read header fix part 1");
  if (rc==SION_NOT_SUCCESS) {
    return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"_sion_open_rank: cannot read header from file %s, aborting ...", fname));
  }
  sion_filedesc->rank  = *rank;
  sion_filedesc->state = SION_FILESTATE_SEROPENRANK;
  sion_filedesc->mode  = SION_FILEMODE_READ;

  /* memory allocation for internal fields */
  _sion_alloc_filedesc_arrays(sion_filedesc);

  /* read part of header which depends on ntasks */
  rc = _sion_read_header_var_part(sion_filedesc);
  /*                                                                      */ DPRINTFTS(*rank, "start after read header var part 1");
  if (rc==SION_NOT_SUCCESS) {
    return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"_sion_open_rank: cannot read var part header from file %s, aborting ...", fname));
  }

  DPRINTFP((1, "_sion_open_rank", *rank, "found number of files %d in %s rank=%d\n", sion_filedesc->nfiles,sion_filedesc->fname,*rank));
  if(sion_filedesc->nfiles>1) {
    /* read mapping for rank */
    rc=_sion_read_header_var_part_mapping_rank(sion_filedesc);
    /*                                                                      */ DPRINTFTS(*rank, "start after read mapping rank 1");
    if (rc==SION_NOT_SUCCESS) {
      return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"cannot read var part mapping header from file %s, aborting ...", fname));
    }
    mapping_filenr = sion_filedesc->mapping[0];
    mapping_lrank  = sion_filedesc->mapping[1];
    DPRINTFP((1, "_sion_open_rank", *rank, "data for rank %d is in file %d (lrank=%d)\n", *rank, mapping_filenr,mapping_lrank));
    /*                                                                      */ DPRINTFTS(*rank, "start after read mapping 1");

    /* reopen one of other multi files if necessary */
    if(mapping_filenr>0) {

      /* close current file */
      _sion_free_filedesc_arrays(sion_filedesc);
      _sion_file_close(sion_filedesc->fileptr);
      sion_filedesc->fileptr=NULL;
      /*                                                                      */ DPRINTFTS(*rank, "start after close file 1");

      /*  and open again the correct one */
      sion_filedesc->fname=_sion_get_multi_filename(fname,mapping_filenr);

      DPRINTFP((1, "_sion_open_rank", *rank, "open file sion_filedesc->fname=%s  fname=%s rank=%d\n", sion_filedesc->fname,fname,*rank));

      sion_fileptr = _sion_file_open(sion_filedesc->fname,apiflag|SION_FILE_FLAG_READ,0);
      if (!sion_fileptr) {
        return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"_sion_open_rank: cannot open %s for reading, aborting ...", fname));
      }
      sion_filedesc->fileptr = sion_fileptr;
      /*                                                                      */ DPRINTFTS(*rank, "start after open file i");

      rc = _sion_read_header_fix_part(sion_filedesc);
      if (rc==SION_NOT_SUCCESS) {
        return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"sion_open: cannot read header from file %s, aborting ...", fname));
      }
    /*                                                                      */ DPRINTFTS(*rank, "start after read header fix part i");
      _sion_alloc_filedesc_arrays(sion_filedesc);
      rc = _sion_read_header_var_part(sion_filedesc);
      if (rc==SION_NOT_SUCCESS) {
        return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,*rank,"cannot var mapping data from file %s, aborting ...", fname));
      }
      /*                                                                      */ DPRINTFTS(*rank, "start after read header var part i");
      
    } else {
      sion_filedesc->fname              = strdup(fname);
    }
  } else {
    mapping_lrank=*rank;
  }

  DPRINTFP((1, "_sion_open_rank", *rank, "max blocks=%d\n", sion_filedesc->maxusedchunks));
  _sion_realloc_filedesc_blocklist(sion_filedesc, sion_filedesc->maxusedchunks);

  sion_filedesc->prefix             = strdup(&prefix);

  _sion_calculate_startpointers(sion_filedesc);

  sion_filedesc->rank = mapping_lrank;

  rc = _sion_read_header_var_part_blocksizes_rank(sion_filedesc);
  if (rc==SION_NOT_SUCCESS) {
    return(_sion_errorprint_on_rank(SION_ID_UNDEF,_SION_ERROR_RETURN,sion_filedesc->rank,"cannot read header from file %s, aborting ...", fname));
  }
  /*                                                                      */ DPRINTFTS(*rank, "start after read header blocksizes i");

  /* set info for current rank and position */
  sion_filedesc->chunksize  = sion_filedesc->all_chunksizes[sion_filedesc->rank];
  sion_filedesc->startpos   = sion_filedesc->all_startpointers[sion_filedesc->rank];
  sion_filedesc->currentpos = sion_filedesc->startpos;
  sion_filedesc->currentblocknr = 0;

  /* set file pointer */
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);

  /* set output parameter */
  *chunksize=sion_filedesc->chunksize;
  *fsblksize=sion_filedesc->fsblksize;

  if(fileptr!=NULL) {
    if(sion_filedesc->fileptr->flags&SION_FILE_FLAG_ANSI) {
      *fileptr=sion_filedesc->fileptr->fileptr;
      sion_filedesc->fileptr_exported=1;
    } else {
      *fileptr=NULL;
      sion_filedesc->fileptr_exported=0;
    }
  }

  _sion_print_filedesc(sion_filedesc, 512, "_sion_open_rank", 1);

  DPRINTFP((1, "_sion_open_rank", 0, "leave open of file %s in %s mode\n", fname, file_mode));
  /*                                                                      */ DPRINTFTS(*rank, "after open rank");

  return (sid);

}

/* sion_filedesc has to be freed by calling function */
int _sion_close(_sion_filedesc *sion_filedesc)
{

  int       rc = SION_SUCCESS;
  int       blknum, rank, currentrank, mapping_size;
#ifdef SION_SERIAL_MASTER	
  int filenr;
#endif



  DPRINTFP((1, "_sion_close", -1, "enter close   sid=%d currentpos=%15lld\n", sion_filedesc->sid, sion_filedesc->currentpos));
  _sion_print_filedesc(sion_filedesc, 512, "_sion_close", 1);


  if ((sion_filedesc->state != SION_FILESTATE_SEROPEN)
      && (sion_filedesc->state != SION_FILESTATE_SEROPENRANK) 
      && (sion_filedesc->state != SION_FILESTATE_SEROPENMASTER) 
      ) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"_sion_close: invalid file open state (!SEROPEN), aborting %d ...", sion_filedesc->sid));
  }

  if (sion_filedesc->mode == SION_FILEMODE_WRITE) {
    /* write rest of metadata in headers */

    DPRINTFP((1, "_sion_close", -1, " file was opened in write mode sid=%d setpos to %lld\n", sion_filedesc->sid, sion_filedesc->end_of_header));

    /* update meta data of current rank */
    _sion_flush_block(sion_filedesc);
    DPRINTFP((1, "_sion_close", -1, " after flush block sid=%d  fileptr is at position %14lld\n", sion_filedesc->sid, _sion_file_get_position(sion_filedesc->fileptr)));

    if (sion_filedesc->usebuffer) {
      _sion_buffer_flush(sion_filedesc);
    }

    /* store data of current rank */
    currentrank = sion_filedesc->rank;
    sion_filedesc->all_currentpos[currentrank]     = sion_filedesc->startpos + sion_filedesc->blocksizes[sion_filedesc->lastchunknr];
    sion_filedesc->all_currentblocknr[currentrank] = sion_filedesc->lastchunknr;
    sion_filedesc->all_blockcount[currentrank]     = sion_filedesc->lastchunknr + 1;
    for (blknum = 0; blknum <= sion_filedesc->lastchunknr; blknum++) {
      sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + currentrank] = sion_filedesc->blocksizes[blknum];
    }

    /* search and set maxusedchunks */
    sion_filedesc->maxusedchunks = -1;
    for (blknum = 0; blknum < sion_filedesc->ntasks; blknum++)
      if (sion_filedesc->all_blockcount[blknum] > sion_filedesc->maxusedchunks) 
	sion_filedesc->maxusedchunks = (int) sion_filedesc->all_blockcount[blknum];

    /* calculate and set start_of_varheader */
    sion_filedesc->start_of_varheader = sion_filedesc->start_of_data + sion_filedesc->maxusedchunks * sion_filedesc->globalskip;

    /* write rest of first meta data block */
    _sion_write_header_var_info(sion_filedesc);

    /* set blocksizes of all not used chunks to zero */
    for (blknum = 0; blknum < sion_filedesc->maxusedchunks; blknum++) {
      for (rank = 0; rank < sion_filedesc->ntasks; rank++) {
        if (blknum >= sion_filedesc->all_blockcount[rank]) {
          sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + rank] = 0;
        } else {
	  DPRINTFP((1, "_sion_close", -1,
		    " blocksize rank %2d blk %2d -> %lld\n", 
		    rank, blknum, sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + rank]));
	}
      }
    }
    
    /* write blockinfo to second meta data block */
    _sion_write_header_var_part_blocksizes(sion_filedesc);

    /* write mapping info to second meta data block */
    mapping_size = 0;
    _sion_write_header_var_part_mapping(sion_filedesc,mapping_size,NULL);

    _sion_print_filedesc(sion_filedesc, 512, "_sion_close", 1);

    /* close file and free data structure */
    _sion_file_close(sion_filedesc->fileptr);
    sion_filedesc->fileptr=NULL;

  } else {
    /* read */
    
#ifdef SION_SERIAL_MASTER	
    /* close all files if multi file */
    ;
    if(sion_filedesc->state == SION_FILESTATE_SEROPENMASTER) {
      for(filenr=1;filenr<sion_filedesc->nfiles;filenr++) {
	_sion_close(sion_filedesc->multifiles[filenr]);
	_sion_freevcd(sion_filedesc->multifiles[filenr]->sid);
	_sion_free_filedesc(sion_filedesc->multifiles[filenr]);
	sion_filedesc->multifiles[filenr]=NULL;
      }
    } else {
#endif

    /* close file and free data structure */
    _sion_file_close(sion_filedesc->fileptr);
    sion_filedesc->fileptr=NULL;
   
#ifdef SION_SERIAL_MASTER	
    }
#endif 
  }

  DPRINTFP((1, "_sion_close", -1, "leave close   sid=%d\n", sion_filedesc->sid));

  return (rc);
}

/*!\brief generates the multi filename
 *
 * @param  fname                file name
 * @param  filenumber           file number
 *
 * @retval pointer to new filename
 */
char * _sion_get_multi_filename(const char *fname, int filenumber)
{
  char *newfname;

  newfname = malloc(SION_FILENAME_LENGTH);
  if (newfname == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_get_multi_filename: Cannot allocate string newfname\n");
    return(NULL);
  }
  if(filenumber>0) {
    if(strlen(fname)<SION_FILENAME_LENGTH-7) {
      sprintf(newfname, "%s.%06d", fname, filenumber);
    } else {
      _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_get_multi_filename: filename too long\n");
      free(newfname);
      return(NULL);
    }
  } else {
    strcpy(newfname, fname);
  }

  return(newfname);
}


/*!\brief Flush the data to the disk for the current task
 *
 * @param  sid          sion file handle
 *
 * @retval SION_SUCCESS if OK
 */
int _sion_flush_file(int sid)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_flush_file: invalid sion_filedesc, aborting %d ...\n", sid));
  }
  DPRINTFP((32, "_sion_flush_file", sion_filedesc->rank, "flush sid=%d\n", sid));

  _sion_file_flush(sion_filedesc->fileptr);
  return (rc);
}


/*!\brief Update the internal data structure (check fileposition)
 *
 * Update is only performed if file pointer is exported. The body of
 * the function is skipped otherwise.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval SION_SUCCESS if OK
 */
int _sion_update_fileposition( _sion_filedesc *sion_filedesc )
{
  int       rc = SION_SUCCESS;

  DPRINTFP((2, "_sion_update_fileposition", sion_filedesc->rank, "enter\n"));
  /* only neccesary to get file position directly from system file pointer,
     if fileptr was exported to application */
  if(sion_filedesc->fileptr_exported) {

#ifdef OLD
    /* if fileptr is exported, it will be used in collective mode only from the collector tasks  */
    if( (!sion_filedesc->usecoll) || (sion_filedesc->collector==sion_filedesc->rank) ) { 
      _sion_file_flush(sion_filedesc->fileptr);
      sion_filedesc->currentpos = _sion_file_get_position(sion_filedesc->fileptr);
    } 
#endif
    /* file position is possible be changed by external (non-sion) file read/write operation, 
       therefore  the sion internal file position must be updated */
    _sion_file_flush(sion_filedesc->fileptr);
    sion_filedesc->currentpos = _sion_file_get_position(sion_filedesc->fileptr);
  }

  DPRINTFP((2, "_sion_update_fileposition", sion_filedesc->rank, "leave pos=%lld usecoll=%d collector=%d rank\n",
	    (long long) sion_filedesc->currentpos, sion_filedesc->usecoll,sion_filedesc->collector,sion_filedesc->rank));
  return (rc);
}

/*!\brief check if a collective operation are already called,  
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval SION_SUCCESS if OK
 */
int _sion_check_on_collective_mode( _sion_filedesc *sion_filedesc) {
  int       rc = SION_SUCCESS;

  /* don't check if collective merge  */
  /* branch to merge mode if enabled */
  if(sion_filedesc->collmergemode) {
    return(rc);
  } 


  /* this functions checks in non-collective call after a collective one was called
     this function ensures that buffer are flushed accordantly */
  /* if(sion_filedesc->fileptr_exported) { */
    if(sion_filedesc->usecoll) {
      sion_filedesc->usecoll=0; 
      if((!sion_filedesc->collector) && (sion_filedesc->collcmdused)) {
	_sion_file_flush(sion_filedesc->fileptr);
	_sion_file_set_position(sion_filedesc->fileptr,sion_filedesc->currentpos);;
      }
      return(rc);
      /* return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_WARN,"_sion_check_on_collective_mode: individual SION read/write function call in collective mode, switching back to no-collective mode")); */
    }
  /* } */
  return(rc);
}


/*!\brief Update the internal data structure
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @retval 1 if OK
 */
int _sion_flush_block( _sion_filedesc *sion_filedesc )
{
  int       rc = SION_SUCCESS;
  sion_int64 byteswritten;

  DPRINTFP((2, "_sion_flush_block", sion_filedesc->rank, "enter fileposition=%lld\n", sion_filedesc->currentpos));

  _sion_update_fileposition(sion_filedesc);

  DPRINTFP((2, "_sion_flush_block", sion_filedesc->rank, "after update fileposition=%lld\n", sion_filedesc->currentpos));

  byteswritten = sion_filedesc->currentpos  
    - ( sion_filedesc->startpos 
	+ sion_filedesc->currentblocknr * sion_filedesc->globalskip);
  
  if (byteswritten > 0) {
    sion_filedesc->blocksizes[sion_filedesc->currentblocknr] = byteswritten;

    DPRINTFP((2, "_sion_flush_block", sion_filedesc->rank,
              "flushed lastchunknr->%d currentblocknr=%d byteswritten=%lld fileposition=%lld startpos=%lld (%lld)\n", 
	      sion_filedesc->lastchunknr, sion_filedesc->currentblocknr,
              byteswritten, sion_filedesc->currentpos, sion_filedesc->startpos, sion_filedesc->blocksizes[sion_filedesc->currentblocknr]));
  } else {
    rc = SION_NOT_SUCCESS;
    DPRINTFP((2, "_sion_flush_block", sion_filedesc->rank, "not flushed lastchunknr->%d byteswritten=%lld fileposition=%lld startpos=%lld  (%lld)\n",
              sion_filedesc->lastchunknr, byteswritten, sion_filedesc->currentpos, sion_filedesc->startpos, sion_filedesc->blocksizes[sion_filedesc->currentblocknr]));
  }

  DPRINTFP((2, "_sion_flush_block", sion_filedesc->rank, "leave\n"));

  return (rc);
}

/*!\brief Create a new block for the internal data structure
 *
 * @param  sion_filedesc  sion file handle
 *
 * @retval  SION_SUCCESS if OK
 */
int _sion_create_new_block( _sion_filedesc *sion_filedesc )
{
  int       rc = SION_SUCCESS;

  DPRINTFP((2, "_sion_create_new_block", _SION_DEFAULT_RANK, "enter alloc \n"));
  
  if(sion_filedesc->currentblocknr<sion_filedesc->lastchunknr) {
    /* in-between write, skip to next block and overwrite it */

    /* flush current block */
    _sion_flush_block(sion_filedesc);
    
    /* apply hint for freeing current chunk */
    _sion_apply_hints(sion_filedesc,SION_HINTS_FREE_TYPE_CHUNK);

    sion_filedesc->currentblocknr++;
    sion_filedesc->currentpos  = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip;

    /* apply hint for access current (new) chunk */
    rc = _sion_apply_hints(sion_filedesc,SION_HINTS_ACCESS_TYPE_CHUNK);

    /* advance fp to next block */
    if(sion_filedesc->fileptr) {
      _sion_file_flush(sion_filedesc->fileptr);
      rc = _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);
    }
    
    DPRINTFP((2, "_sion_create_new_block", _SION_DEFAULT_RANK, "skip to next already allocated block currentblocknr=%d lastchunknr->%d currentpos=%lld\n", 
	      sion_filedesc->currentblocknr, sion_filedesc->lastchunknr, sion_filedesc->currentpos));
    
  } else {
    /* at-end write, skip to next block and overwrite it */

    /* flush current block */
    _sion_flush_block(sion_filedesc);

    /* adjust arraysize if necessary */
    if ((sion_filedesc->lastchunknr + 1) >= sion_filedesc->maxchunks) {
      _sion_realloc_filedesc_blocklist(sion_filedesc, sion_filedesc->maxchunks + MAXCHUNKS);
    }

    /* apply hint for freeing current chunk */
    _sion_apply_hints(sion_filedesc,SION_HINTS_FREE_TYPE_CHUNK);

    sion_filedesc->lastchunknr++;
    sion_filedesc->currentblocknr++;
    sion_filedesc->currentpos  = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip;
    if(sion_filedesc->lastchunknr+1>sion_filedesc->maxusedchunks) sion_filedesc->maxusedchunks=sion_filedesc->lastchunknr+1;
 
    /* apply hint for access current (new) chunk */
    rc = _sion_apply_hints(sion_filedesc,SION_HINTS_ACCESS_TYPE_CHUNK);

    /* advance fp to next block */
    if(sion_filedesc->fileptr) {
      _sion_file_flush(sion_filedesc->fileptr);
      rc = _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);
    }
    DPRINTFP((2, "_sion_create_new_block", _SION_DEFAULT_RANK, "skip to new allocated block currentblocknr=%d lastchunknr->%d currentpos=%lld\n", 
	      sion_filedesc->currentblocknr, sion_filedesc->lastchunknr, sion_filedesc->currentpos));

  }

  return (rc);
}

/*!\brief Return endianness including possible choice via flags
 *
 *  return endianness
 */
sion_int32 _sion_get_endianness_with_flags(sion_int64 flags) {
  sion_int32 endianness = 0;
  
  DPRINTFP((2, "_sion_get_endianness_flags", -1, "enter with flags 0x%x\n", flags));

  /* determine endianness */
  endianness = sion_get_endianness();
  /* make value symmetric (first byte = last byte) -> endianness independent */
  endianness |= endianness << 24;
  if (flags & _SION_FMODE_ENDIANNESS_SET) {
    /* manually set endianness on middle bytes */
    if (flags & _SION_FMODE_ENDIANNESS_BIG) {
      endianness |= 0x0101 << 8;
    }
  }
  else if (endianness & 1) {
    /* no manual setting of endianness: set middle bytes matching the out ones */
    endianness |= 0x0101 << 8;
  }
  
  DPRINTFP((2, "_sion_get_endianness_flags", -1, "leave with endianness 0x%x\n", endianness));

  return endianness;
}

#define DFUNCTION "_sion_getenv"
/*! encapsulate getenv
 *
 */
char *_sion_getenv(const char *name) {
  char *name_with_prefix = NULL;
  char *getenv_result = NULL;
  const int full_len = strlen(name) + strlen(SION_LVERSION_PREFIX) + 1;

  DPRINTFP((2, DFUNCTION, -1, "enter: name = %s\n", name));

  name_with_prefix = (char *)malloc(full_len);
  sprintf(name_with_prefix, "%s%s", SION_LVERSION_PREFIX, name);

  getenv_result = getenv(name_with_prefix);
  free(name_with_prefix);

  DPRINTFP((2, DFUNCTION, -1, "leave: getenv_result = %s\n", getenv_result));

  return getenv_result;
}
#undef DFUNCTION

/*!\brief allocates an io_info data structure for nfiles files
 *
 *  return flags
 #   p: pointer to data structure
 */
sion_io_stat_t* _sion_alloc_io_info(int p_nf) {
  sion_io_stat_t *p=NULL;
  int i;

  /* allocate data structure */
  p = (sion_io_stat_t *) malloc(sizeof(sion_io_stat_t));
  if (p == NULL) {
    _sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_alloc_io_info: cannot allocate memory of size %lu (p_fn), aborting ...\n",
		     (unsigned long) sizeof(sion_io_stat_t));
    return(NULL);
  }
  
  p->nfiles=p_nf;

  /* allocate vectors */
  p->names = (const char **) malloc(p_nf * sizeof(char *));
  if (p->names == NULL) {
    _sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_alloc_io_info: cannot allocate memory of size %lu (names), aborting ...\n",
		     (unsigned long) p_nf * sizeof(char *));
    free(p);
    return(NULL);
  }
  
  p->sizes = (size_t *) malloc(p_nf * sizeof(size_t));
  if (p->sizes == NULL) {
    _sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_alloc_io_info: cannot allocate memory of size %lu (sizes), aborting ...\n",
		     (unsigned long) p_nf * sizeof(size_t));
    free(p->names);
    free(p);
    return(NULL);
  }

  p->roles = (int *) malloc(p_nf * sizeof(int));
  if (p->roles == NULL) {
    _sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_alloc_io_info: cannot allocate memory of size %lu (roles), aborting ...\n",
		     (unsigned long) p_nf * sizeof(int));
    free(p->names);
    free(p->sizes);
    free(p);
    return(NULL);
  }
  /* init fields */
  for(i=0;i<p_nf;i++) {
    p->names[i]=NULL;
    p->sizes[i]=0;
    p->roles[i]=SION_ROLE_NONE;
  }

  return(p);
}

/*!\brief frees an io_info data structure 
 *
 *  return flags: 
 *    rc
 */
int _sion_free_io_info(sion_io_stat_t *p) {
  int rc=SION_SUCCESS;

  if(p->names!=NULL) free(p->names);
  if(p->sizes!=NULL) free(p->sizes);
  if(p->roles!=NULL) free(p->roles);
  
  free(p);
  
  return(rc);
}
