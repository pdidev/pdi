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
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>

#include <sys/types.h>
#include <fcntl.h>

#include <unistd.h>

#ifdef _SION_CUDA
#include <cuda_runtime.h>
#endif

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"
#include "sion_generic_apidesc.h"
#include "sion_generic_buddy.h"
#include "sion_generic_collective.h"

/* collective I/O */
#define DFUNCTION "sion_coll_fwrite"
size_t sion_coll_fwrite(const void *data, size_t size, size_t nitems, int sid) {
  _sion_filedesc        *sion_filedesc;
  _sion_generic_gendata *sion_gendata;
  _sion_generic_apidesc *sion_apidesc;
  sion_int64             bwrote=0, spec[2], ownnewposition, items_wrote;
  int                    rc_own=SION_STD_SUCCESS,rc_cb=SION_STD_SUCCESS,rc_buddy=SION_STD_SUCCESS;
  int  collector, firstsender, lastsender;

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fwrite: invalid sion_filedesc %d", sid));
  }
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "enter usecoll=%d collector=%d collsize=%d (%d tasks, %d files)\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles));

  sion_gendata=sion_filedesc->dataptr;
  sion_apidesc=sion_gendata->apidesc;
  
  /* no collective mode */
  if(!sion_filedesc->usecoll) {
    return(sion_fwrite(data,size,nitems,sid));
  } 

  /* branch to merge mode if enabled */
  if(sion_filedesc->collmergemode) {
    return(_sion_coll_fwrite_merge(data,size,nitems,sid));
  } 
  

  /* needed for avoiding subsequent non-collective calls */
  sion_filedesc->collcmdused=1;

  /* check collsize */
  if (sion_filedesc->collsize<=0) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fwrite: collsize=%d <= 0, returning ...\n", (int) sion_filedesc->collsize));
  }

  /* parameter of callback function */
  collector=(int) sion_filedesc->collector;
  firstsender=collector+1;
  lastsender=collector+sion_filedesc->collsize-1;
  if(lastsender>sion_filedesc->ntasks) lastsender=sion_filedesc->ntasks-1;
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector=%d collsize=%d firstsender=%d lastsender=%d\n",
	    collector, sion_filedesc->collsize, firstsender, lastsender));

  /* ensure free space for this block */
  if(sion_ensure_free_space(sid,size*nitems) != SION_SUCCESS) {
    _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"could not ensure free space for this block, returning %d ...\n", sid);
    spec[0]=spec[1]=-1; 	/* signaling error */
  } else {
      spec[0]=sion_filedesc->currentpos;
      spec[1]=size*nitems;
  }

  /* write own part */
  if(sion_filedesc->rank == sion_filedesc->collector) {
    rc_own=_sion_generic_collective_process_write(data,spec,sid);
  }
  ownnewposition=sion_filedesc->currentpos;

  /* collect and write parts of sender tasks via callback function */
  if(!sion_apidesc->gather_execute_cb ) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fwrite: API %s not correctly initalized, collective I/O calls missing, aborting",sion_apidesc->name));
  }
  rc_cb=sion_apidesc->gather_execute_cb(data,spec,2, sion_filedesc->fsblksize,
					   sion_gendata->comm_data_local,collector,firstsender,lastsender,sid, 
					   _sion_generic_collective_process_write);
 
  /* set own position to end of own block written in this call */ 
  if(sion_filedesc->rank == sion_filedesc->collector) {
    _sion_file_flush(sion_filedesc->fileptr);
    _sion_file_set_position(sion_filedesc->fileptr,ownnewposition);sion_filedesc->currentpos=ownnewposition;
  }

  /* set file pointer in data structure and in file if it is exported and can be used without control of SIONlib */
  if(sion_filedesc->rank != sion_filedesc->collector) { 
    sion_filedesc->currentpos+=size*nitems;
    if(sion_filedesc->fileptr_exported) {
      _sion_file_set_position(sion_filedesc->fileptr,sion_filedesc->currentpos);
    }
  }

  /* switch to buddy checkpointing, if enabled */
  if(sion_filedesc->usebuddy ) {
    rc_buddy=_sion_coll_fwrite_buddy(data, size, nitems, sid, sion_gendata);
  }

  /* return code */
  if( (rc_own == SION_STD_SUCCESS) && (rc_cb == SION_STD_SUCCESS)&& (rc_buddy == SION_STD_SUCCESS) ) {
    bwrote=size*nitems;
  } else {
    bwrote=0;
  }

  items_wrote = size ? bwrote / size : 0;

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave usecoll=%d collector=%d collsize=%d (%d tasks, %d files) rc=%d\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles,items_wrote));

  return items_wrote;
}
#undef DFUNCTION

#define DFUNCTION "sion_coll_fread"
size_t sion_coll_fread( void *data, size_t size, size_t nitems, int sid) {
  _sion_filedesc        *sion_filedesc;
  _sion_generic_gendata *sion_gendata;
  _sion_generic_apidesc *sion_apidesc;
  sion_int64             bread=-1, spec[2], ownnewposition, items_read;
  int                    rc_own=SION_STD_SUCCESS,rc_cb=SION_STD_SUCCESS;
  int                    collector, firstsender, lastsender;

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fread: invalid sion_filedesc %d", sid));
  }
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "enter usecoll=%d collector=%d collsize=%d (%d tasks, %d files)\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles));

  sion_gendata=sion_filedesc->dataptr;
  sion_apidesc=sion_gendata->apidesc;
  
  /* no collective mode */
  if(!sion_filedesc->usecoll) {
    return(sion_fread(data,size,nitems,sid));
  } 

  /* switch to buddy checkpointing, if enabled */
  if(sion_filedesc->usebuddy ) {
    return( _sion_coll_fread_buddy(data, size, nitems, sid));
  }

  /* needed for avoiding subsequent non-collective calls */
  sion_filedesc->collcmdused=1;

    /* check collsize */
  if (sion_filedesc->collsize<=0) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fread: collsize=%d <= 0, returning ...\n", 
			    (int) sion_filedesc->collsize));
  }

  /* parameter of callback function */
  collector=(int) sion_filedesc->collector;
  firstsender=collector+1;
  lastsender=sion_filedesc->rank+sion_filedesc->collsize-1;
  if(lastsender>sion_filedesc->ntasks) lastsender=sion_filedesc->ntasks-1;
  
  /* ensure to be at the beginning of the right block */
  if(size*nitems>0) {
    if(sion_feof(sid)) {
      _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"early eof found for this block, returning %d ...\n", sid);
      spec[0]=spec[1]=-1; 	/* signaling that no data is requested */
    } else {
      /* specification of location in file  */
      spec[0]=sion_filedesc->currentpos;
      spec[1]=size*nitems;
    }
  } else {
    /* signaling that no data is requested */
    spec[0]=spec[1]=-1; 	
  }

  /* read own part */
  if(sion_filedesc->rank == sion_filedesc->collector) {
    rc_own=_sion_generic_collective_process_read(data,spec,sid);
  }
  ownnewposition=sion_filedesc->currentpos;


  /* read parts and scatter these to sender tasks via callback function */
  if(!sion_apidesc->execute_scatter_cb ) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,
			    "sion_coll_fread: API %s not correctly initalized, collective I/O calls missing, aborting",sion_apidesc->name));
  }
  rc_cb=sion_apidesc->execute_scatter_cb(data,spec,2, sion_filedesc->fsblksize,
					    sion_gendata->comm_data_local,collector,firstsender,lastsender,sid, 
					    _sion_generic_collective_process_read);

  /* set own position to end of own block read in this call */ 
  if(sion_filedesc->rank == sion_filedesc->collector) {
    _sion_file_flush(sion_filedesc->fileptr);
    _sion_file_set_position(sion_filedesc->fileptr,ownnewposition);sion_filedesc->currentpos=ownnewposition;
  }

  /* set file pointer in data structure and in file if it is exported and can be used without control of SIONlib */
  if(sion_filedesc->rank != sion_filedesc->collector) { 
    sion_filedesc->currentpos+=size*nitems;
    if(sion_filedesc->fileptr_exported) {
      _sion_file_set_position(sion_filedesc->fileptr,sion_filedesc->currentpos);
    }
  }

  if( (rc_own == SION_STD_SUCCESS) && (rc_cb == SION_STD_SUCCESS) ) {
    bread=size*nitems;
  } else {
    bread=0;
  }

  items_read = size ? bread / size : 0;

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave usecoll=%d collector=%d collsize=%d (%d tasks, %d files) rc=%d\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles,items_read));

  return items_read;
  
}
#undef DFUNCTION


#define DFUNCTION "_sion_generic_collective_process_write"
int _sion_generic_collective_process_write( const void *data, sion_int64 *spec, int sid ) {
  _sion_filedesc        *sion_filedesc;
  int                    rc=SION_STD_SUCCESS;
  sion_int64             bwrote=0, destpos, bytestowrite;
  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_collective_process_write: invalid sion_filedesc %d", sid));
  }
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "enter spec[0]=%d spec[1]=%d sid=%d\n", (int) spec[0],(int) spec[1],sid));

  /* move file pointer */
  destpos=spec[0];
  bytestowrite=spec[1];
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "currentpos=%d destpos=%d\n", (int) sion_filedesc->currentpos,(int) destpos));
  if(sion_filedesc->currentpos!=destpos)   { 
    _sion_file_flush(sion_filedesc->fileptr);
    _sion_file_set_position(sion_filedesc->fileptr,destpos);sion_filedesc->currentpos=destpos;
  } 

  /* get and write data */
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector start to write data of size %lld at position %lld\n", 
	    (long long) bytestowrite, (long long) destpos));
#ifdef _SION_CUDA
  struct cudaPointerAttributes attrs;
  cudaError_t err = cudaPointerGetAttributes(&attrs, data);
  if ((err == cudaSuccess) && (!attrs.isManaged && (attrs.memoryType == cudaMemoryTypeDevice)) ) {
    char* buffer = malloc(sion_filedesc->fsblksize);
    const char* data_ = data;
    while (bwrote < bytestowrite) {
      sion_int64 to_write = (bytestowrite - bwrote) > sion_filedesc->fsblksize ? sion_filedesc->fsblksize : (bytestowrite - bwrote);
      cudaMemcpy(buffer, data_, to_write, cudaMemcpyDeviceToHost);
      sion_int64 bwrote_ = _sion_file_write(buffer, to_write, sion_filedesc->fileptr);
      if (bwrote_ != to_write) break;
      bwrote += bwrote_;
      data_ += bwrote_;
    }
    free(buffer);
  } else {
    bwrote = _sion_file_write(data, bytestowrite, sion_filedesc->fileptr);
  }
#else
  bwrote = _sion_file_write(data, bytestowrite, sion_filedesc->fileptr);
#endif
  if(bwrote != bytestowrite) {
    return(_sion_errorprint(SION_STD_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_collective_process_write: problems writing data ...\n"));
  }

  /* update internal data */
  sion_filedesc->currentpos+=bytestowrite;
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wrote data block (bwrote=%d) of size %ld new pos %lld, %lld\n", 
	    bwrote, (long) bytestowrite, (long long) sion_filedesc->currentpos, (long long) _sion_file_get_position(sion_filedesc->fileptr)));
  

  return(rc);
  
}
#undef DFUNCTION

#define DFUNCTION "_sion_generic_collective_process_read"
int _sion_generic_collective_process_read( void *data, sion_int64 *spec, int sid ) {
  _sion_filedesc        *sion_filedesc;
  int                    rc=SION_STD_SUCCESS;
  sion_int64             bread=0, destpos, bytestoread;

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_collective_process_read: invalid sion_filedesc %d", sid));
  }
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "enter spec[0]=%d spec[1]=%d sid=%d\n", (int) spec[0],(int) spec[1],sid));

  /* move file pointer */
  destpos=spec[0];
  bytestoread=spec[1];
  if(sion_filedesc->currentpos!=destpos)   { 
    if(sion_filedesc->fileptr!=NULL) {
      _sion_file_flush(sion_filedesc->fileptr);
      _sion_file_set_position(sion_filedesc->fileptr,destpos);
    }
    sion_filedesc->currentpos=destpos;
  } 

  /* get and read data */
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector start to read data of size %lld at position %lld\n", 
	    (long long) bytestoread, (long long) destpos));
#ifdef _SION_CUDA
  struct cudaPointerAttributes attrs;
  cudaError_t err = cudaPointerGetAttributes(&attrs, data);
  if ((err == cudaSuccess) && (!attrs.isManaged && (attrs.memoryType == cudaMemoryTypeDevice)) ) {
    char* buffer = malloc(sion_filedesc->fsblksize);
    char* data_ = data;
    while (bread < bytestoread) {
      sion_int64 to_read = (bytestoread - bread) > sion_filedesc->fsblksize ? sion_filedesc->fsblksize : (bytestoread - bread);
      sion_int64 bread_ = _sion_file_read(buffer, to_read, sion_filedesc->fileptr);
      if (bread_ != to_read) break;
      cudaMemcpy(data_, buffer, bread_, cudaMemcpyHostToDevice);
      bread += bread_;
      data_ += bread_;
    }
    free(buffer);
  } else {
    bread = _sion_file_read(data, bytestoread, sion_filedesc->fileptr);
  }
#else
  bread = _sion_file_read(data, bytestoread, sion_filedesc->fileptr);
#endif
  if(bread != bytestoread) {
    return(_sion_errorprint(SION_STD_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_collective_process_read: problems reading data ...\n"));
  }

  {
    ONLY_DEBUG(char *p=data;)
    DPRINTFP((128, DFUNCTION, _SION_DEFAULT_RANK, "data[0]=%c data[%d]=%c\n",(char) p[0], (int) bytestoread, (char) p[bytestoread-1]));
  }

  /* update internal data */
  sion_filedesc->currentpos+=bytestoread;
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector read data block (bread=%d) of size %ld new pos %lld, %lld\n", 
	    bread, (long) bytestoread, (long long) sion_filedesc->currentpos, (long long) _sion_file_get_position(sion_filedesc->fileptr)));
  

  return(rc);
  
}
#undef DFUNCTION


/*!\brief checks if environment variables are set to use collective I/O
 * SION_COLLSIZE = -1 -> number of collector is computed by sionlib and depends on chunksizes and filesystem-blocksize 
 * SION_COLLSIZE =  0 -> collective is not used
 * SION_COLLSIZE >  0 -> number of task to be collected by one (master) task
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if env could be checked
 */
#define DFUNCTION "_sion_coll_check_env"
int  _sion_coll_check_env(_sion_filedesc *sion_filedesc) {
  const char *cs;
  const char *cn;
  const char *cd;
  int       rc = SION_SUCCESS, numcoll;


  cd = _sion_getenv("SION_COLLDEBUG");
  if(cd) {
    sion_filedesc->colldebug=atoi(cd);
  }

  cs = _sion_getenv("SION_COLLSIZE");
  cn = _sion_getenv("SION_COLLNUM");
  if(cs) {
    sion_filedesc->collsize=atoi(cs);
    if(sion_filedesc->collsize>sion_filedesc->ntasks) sion_filedesc->collsize=sion_filedesc->ntasks;
    if(sion_filedesc->colldebug>=1) {
      fprintf(stderr, "collective statistics:            SION_COLLSIZE=%11d\n",sion_filedesc->collsize);
    }
  } else if(cn) {
    numcoll=atoi(cn);
    if(numcoll>0) {
      if(numcoll>sion_filedesc->ntasks) numcoll=sion_filedesc->ntasks;
      sion_filedesc->collsize=sion_filedesc->ntasks/numcoll;
      if(sion_filedesc->ntasks%numcoll>0) sion_filedesc->collsize++;

      if(sion_filedesc->colldebug>=1) {
	fprintf(stderr, "collective statistics:             SION_COLLNUM=%11d\n",numcoll);
	fprintf(stderr, "collective statistics:                 collsize=%11d\n",sion_filedesc->collsize);
      }
    } 
  } 

  /* enable collective operation? */
  if((cs) || (cn)) {
    if(sion_filedesc->collsize>0) sion_filedesc->usecoll=1;
    if(sion_filedesc->collsize<0) sion_filedesc->usecoll=1;
    if(sion_filedesc->collsize==0) sion_filedesc->usecoll=0;
  }


  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "usecoll=%d collsize=%d (%d tasks, %d files) colldebug=%d\n", sion_filedesc->usecoll, sion_filedesc->collsize,sion_filedesc->ntasks,sion_filedesc->nfiles,sion_filedesc->colldebug));

  return (rc);
}
#undef DFUNCTION
