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
#include <fcntl.h>

#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"
#include "sion_generic_apidesc.h"
#include "sion_generic_collective_merge.h"

/* callback functions */
int _sion_generic_collective_process_write_merge( const void *data, sion_int64 *spec, int sid );


/* collective I/O */
#define DFUNCTION "_sion_coll_fwrite_merge"
size_t _sion_coll_fwrite_merge(const void *data, size_t size, size_t nitems, int sid) {
  _sion_filedesc        *sion_filedesc;
  _sion_generic_gendata *sion_gendata;
  _sion_generic_apidesc *sion_apidesc;
  sion_int64             bwrote=0, spec[2];
  int                    rc_own=SION_STD_SUCCESS,rc_cb=SION_STD_SUCCESS;
  int                    collector, firstsender, lastsender;

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fwrite: invalid sion_filedesc %d", sid));
  }
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "enter usecoll=%d collector=%d collsize=%d (%d tasks, %d files)\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles));

  sion_gendata=sion_filedesc->dataptr;
  sion_apidesc=sion_gendata->apidesc;
  
  /* needed for avoiding subsequent non-collective calls */
  sion_filedesc->collcmdused=1;

  /* check collsize */
  if (sion_filedesc->collsize<=0) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fwrite: collsize=%d <= 0, returning ...\n", (int) sion_filedesc->collsize));
  }

  /* parameter of callback function */
  collector=(int) sion_filedesc->collector;
  firstsender=collector+1;
  lastsender=sion_filedesc->rank+sion_filedesc->collsize-1;
  if(lastsender>sion_filedesc->ntasks) lastsender=sion_filedesc->ntasks-1;

  spec[0]=-2; 			/* signaling that data should be written in chunk of collector */
  spec[1]=size*nitems;

  /* write own part */
  if(sion_filedesc->rank == sion_filedesc->collector) {
    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "COLLECTOR write data of size %ld at current position\n", (long) spec[1]));
    rc_own=_sion_generic_collective_process_write_merge(data,spec,sid);
  } else {
    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "SENDER send data of size %ld at current position\n", (long) spec[1]));
  }


  /* collect and write parts of sender tasks via callback function */
  if(!sion_apidesc->gather_execute_cb ) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_coll_fwrite: API %s not correctly initalized, collective I/O calls missing, aborting",sion_apidesc->name));
  }
  rc_cb=sion_apidesc->gather_execute_cb(data,spec,2, sion_filedesc->fsblksize,
					sion_gendata->comm_data_local,collector,firstsender,lastsender,sid, 
					_sion_generic_collective_process_write_merge);
  
  /* return code */
  if( (rc_own == SION_STD_SUCCESS) && (rc_cb == SION_STD_SUCCESS) ) {
    bwrote=size*nitems;
  } else {
    bwrote=0;
  }

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave usecoll=%d collector=%d collsize=%d (%d tasks, %d files) rc=%d\n", 
	    sion_filedesc->usecoll, sion_filedesc->collector, sion_filedesc->collsize, sion_filedesc->ntasks,sion_filedesc->nfiles,bwrote));

  return(bwrote);
}
#undef DFUNCTION


#define DFUNCTION "_sion_generic_collective_process_write_merge"
int _sion_generic_collective_process_write_merge( const void *data, sion_int64 *spec, int sid ) {
  _sion_filedesc        *sion_filedesc;
  int                    rc=SION_STD_SUCCESS;
  sion_int64             bwrote=0, bytestowrite;
  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_collective_process_write: invalid sion_filedesc %d", sid));
  }
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "enter spec[0]=%d spec[1]=%d sid=%d\n", (int) spec[0],(int) spec[1],sid));

  /* move file pointer */
  bytestowrite=spec[1];

  /* ensure free space for this block */
  if(sion_ensure_free_space(sid,bytestowrite) != SION_SUCCESS) {
    _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"could not ensure free space for this block, returning %d ...\n", sid);
  } 

  /* get and write data */
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector start to write data of size %lld at position %lld\n", 
	    (long long) bytestowrite, (long long) sion_filedesc->currentpos));
  bwrote = _sion_file_write(data, bytestowrite, sion_filedesc->fileptr);
  if(bwrote != bytestowrite) {
    return(_sion_errorprint(SION_STD_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_collective_process_write: problems writing data ...\n"));
  }

  /* update internal data */
  sion_filedesc->currentpos+=bytestowrite;
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wrote data block (bwrote=%d) of size %ld new pos %lld, %lld\n", 
	    bwrote, (long) bytestowrite, (long long) sion_filedesc->currentpos, (long long) _sion_file_get_position(sion_filedesc->fileptr)));
  

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave spec[0]=%d spec[1]=%d sid=%d rc=%d\n", (int) spec[0],(int) spec[1],sid,rc));
  return(rc);
  
}
#undef DFUNCTION

