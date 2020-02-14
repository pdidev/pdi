/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
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
#include "sion_internal.h"
#include "sion_buffer.h"


#if defined(_SION_LINUX)
#elif defined(_SION_AIX)
#elif defined(_SION_BGP)
#endif


/*!
 * \file
 * 
 * This functions control and manage a memory based buffer for sion
 * files. Depending on a set of environment variables the buffer will work as
 * write-through or as write-back buffer. On Unix systems a POSIX shared memory
 * segment will be used, on Blue Gene/P it is persistent memory.
 *
 * A buffer of n MB will store the last n MB of data per task in the buffer.  If
 * the buffer memory is filled, the full buffer will be written to disk and the
 * buffer will be exhausted. 
 * 
 */

#define DFUNCTION "_sion_buffer_check_env"
/*!\brief Checks if environment variables are set to use buffer.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if env could be checked
 */
int  _sion_buffer_check_env(_sion_filedesc *sion_filedesc) {
  const char *t;
  int rc = SION_SUCCESS;
  t = _sion_getenv("SION_BUFFERSIZE");
  if(t) {
    sion_filedesc->buffer_size=atoi(t);
    if(sion_filedesc->buffer_size == -1) sion_filedesc->buffer_size=sion_filedesc->fsblksize;
  }
  DPRINTFP((2, DFUNCTION, -1, "buffersize=%d\n", sion_filedesc->buffer_size));
  _sion_buffer_init(sion_filedesc);
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_buffer_init"
/*!\brief Allocate and initalize the buffer.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if ok
 */
int  _sion_buffer_init(_sion_filedesc *sion_filedesc) {
  int rc = SION_SUCCESS;

  /* allocation */
  if(sion_filedesc->buffer_size>0) {
    sion_filedesc->buffer      = (char *) malloc(sion_filedesc->buffer_size);
    if (sion_filedesc->buffer == NULL) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_buffer_init: cannot allocate internal buffer of size %lu , aborting ...\n", (unsigned long) sion_filedesc->buffer_size));
    }
    sion_filedesc->usebuffer=1;
    DPRINTFP((2, DFUNCTION, -1, "buffer with size=%d allocated\n", sion_filedesc->buffer_size));
  }
  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_buffer_push"
/*!\brief Push data to buffer.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 * @param  *data                                pointer to data
 * @param  *bytes                               number of bytes to push
 *
 * @return number of bytes stored
 */
sion_int64 _sion_buffer_push(_sion_filedesc *sion_filedesc,
			     const void *data, sion_int64 bytes) {
  sion_int64 bytes_free;
  sion_int64 bytes_stored;
  ONLY_DEBUG(sion_int64 bytes_not_stored;)
  /* allocation */
  bytes_free=sion_filedesc->buffer_size-sion_filedesc->buffer_ptr;
  if(bytes<bytes_free) { 	/* data fits in buffer */
    bytes_stored=bytes;
    ONLY_DEBUG(bytes_not_stored=0;)
  } else {
    bytes_stored=bytes_free;
    ONLY_DEBUG(bytes_not_stored=bytes-bytes_stored;)
  }
  if(bytes_stored>0) {
#ifdef _SION_CUDA
    struct cudaPointerAttributes attrs;
    cudaError_t err = cudaPointerGetAttributes(&attrs, data);
    if ((err == cudaSuccess) && (!attrs.isManaged && (attrs.memoryType == cudaMemoryTypeDevice)) ) {
      cudaMemcpy(sion_filedesc->buffer + sion_filedesc->buffer_ptr, data, bytes_stored, cudaMemcpyDeviceToHost);
    } else {
      memcpy(sion_filedesc->buffer+sion_filedesc->buffer_ptr,data,bytes_stored);
    }
#else
    memcpy(sion_filedesc->buffer+sion_filedesc->buffer_ptr,data,bytes_stored);
#endif
    sion_filedesc->buffer_ptr+=bytes_stored;
  }

  DPRINTFP((2, DFUNCTION, -1, "pushed %d of %d bytes into buffer -> ptr=%d ret=%d\n", 
	    (int) bytes_stored,(int) bytes,
	    (int) sion_filedesc->buffer_ptr, (int) bytes_not_stored));
  return (bytes_stored);
}
#undef DFUNCTION

#define DFUNCTION "_sion_buffer_get_data_ptr"
/*!\brief Pop all data from buffer.
 *
 * @param  *sion_filedesc                       sion file description struct (_sion_filedesc)
 * @param  *data                                pointer to data
 * @param  *bytes                               number of bytes in buffer
 *
 * @return 1 if there is data to process
 *         0 not data in buffer 
 */
int _sion_buffer_get_data_ptr(_sion_filedesc *sion_filedesc,
			      void **data, sion_int64 *bytes) {
  int flag = 0;
  
  /* pointer to and size of actual buffer contents */
  *data=sion_filedesc->buffer;
  *bytes=sion_filedesc->buffer_ptr;
  flag=(*bytes>0);
  
  /* empty buffer */
  sion_filedesc->buffer_ptr=0;
  DPRINTFP((2, DFUNCTION, -1, "returns data ptr to %d bytes, flag=%d\n", 
	    (int) *bytes, flag));
  
  return (flag);
}
#undef DFUNCTION

#define DFUNCTION "_sion_buffer_flush"
/*!\brief Flush buffer.
 *
 * @param  *sion_filedesc sion file description struct (_sion_filedesc)
 *
 * @return SION_SUCCESS if ok
 */
int _sion_buffer_flush(_sion_filedesc *sion_filedesc) {
  sion_int64 bbytes, frc, byteswritten;
  void *bdata;
  

  DPRINTFP((1, DFUNCTION, -1, "enter\n"));

  _sion_flush_block(sion_filedesc);

  byteswritten = sion_filedesc->blocksizes[sion_filedesc->currentblocknr];

  _sion_buffer_get_data_ptr(sion_filedesc,&bdata,&bbytes);

  if ((byteswritten + bbytes) > sion_filedesc->chunksize) {
    /* not enough space for writing next data */
    _sion_create_new_block(sion_filedesc);
  }

  frc = _sion_file_write(bdata, bbytes, sion_filedesc->fileptr);
  if(frc != bbytes) {
    return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
				    "could not write data (%d bytes) to file (sid=%d) ...",  (int) bbytes, sion_filedesc->sid));
  }
  sion_filedesc->currentpos+=bbytes;

  DPRINTFP((2, DFUNCTION, -1, "leave\n"));
  
  return (SION_SUCCESS);
}
#undef DFUNCTION
