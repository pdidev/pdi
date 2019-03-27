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
 * Common SIONlib functions
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <sys/time.h>

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
#include "sion_hints.h"
#include "sion_printts.h"
#include "sion_buffer.h"
#include "sion_dup.h"
#include "sion_lock.h"
#include "sion_buddy_common.h"


sion_io_stat_t* sion_get_io_info_buddy(int sid, int roles, int flag);

/*!
 * \defgroup sion_common SIONlib API Common
 * @{
 * Common functions of the SIONlib API
 * @}
 */

/*!
 * \ingroup sion_common
 * @brief Returns pointers to internal fields.
 *
 * See \ref sion_get_locations_description .
 *
 * @param[in]   sid                 sion file handle
 * @param[out]  ntasks              number of tasks which wrote to the sion
 *                                  file
 * @param[out]  maxchunks           maximum number of blocks in the file
 * @param[out]  globalskip          distance in bytes between the first bytes
 *                                  of two subsequent chunks of a task
 * @param[out]  start_of_varheader  start position of the meta data block at
 *                                  the end of sion file
 * @param[out]  sion_chunksizes     field containing requested chunk size of
 *                                  each task
 *                                  access: sion_chunksizes[size*chunknr+rank]
 * @param[out]  sion_globalranks    field containing global unique id of each
 *                                  task
 * @param[out]  sion_blockcount     field containing number of blocks used by
 *                                  each task
 * @param[out]  sion_blocksizes     field containing for each task and block
 *                                  the number of bytes used in this chunk e.g.
 *                                  access:
 *                                  sion_blocksizes[size*blocknr+rank]
 * @return      SION_SUCCESS if ok
 */
int sion_get_locations(int sid,
                       int *ntasks,
                       int *maxchunks,
                       sion_int64 *globalskip,
                       sion_int64 *start_of_varheader,
                       sion_int64 **sion_chunksizes,
                       sion_int64 **sion_globalranks,
                       sion_int64 **sion_blockcount,
                       sion_int64 **sion_blocksizes ) {
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;
#ifdef SION_SERIAL_MASTER
  _sion_filedesc *help;
#endif

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  DPRINTFP((1, "sion_get_locations", -1, "enter\n"));

#ifdef SION_SERIAL_MASTER
  if((sion_filedesc->state == SION_FILESTATE_SEROPENMASTER) &&
    (sion_filedesc->all_blockcount==NULL) ) {
    int i;

    /* collect info from sub files */
    _sion_alloc_filedesc_arrays(sion_filedesc);
    _sion_alloc_filedesc_block_arrays_only(sion_filedesc);

    for (i = 0; i < sion_filedesc->ntasks; i++) {
      int lfile=sion_filedesc->mapping[i*2+0];
      int lrank=sion_filedesc->mapping[i*2+1];
      sion_filedesc->all_chunksizes[i]  = sion_filedesc->multifiles[lfile]->all_chunksizes[lrank];
      sion_filedesc->all_globalranks[i] = sion_filedesc->multifiles[lfile]->all_globalranks[lrank];
      sion_filedesc->all_blockcount[i]  = sion_filedesc->multifiles[lfile]->all_blockcount[lrank];
    }
    for (i = 0; i < sion_filedesc->ntasks; i++) {
      int lfile=sion_filedesc->mapping[i*2+0];
      int lrank=sion_filedesc->mapping[i*2+1];
      int blknum;
      help=sion_filedesc->multifiles[lfile];
      for (blknum = 0; blknum < sion_filedesc->all_blockcount[i]; blknum++) {
        sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + i] =
          help->all_blocksizes[help->ntasks * blknum + lrank];
      }
    }
  }
#endif

  *ntasks             = sion_filedesc->ntasks;
  *maxchunks          = sion_filedesc->maxusedchunks;
  *sion_chunksizes    = sion_filedesc->all_chunksizes;
  *sion_globalranks   = sion_filedesc->all_globalranks;
  *sion_blockcount    = sion_filedesc->all_blockcount;
  *sion_blocksizes    = sion_filedesc->all_blocksizes;
  *globalskip         = sion_filedesc->globalskip;
  *start_of_varheader = sion_filedesc->start_of_varheader;

  DPRINTFP((1, "sion_get_locations", -1, "leave\n"));

  return (rc);
}

int _sion_close_sid(int sid)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_close: invalid sion_filedesc, aborting %d ...\n", sid));
  }
  rc=_sion_close(sion_filedesc);

  if(rc==SION_SUCCESS) {
    _sion_freevcd(sid);
    _sion_free_filedesc(sion_filedesc);
  }

  return(rc);

}

/*!
 * \ingroup sion_common
 * @brief Returns current position in file and pointer fiels containing chunk
 * sizes.
 *
 * Returns current position in file and pointer fiels containing chunk
 * sizes. This function is only needed if the sion file was opened for reading
 * in parallel mode (sion_paropen..., sion_open_rank).
 *
 * @param[in]   sid            sion file handle
 * @param[out]  currentchunknr number of current block in the file
 * @param[out]  currentpos     position in current block in the file
 * @param[out]  maxchunks      number of last block in the file
 * @param[out]  chunksizes     field containing chunk size of each task
 *
 * @return      SION_SUCCESS if ok
 */
int sion_get_current_location(int sid, int *currentchunknr, sion_int64 *currentpos, int *maxchunks, sion_int64 **chunksizes)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  DPRINTFP((1, "sion_get_current_location", -1, "enter\n"));

  *currentchunknr = sion_filedesc->currentblocknr;
  *currentpos     = sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);
  *maxchunks      = sion_filedesc->lastchunknr+1;
  *chunksizes     = sion_filedesc->blocksizes;

  DPRINTFP((1, "sion_get_current_location", -1, "leave\n"));
  return (rc);
}

/*!
 * \ingroup sion_common
 * @brief Returns pointers to the internal field mapping.
 *
 * This function is only needed if the sion file was opened for reading in
 * serial mode.
 *
 * @param[in]   sid          sion file handle
 * @param[out]  mapping_size size of mapping vector
 * @param[out]  mapping      pointer to mapping vector
 * @param[out]  numfiles     pointer to mapping vector
 *
 * @return      SION_SUCCESS if ok
 */
int sion_get_mapping(int          sid,
                     int         *mapping_size,
                     sion_int32 **mapping,
                     int         *numfiles ) {
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  DPRINTFP((1, "sion_get_mapping", -1, "enter\n"));

  if(sion_filedesc->mapping_size>0) {
    *mapping_size=sion_filedesc->mapping_size;
    *mapping=sion_filedesc->mapping;
  } else {
    *mapping_size=-1;
  }
  *numfiles=sion_filedesc->nfiles;

  DPRINTFP((1, "sion_get_mapping", -1, "leave (mapping_size=%d)\n",*mapping_size));

  return (rc);
}

/*!
 * \ingroup sion_common
 * @brief Returns edianness of data in file sid
 *
 * @param[in] sid sion file handle
 *
 * @return    1-> big endian, 0-> little endian
 */
int sion_get_file_endianness(int sid)
{
  _sion_filedesc *sion_filedesc;

  DPRINTFP((1, "sion_get_file_endianness", -1, "enter (sid=%d)\n",sid));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  DPRINTFP((1, "sion_get_file_endianness", -1, "leave (sid=%d)\n",sid));
  /* return endianness of user data */
  return ((sion_filedesc->endianness >> 8) & 1);
}

/*!
 * \ingroup sion_common
 * @brief Returns whether or not byte swapping is needed for sid
 *
 * @param[in] sid sion file handle
 *
 * @return    1-> swap needed, 0-> no swap needed
 */
int sion_endianness_swap_needed(int sid)
{
  int swap_needed = 0;

  DPRINTFP((1, "sion_endianness_swap_needed", -1, "enter (sid=%d)\n",sid));

  swap_needed = (sion_get_file_endianness(sid) != sion_get_endianness());

  DPRINTFP((1, "sion_endianness_swap_needed", -1, "leave (swap_needed=%d)\n",
            swap_needed));

  return swap_needed;
}

/*!
 * \ingroup sion_common
 * @brief Returns current position in file and pointer fiels containing chunk
 * sizes.
 *
 * Returns current position in file and pointer fiels containing chunk
 * sizes. This function is only needed if the sion file was opened for reading
 * in parallel mode (sion_paropen..., sion_open_rank).
 *
 * @param[in]   sid                 sion file handle
 * @param[out]  ntasks              number of tasks
 * @param[out]  sion_currentpos     current positions for all ranks
 * @param[out]  sion_currentblocknr current block number for all ranks
 *
 * @return      SION_SUCCESS if ok
 */
int sion_get_current_locations(int sid,
                               int *ntasks,
                               sion_int64 **sion_currentpos,
                               sion_int64 **sion_currentblocknr)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  DPRINTFP((1, "sion_get_current_locations", -1, "enter\n"));

  *ntasks = sion_filedesc->ntasks;
  *sion_currentpos = sion_filedesc->all_currentpos;
  *sion_currentblocknr = sion_filedesc->all_currentblocknr;

  DPRINTFP((1, "sion_get_current_locations", -1, "leave\n"));
  return (rc);
}

int sion_get_number_of_files(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  return (sion_filedesc->nfiles);
}

int sion_get_filenumber(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  return (sion_filedesc->filenumber);
}

int sion_is_serial_opened(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  return (sion_filedesc->state == SION_FILESTATE_SEROPEN
          || sion_filedesc->state == SION_FILESTATE_SEROPENRANK
          || sion_filedesc->state == SION_FILESTATE_SEROPENMASTER);
}

int sion_using_hints(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  return (sion_filedesc->usehints);
}

sion_int64 sion_get_bytes_written(int sid)
{
  _sion_filedesc *sion_filedesc;
  sion_int64 bytes=SION_SIZE_NOT_VALID;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  if (sion_filedesc->mode == SION_FILEMODE_WRITE) {
    int i;
    if (sion_filedesc->usebuffer) {
      _sion_buffer_flush(sion_filedesc);
    }
    _sion_flush_block(sion_filedesc);
    bytes=0;
    for(i=0;i<=sion_filedesc->lastchunknr;i++) bytes+=sion_filedesc->blocksizes[i];
  }

  return (bytes);
}

sion_int64 sion_get_bytes_read(int sid)
{
  _sion_filedesc *sion_filedesc;
  sion_int64 bytes=SION_SIZE_NOT_VALID;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  if (sion_filedesc->mode == SION_FILEMODE_READ) {
    int i;
    _sion_file_purge(sion_filedesc->fileptr);
    sion_filedesc->currentpos = _sion_file_get_position(sion_filedesc->fileptr);

    bytes=0;
    for(i=0;i<sion_filedesc->currentblocknr;i++) bytes+=sion_filedesc->blocksizes[i];
    /* current chunk */
    bytes+=sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);
  }

  return (bytes);
}

/*   return list of filenames created and the size of their payload, and the task role on this file  */
sion_io_stat_t* sion_get_io_info(int sid) {
  
  return(sion_get_io_info_by_spec(sid,SION_ROLE_COLLECTOR|SION_ROLE_WRITER|SION_ROLE_NOWRITER,SION_GET_IO_INFO_FLAG_NONE));

}

/*   return list of filenames created and the size of their payload, and the task role on this file  */
sion_io_stat_t* sion_get_io_info_by_spec(int sid, int roles, int flag) {
  _sion_filedesc *sion_filedesc;
  sion_io_stat_t *p=NULL;
  
  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid);
    return(NULL);
  }

  /* branch to buddy CP */
  if(sion_filedesc->usebuddy) {
    p=sion_get_io_info_buddy(sid,roles,flag);
  }

  return (p);
}

int sion_free_io_info(sion_io_stat_t *p) {
  return (_sion_free_io_info(p));
}


/*!
 * \ingroup sion_common
 * @brief Write data to sion file.
 *
 * See \ref sion_fwrite_description
 *
 * @param[in]  data   pointer to data to be written
 * @param[in]  size   size of a single item
 * @param[in]  nitems number of items to be written
 * @param[in]  sid    sion file id to be written to
 *
 * @return     number of elements written
 */
size_t sion_fwrite(const void *data, size_t size, size_t nitems, int sid)
{
  sion_int64 bytes_to_write, bbytes;
  _sion_filedesc *sion_filedesc;
  size_t rc=0, frc;
  void *bdata;

  DPRINTFP((1, "sion_fwrite", -1, "enter size=%ld nitems=%ld\n",(long) size, (long) nitems));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }

  _sion_check_on_collective_mode(sion_filedesc);

  bytes_to_write=size*nitems;

  /* buffering? */
  if(sion_filedesc->usebuffer) {

    /* create a new block if needed and check size of input */
    if(!sion_ensure_free_space(sid,bytes_to_write)) {
      return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
                                      "could not ensure free space for this buffered block of size %d, returning sid=%d ...",
                                      (int) bytes_to_write, sid));
    }

    char *data_ptr=(void *) data;
    sion_int64 bytes_left = sion_filedesc->chunksize -
      sion_filedesc->blocksizes[sion_filedesc->currentblocknr];
    bbytes = sion_filedesc->buffer_ptr;

    /* check for enough space in current block */
    if (bbytes + bytes_to_write > bytes_left) {
      /* not enough space for buffer + bytes_to_write => flush first */
      _sion_buffer_get_data_ptr(sion_filedesc,&bdata,&bbytes);
      if(sion_ensure_free_space(sid,bbytes)) {
        frc = _sion_file_write(bdata, bbytes, sion_filedesc->fileptr);
        if(frc != bbytes) {
          return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
                                  "could not write data (%d bytes) to file (sid=%d) ...",  (int) bbytes, sid));
        }
      } else {
        return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
                                "could not ensure free space for this buffered block of size %d, returning sid=%d ...",
                                (int) bbytes, sid));
      }
      /* increase current position */
      sion_filedesc->currentpos+=bbytes;
    }
    /* buffer data */
    sion_int64 bytes_buffered=_sion_buffer_push(sion_filedesc,data_ptr,bytes_to_write);
    bytes_to_write-=bytes_buffered;
    data_ptr+=bytes_buffered;

    while(bytes_to_write>0) {
      /* flush filled buffer to file, not all data could be stored */
      _sion_buffer_get_data_ptr(sion_filedesc,&bdata,&bbytes);
      if(sion_ensure_free_space(sid,bbytes)) {
        frc = _sion_file_write(bdata, bbytes, sion_filedesc->fileptr);
        if(frc != bbytes) {
          return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
                                  "could not write data (%d bytes) to file (sid=%d) ...",  (int) bbytes, sid));
        }
      } else {
        return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
                                "could not ensure free space for this buffered block of size %d, returning sid=%d ...",
                                (int) bbytes, sid));
      }
      /* increase current position */
      sion_filedesc->currentpos+=bbytes;

      /* next try to push data into buffer */
      bytes_buffered=_sion_buffer_push(sion_filedesc,data_ptr,bytes_to_write);
      bytes_to_write-=bytes_buffered;
      data_ptr+=bytes_buffered;
    }
    /* reset value since it will be used later */
    bytes_to_write=size*nitems;
    /* return code: bytes stored  */
    rc=(size_t) (size ? bytes_to_write / size : 0);

  } else {
    /* normal write */
    if(sion_ensure_free_space(sid,bytes_to_write)) {
      frc = _sion_file_write(data, bytes_to_write, sion_filedesc->fileptr);
      if(frc != bytes_to_write) {
        return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,
                                "could not write data (%d bytes) to file (frc=%d sid=%d) ...",  (int) bytes_to_write, (int) frc, sid));
      }

      rc=(size_t) (size ? bytes_to_write / size : 0);

    } else {
      return(_sion_errorprint_on_rank(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank,"could not ensure free space for this block, returning %d ...", sid));
    }
    /* increase current position */
    sion_filedesc->currentpos+=bytes_to_write;
  }

  DPRINTFP((1, "sion_fwrite", -1, "leave rc=%ld\n",(long) rc));
  return(rc);
}

/*!
 * \ingroup sion_common
 * @brief Read data from sion file.
 *
 * This is the basic function for reading data from the underlying
 * storage.
 *
 * @param[out] data   pointer to data to be written
 * @param[in]  size   size of a single item
 * @param[in]  nitems number of items to be written
 * @param[in]  sid    sion file id to be written to
 *
 * @return     number of elements read
 */
size_t sion_fread( void *data, size_t size, size_t nitems, int sid) {
  _sion_filedesc *sion_filedesc;
  sion_int64 bytes, bread, bytes_left;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid sion_filedesc, aborting %d ...\n", sid));
  }
  if (!data) {
    /* FIXME: replace SION_ANSI_SIZE_NOT_VALID? */
    return(_sion_errorprint(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,"invalid pointer, aborting %d ...\n", data));
  }

  /* check and update current position */
  _sion_update_fileposition(sion_filedesc);

  bytes_left = sion_filedesc->startpos +
    sion_filedesc->currentblocknr * sion_filedesc->globalskip +
    sion_filedesc->blocksizes[sion_filedesc->currentblocknr] -
    sion_filedesc->currentpos;

  DPRINTFP((1, "sion_fread", -1, "enter\n"));
  DPRINTFP((4, "sion_fread", -1, " parameter size=%ld nitems=%ld\n",(long) size,(long) nitems));

  _sion_check_on_collective_mode(sion_filedesc);

  bread=0;
  bytes=size*nitems;

  /* Check size. bytes_left == 0 is handled by sion_feof */
  if((sion_filedesc->chunksize < bytes) || ((bytes_left < bytes) &&
                                            (bytes_left != 0))) {
    /* FIXME: replace SION_ANSI_SIZE_NOT_VALID? */
    return(_sion_errorprint(SION_ANSI_SIZE_NOT_VALID,_SION_ERROR_RETURN,"not enough bytes left in chunk, aborting (%ld < %ld) ...\n", bytes_left, bytes));
  }

  if(!sion_feof(sid)) {
    bread=_sion_file_read(data,bytes,sion_filedesc->fileptr);
    DPRINTFP((256, "sion_fread", -1, "  _sion_file_read returns %ld\n",(long) bread));
  }
  if(bread!=bytes) {
    return (size_t) (size ? bread / size : 0);
  }

  bread = size ? bread / size : 0;

  /* increase current position */
  sion_filedesc->currentpos+=bytes;

  DPRINTFP((4, "sion_fread", -1, " return value  %ld\n",(long) bread));
  DPRINTFP((1, "sion_fread", -1, "leave\n"));
  return(bread);
}

/*!
 * @brief Function to set the file pointer to a new position.
 *
 * See \ref sion_seek_description .
 *
 * @param[in]   sid             sion file handle
 * @param[in]   rank            rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]   currentblocknr  block number (SION_CURRENT_BLK to select the current block)
 *                              (SION_ABSOLUTE_POS gives an absolute position;
 *                              SION_END_POS seeks relative to the end of the file)
 * @param[in]   posinblk        position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      SION_SUCCESS if file pointer can be moved to new position
 */
int sion_seek(int sid, int rank, int currentblocknr, sion_int64 posinblk)
{
  FILE *fileptr;
  int rc=0;
  /* TBD: check if not sion_open and multi file */
  rc=sion_seek_fp(sid,rank,currentblocknr,posinblk,&fileptr);
  return(rc);

}

/*!
 * @brief Deprecated. Use sion_seek() instead.
 *
 * See \ref sion_seek_fp_description .
 *
 * @param[in]   sid             sion file handle
 * @param[in]   rank            rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]   currentblocknr  block number (SION_CURRENT_BLK to select the current block)
 * @param[in]   posinblk        position in the block (SION_CURRENT_POS to select the current position)
 * @param[out]  **fileptr       file pointer to corresponding file of a multi-file set
 *
 * @retval      SION_SUCCESS if file pointer can be moved to new position
 */
int sion_seek_fp(int sid, int rank, int currentblocknr, sion_int64 posinblk, FILE **fileptr)
{
  int             rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  DPRINTFP((1, "sion_seek_fp", -1, " enter seek with sid=%d\n", sid));
  assert((_sion_vcdtype(sid) == SION_FILEDESCRIPTOR));

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: invalid sion_filedesc, aborting %d ...\n", sid));
  }

  /* check state */
  if ((sion_filedesc->mode != SION_FILEMODE_READ)  && (sion_filedesc->mode != SION_FILEMODE_WRITE)) {
      return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_seek_fp: unknown file open state !(READ|WRITE), aborting %d ...", sid));
  }
  if (sion_filedesc->usebuffer) {
    _sion_buffer_flush(sion_filedesc);
  }
  if ( (sion_filedesc->state != SION_FILESTATE_PAROPEN)
       && (sion_filedesc->state != SION_FILESTATE_SEROPEN)
       && (sion_filedesc->state != SION_FILESTATE_SEROPENMASTER)
       && (sion_filedesc->state != SION_FILESTATE_SEROPENRANK)
       && (sion_filedesc->state != SION_FILESTATE_PAROPENMAPPEDMASTER)
       ) {
      return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_seek_fp: unknown file open state !(PAR|SER|SERRANK|MAPPED), aborting %d ...", sid));
  }


  if (sion_filedesc->mode == SION_FILEMODE_READ) {  /* READ */

    if (sion_filedesc->state == SION_FILESTATE_SEROPEN) {
      rc = _sion_seek_on_all_ranks_read(sion_filedesc, rank, currentblocknr, posinblk);
    }
    else if (sion_filedesc->state == SION_FILESTATE_SEROPENMASTER) {
      rc = _sion_seek_on_all_ranks_read_master(sion_filedesc, rank, currentblocknr, posinblk);
    }
    else if ((sion_filedesc->state == SION_FILESTATE_SEROPENRANK) ||
             (sion_filedesc->state == SION_FILESTATE_PAROPEN)) {
      rc = _sion_seek_on_current_rank_read(sion_filedesc, rank, currentblocknr, posinblk);
    }
    else if (sion_filedesc->state == SION_FILESTATE_PAROPENMAPPEDMASTER) {
      rc = _sion_seek_on_all_ranks_read_mapped(sion_filedesc, rank, currentblocknr, posinblk);
    }
  } else {                                         /* WRITE */
    if (sion_filedesc->state == SION_FILESTATE_SEROPEN) {
      rc = _sion_seek_on_all_ranks_write(sion_filedesc, rank, currentblocknr, posinblk);
    }
    else if (sion_filedesc->state == SION_FILESTATE_SEROPENRANK) {
      return _sion_errorprint_on_rank(SION_NOT_SUCCESS, _SION_ERROR_RETURN, sion_filedesc->rank, "sion_seek_fp: seek not supported for this type (write, sion_open_rank), aborting ...");
    }
    else if (sion_filedesc->state == SION_FILESTATE_PAROPEN) {
      rc = _sion_seek_on_current_rank_write(sion_filedesc, rank, currentblocknr, posinblk);
    }
    else if (sion_filedesc->state == SION_FILESTATE_PAROPENMAPPEDMASTER) {
      rc = _sion_seek_on_all_ranks_write_mapped(sion_filedesc, rank, currentblocknr, posinblk);
    }

  }

  /* TBD: implement sion_open and sion_seek_fp for multi-files */
  if(fileptr!=NULL) {
    if(sion_filedesc->fileptr->flags&SION_FILE_FLAG_ANSI) {
      *fileptr=sion_filedesc->fileptr->fileptr;
      sion_filedesc->fileptr_exported=1;

    } else {
      *fileptr=NULL;
      sion_filedesc->fileptr_exported=0;
    }
  }

  DPRINTFP((1, "sion_seek_fp", -1, "leave seek rc=%d\n",rc));

  return (rc);
}

/*!
 * @brief Function that indicates whether the end of file is reached for this task.
 *
 * See \ref sion_feof_description.
 *
 * @param[in]   sid     sion file handle (in)
 *
 * @return      SION_SUCCESS (1) if end of last chunk reached
 *              SION_NOT_SUCCESS (0) otherwise
 */
int sion_feof(int sid)
{
  int       rc = SION_NOT_SUCCESS;
  sion_int64 maxpos;

  _sion_filedesc *sion_filedesc;
  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_feof: invalid sion_filedesc, aborting %d ...\n", sid));
  }

  DPRINTFP((8, "sion_feof", -1, "enter feof   sid=%d currentpos=%15lld\n", sid, sion_filedesc->currentpos));

  /* check and update current position */
  _sion_update_fileposition(sion_filedesc);

  maxpos = sion_filedesc->startpos
    + sion_filedesc->currentblocknr * sion_filedesc->globalskip + sion_filedesc->blocksizes[sion_filedesc->currentblocknr];

  DPRINTFP((16, "sion_feof", -1,
            " after flush sid=%d currentpos=%15lld maxpos= %lld startpos=%lld curblknr=%d\n",
            sid, sion_filedesc->currentpos, maxpos, sion_filedesc->startpos, sion_filedesc->currentblocknr));

  if (sion_filedesc->currentpos < maxpos) {
    /* end of current block not reached */
    rc = 0;
    DPRINTFP((16, "sion_feof", -1, " end of current block %d not reached sid=%d\n",
              sion_filedesc->currentblocknr, sid));
  } else {
    if (sion_filedesc->currentblocknr < sion_filedesc->lastchunknr) {

      /* apply hint for freeing current chunk */
      _sion_apply_hints(sion_filedesc,SION_HINTS_FREE_TYPE_CHUNK);

      /* end of current block reached, skip to next block */
      sion_filedesc->currentblocknr++;
      sion_filedesc->currentpos = sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip;

      /* apply hint for access current (new) chunk */
      _sion_apply_hints(sion_filedesc,SION_HINTS_ACCESS_TYPE_CHUNK);

      _sion_file_purge(sion_filedesc->fileptr);
      _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);
      rc = SION_NOT_SUCCESS;
      DPRINTFP((8, "sion_feof", -1,
                " end of block %d reached, skipping to next %lld -> %lld position=%lld gs=%lld sid=%d\n",
                sion_filedesc->currentblocknr - 1,
                sion_filedesc->currentpos, _sion_file_get_position(sion_filedesc->fileptr), sion_filedesc->currentpos, sion_filedesc->globalskip, sid));
    }
    else {
      /* end of last block reached */
      DPRINTFP((8, "sion_feof", -1, " end of last block %d reached sid=%d\n", sion_filedesc->currentblocknr, sid));
      rc = SION_SUCCESS;
    }
  }

  DPRINTFP((8, "sion_feof", -1, "leave feof   sid=%d currentpos=%15lld rc=%d\n", sid, sion_filedesc->currentpos, rc));

  return (rc);
}

/*!@brief Return the number of bytes available in the current chunk.
 *
 * See \ref sion_bytes_avail_in_block_description. 
 *
 * @param[in]   sid             sion file handle
 *
 * @return      rc>0  number of bytes
 *              rc<=0 file position is after end of block
 */
sion_int64 sion_bytes_avail_in_block(int sid)
{
  return(sion_bytes_avail_in_chunk(sid));
}

/*!@brief Function that returns the number of bytes available in the current chunk.
 *
 * This function returns the number of bytes remaining in the current chunk (bytes not read).
 * It is a local function, which can be called independently from other MPI tasks.
 *
 * @param[in]   sid             sion file handle
 *
 * @return      rc>0  number of bytes
 *              rc<=0 file position is after end of block
 */
sion_int64 sion_bytes_avail_in_chunk(int sid)
{
  sion_int64 maxpos;
  sion_int64 bytes_avail=SION_SIZE_NOT_VALID;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_bytes_avail_in_chunk: invalid sion_filedesc, aborting %d ...\n", sid));
  }


  _sion_update_fileposition(sion_filedesc);

  maxpos = sion_filedesc->startpos
    + sion_filedesc->currentblocknr * sion_filedesc->globalskip + sion_filedesc->blocksizes[sion_filedesc->currentblocknr];

  bytes_avail = maxpos - sion_filedesc->currentpos;

  DPRINTFP((8, "sion_bytes_avail_in_chunk", -1,
            "leave sid=%d rank=%4d  currentpos=%lld  maxpos= %lld -> bytes to read %lld\n",
            sid, sion_filedesc->rank, sion_filedesc->currentpos, maxpos, bytes_avail));

  return (bytes_avail);
}

/*!
 * @brief Function that returns the current file position
 *
 * This function returns an sion_int64 containing the current file possition.
 *
 * @param[in]   sid     sion file handle (in)
 *
 * @return      current file position
 *
 */
sion_int64 sion_get_position(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_get_position: invalid sion_filedesc, aborting %d ...\n", sid));
  }


  _sion_file_flush(sion_filedesc->fileptr);

  return (_sion_file_get_position(sion_filedesc->fileptr));

}

int sion_set_fp_closed(int sid)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_set_fp_closed: invalid sion_filedesc, aborting %d ...\n", sid));
  }
  sion_filedesc->state = SION_FILESTATE_CLOSE;
  return (rc);
}

int sion_set_second_fp(int sid, FILE *secondfd)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;


  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_set_second_fp: invalid sion_filedesc, aborting %d ...\n", sid));
  }

  if(!(sion_filedesc->fileptr->flags&SION_FILE_FLAG_ANSI)) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_set_second_fp: file was not opened in ANSI mode, aborting %d ...", sid));
  }

  _sion_file_set_second_fileptr(sion_filedesc->fileptr,secondfd);

  _sion_print_filedesc(sion_filedesc, 512, "sion_set_second_fd", 1);
  /*
  if (sion_filedesc->mode == SION_FILEMODE_READ) {
    sion_seek(sid, SION_CURRENT_RANK, 0, 0);
  }
  */
  return (rc);
}

int sion_unset_second_fp(int sid)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_unset_second_fp: invalid sion_filedesc, aborting %d ...\n", sid));
  }

  if(!(sion_filedesc->fileptr->flags&SION_FILE_FLAG_ANSI)) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_unset_second_fp: file was not opened in ANSI mode, aborting %d ...", sid));
  }

  _sion_file_unset_second_fileptr(sion_filedesc->fileptr);
  return (rc);
}

int sion_optimize_fp_buffer(int sid)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_set_second_fp: invalid sion_filedesc, aborting %d ...\n", sid));
  }

  sion_filedesc->fpbuffer      = (char *) malloc(sion_filedesc->fsblksize);
  if (sion_filedesc->fpbuffer == NULL) {
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_optimize_fp_buffer: cannot allocate internal buffer of size %lu , aborting ...", (unsigned long) sion_filedesc->fsblksize));
  }
  sion_filedesc->fpbuffer_size = sion_filedesc->fsblksize;

  rc=_sion_file_set_buffer(sion_filedesc->fileptr, sion_filedesc->fpbuffer, sion_filedesc->fpbuffer_size);
  return (rc);
}

/*!
 * @brief Flushed sion file.
 *
 * @param[in]   sid             sion file handle
 *
 * @return     SION_SUCCESS if ok
 */
int sion_flush(int sid)
{
  int       rc = SION_SUCCESS;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_unset_second_fp: invalid sion_filedesc, aborting %d ...\n", sid));
  }
  rc = _sion_flush_block(sion_filedesc);
  return (rc);
}

/*!
 * @brief Funtion to ensure that enough space is available for writing.
 *
 * See \ref sion_ensure_free_space_description
 *
 * @param[in]   sid     sion file handle
 * @param[in]   bytes   number of bytes requested for the next write operation
 *
 * @return     SION_SUCCESS if ok
 */
int sion_ensure_free_space(int sid, sion_int64 bytes)
{
  _sion_filedesc *sion_filedesc;
  sion_int64      byteswritten;
  int             rc = SION_SUCCESS;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_ensure_free_space: invalid sion_filedesc, returning %d ...\n", sid));
  }

  DPRINTFP((8, "_sion_ensure_free_space", -1, "enter ensure_free   sid=%d bytes=%lld\n", sid, bytes));

  if (sion_filedesc->mode != SION_FILEMODE_WRITE) {
    DPRINTFP((8, "_sion_ensure_free_space", -1, "invalid opened\n"));
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_ensure_free_space[%2d]: file is opened invalid sion_mode, returning ...", sion_filedesc->rank));
  }
  if (!sion_filedesc->usecoll) {
    if (sion_filedesc->fileptr == NULL) {
      DPRINTFP((8, "_sion_ensure_free_space", -1, "file not opened\n"));
      return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_ensure_free_space[%2d]: file is not open, returning ...", sion_filedesc->rank));
    }
  }

  if (bytes > sion_filedesc->chunksize) {
    DPRINTFP((8, "_sion_ensure_free_space", -1, "could not write %lld bytes, chunksize=%lld\n", bytes, sion_filedesc->chunksize));
    return(_sion_errorprint_on_rank(SION_NOT_SUCCESS,_SION_ERROR_RETURN,sion_filedesc->rank,"sion_ensure_free_space[%2d]: could not write %lld bytes, chunksize=%lld, returning ...",sion_filedesc->rank, bytes, sion_filedesc->chunksize));
  }

  _sion_flush_block(sion_filedesc);

  DPRINTFP((16, "_sion_ensure_free_space", -1,
            "  after getpos sid=%d fileposition is %lld bytes=%lld\n", sid, sion_filedesc->currentpos, bytes));

  byteswritten = sion_filedesc->blocksizes[sion_filedesc->currentblocknr];

  DPRINTFP((16, "_sion_ensure_free_space", -1,
            " sid=%d byteswritten(%lld) + new bytes (%lld) = %lld\n", sid, byteswritten, bytes, byteswritten + bytes));

  if ((byteswritten + bytes) > sion_filedesc->chunksize) {
    /* not enough space for writing next data */
    _sion_create_new_block(sion_filedesc);
  }

  DPRINTFP((8, "_sion_ensure_free_space", -1,
            "leave ensure_free   sid=%d fileposition is %lld bw+bytes=%lld <= chunksize=%lld (fsblksize=%d)\n",
            sid, sion_filedesc->currentpos, byteswritten + bytes, sion_filedesc->chunksize, sion_filedesc->fsblksize));


  return (rc);

}

int sion_is_thread_safe() {
#ifdef SION_PTHREADS
  return(SION_SUCCESS);
#else
  return(SION_NOT_SUCCESS);
#endif
}

/*!
 * @brief Function returns size of internal data structure for sid
 *
 * @param[in]    sid        sion file handle
 * @param[out]   numbytes   number of bytes used
 * @param[out]   numfds     number of file descriptors used
 *
 * @return     SION_SUCCESS if ok
 */
int sion_get_sizeof(int sid, int *numbytes, int *numfds) {
  _sion_filedesc *sion_filedesc;
  int rc=SION_NOT_SUCCESS;
  
  *numbytes=SION_SIZE_NOT_VALID;
  *numfds=SION_SIZE_NOT_VALID;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"sion_get_sizeof: invalid sion_filedesc, returning %d ...\n", sid));
  }
  DPRINTFP((2, "sion_get_sizeof", -1, "enter sid=%d\n", sid));

  rc=_sion_get_size_of_filedesc(sion_filedesc,numbytes,numfds);

  DPRINTFP((2, "sion_get_sizeof", -1, "leave sid=%d numbytes=%d numfds=%d\n", sid, *numbytes, *numfds));

  return (rc);

}


#define DFUNCTION "sion_dup"
/*!
 * @brief Function which duplicates a sion file descriptor
 *
 * Restrictions:
 *  - only in read mode
 *  - only in parallel mode
 *  - collective operation are not callable on the new sid
 *
 * @param[in]   sid             sion file handle
 * @param[in]   mode            one of SION_DUP_ALL, SION_DUP_RANK, SION_DUP_RANK_KEY
 * @param[in]   rank            dup only those part of the internal data struture which are needed to read rank _rank_ from file
 * @param[in]   key             dup only those part of the internal data struture which are needed to read on the rank _rank_ data for key _key_ from file
 *
 * @return     new sid if ok, SION_ID_NOT_VALID in other cases
 */
int sion_dup(int sid, int mode, int rank, uint64_t key)
{
  int             new_sid = SION_ID_NOT_VALID;
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_dup: invalid sion_filedesc, returning %d ...\n", sid));
  }
  DPRINTFP((8, DFUNCTION, -1, "enter sid=%d\n", sid));

  if (sion_filedesc->mode != SION_FILEMODE_READ) {
    DPRINTFP((8, DFUNCTION, -1, "invalid opened\n"));
    return(_sion_errorprint_on_rank(SION_ID_NOT_VALID,_SION_ERROR_RETURN,sion_filedesc->rank, DFUNCTION "[%2d]: file is not opened in read mode, returning ...", sion_filedesc->rank));
  }

  new_sid=_sion_dup(sid, mode, rank, key);

  DPRINTFP((8, DFUNCTION, -1, "leave sid=%d new_sid=%d\n", sid, new_sid));

  return (new_sid);

}
#undef DFUNCTION

#define DFUNCTION "sion_dedup"
/*!
 * @brief Function which destroy a duplicated sion file descriptor
 *
*
 * @param[in]   sid             sion file handle
 *
 * @return     SION_SUCCESS okay
 */
int sion_dedup(int sid)
{
  int rc = 0;
  DPRINTFP((8, DFUNCTION, -1, "enter sid=%d\n", sid));

  rc=_sion_dedup(sid);

  DPRINTFP((8, DFUNCTION, -1, "leave sid=%d rc=%d\n", sid, rc));

  return (rc);

}
#undef DFUNCTION

#define DFUNCTION "sion_lock_register_lock_callbacks"
/*!
 * @brief Function which registers callback funtions for lock and unlock internal access to shared data structures
 *
*
 * @param[in]   lock()             lock function 
 * @param[in]   unlock()           unlock function 
 * @param[in]   lock_data()           unlock function 
 *
 * @return     SION_SUCCESS okay
 */
int sion_lock_register_lock_callbacks(int lock(void *), int unlock(void *), void *lock_data) 
{
  int rc = 0;
  DPRINTFP((8, DFUNCTION, -1, "enter\n"));

  rc=_sion_lock_register_lock_callbacks(lock,unlock,lock_data);

  DPRINTFP((8, DFUNCTION, -1, "leave rc=%d\n", rc));

  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "sion_lock_user_callbacks_defined"
/*!
 * @brief Function which return flag, if user callback for locking are registered
 *
 * @return     SION_SUCCESS (yes) of SION_NOT_SUCCESS (no)
 */
int sion_lock_user_callbacks_defined() 
{
  int rc = 0;
  DPRINTFP((8, DFUNCTION, -1, "enter\n"));

  rc=_sion_lock_user_callbacks_defined();

  DPRINTFP((8, DFUNCTION, -1, "leave rc=%d\n", rc));

  return (rc);
}
#undef DFUNCTION
