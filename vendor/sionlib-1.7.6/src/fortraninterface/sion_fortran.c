/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
/*!
 * @file sion_fortran.c
 *
 * @brief Fortran API
 *
 * @author Ventsislav Petkov
 * @date 14.08.2008
 * @date 03.05.2013 modifications to support different Fortran interfaces, Florian Janetzko
 */

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* #define _FORTRANUNDERSCORE
   #define _FORTRANNOCAPS
   #define _FORTRANCAPS
   #define _FORTRANDOUBLEUNDERSCORE */
#include "sion.h"
#include "sion_debug.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_fortran.h"

#define DFUNCTION "fsion_open_c"
/*!
 * @brief Fortran procedure to open a sion file in serial mode.
 *
 * This function opens a sion file in serial mode
 *
 * @param[in]           fname           name of file, should equal on all tasks
 * @param[in,out]       file_mode       like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in]           ntasks          number of tasks used to write this file
 * @param[in,out]       nfiles          number of physical files
 * @param[in,out]       chunksizes      chunksize for each task
 * @param[in,out]       fsblksize       blocksize of filesystem, must be equal on all processors
 * @param[in]           globalranks     rank numbers for which the file should be open;
 *                                      will be stored in sion file, usefull if comm is not MPI_COMM_WORLD
 *                                      typical: globalrank= rank in MPI_COMM_WORLD
 * @param[in]           fname_len       (internal) length of the fname string
 * @param[in]           file_mode_len   (internal) length of the file_mode string
 *
 * @retval              sid             sion file handle or -1 if error occured
 *
 */
void fsion_open_c(char *fname,
        	  char *file_mode,
        	  int  *ntasks, 
        	  int  *nfiles, 
		  sion_int64 *chunksizes,
		  sion_int32 *fsblksize, 
		  int *globalranks, 
		  int *sid, 
		  int fname_len, 
		  int file_mode_len)
{
  /* Fortran strings are not NULL-terminated => insert \0 at the end */
  char     *fname_tmp, *fmode_tmp;
  

  fname_tmp = (char *) malloc((size_t) ((fname_len + 1) * sizeof(char)));
  if(fname_tmp == NULL) {
    fprintf(stderr, "could not allocate memory for internal filename buffer, returning ...\n");
    *sid=-1;
    return;
  }

  fmode_tmp = (char *) malloc((size_t) ((file_mode_len + 1) * sizeof(char)));
  if(fmode_tmp == NULL) {
    fprintf(stderr, "could not allocate memory for internal filemode buffer, returning ...\n");
    *sid=-1;
    free(fname_tmp); fname_tmp=NULL;
    return;
  }


  /* printf("sizeof(fname_len)=%d\n",sizeof(fname_len));  */
  /* printf("%s fname_len=%d %x %d\n",fname, fname_len,fname_tmp,((fname_len + 1) * sizeof(char)));  */
  /* printf("file_mode_len=%d %x %d\n",file_mode_len,fmode_tmp,((file_mode_len + 1) * sizeof(char)));  */

  /* Copy the strings to the new buffers and pad with nulls */
  strncpy(fname_tmp, fname, fname_len);
  strncpy(fmode_tmp, file_mode, file_mode_len);

  fname_tmp[fname_len] = '\0';
  fmode_tmp[file_mode_len] = '\0';

  

  DPRINTFP((2, DFUNCTION, -1, "params: %s %s %d %ld \n", fname_tmp, fmode_tmp, (int) *ntasks, (long) *fsblksize)); 

  if (!strcmp(fmode_tmp, "bw") || !strcmp(fmode_tmp, "wb")) {
    /* **************** WRITE mode **************** */
    (*sid) = sion_open(fname_tmp, fmode_tmp, ntasks, nfiles, &chunksizes, fsblksize, &globalranks, NULL);
  } else {
    (*sid) = sion_open(fname_tmp, fmode_tmp, ntasks, nfiles, NULL, fsblksize, NULL, NULL);
  }
  DPRINTFP((2, DFUNCTION, -1, "sid: %d\n", *sid));

  /* Free the used memory */
  free(fname_tmp); fname_tmp=NULL;
  free(fmode_tmp); fmode_tmp=NULL;
}
#undef DFUNCTION


/*!
 * @brief Fortran procedure to open a sion file for a specific rank
 *
 * This function opens a sion file for a specific rank.  It can be
 * used to open the sion file independently from each task.
 * (e.g. if no MPI is available or only a subset of tasks chunks are needed)
 *
 * Using this function the meta data at beginning of the sion file are read
 * from each task instead of read once and distribute (sion_open).
 * sion_open_rank reads only the tasks specific meta data  from the meta data
 * block at the end of the sion file.
 *
 * Warning: Only read operations are currently supported.
 *
 * @param[in]           fname           name of file, should equal on all tasks
 * @param[in,out]       file_mode       like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in,out]       chunksize       chunksize for this task
 * @param[in,out]       fsblksize       blocksize of filesystem, must be equal on all processors
 * @param[in]           rank            rank number for which the file should be open;
 *                                      will be stored in sion file, usefull if comm is not MPI_COMM_WORLD
 *                                      typical: globalrank= rank in MPI_COMM_WORLD
 * @param[in]           fname_len       (internal) length of the fname string *
 * @param[in]           file_mode_len   (internal) length of the file_mode string
 *
 * @retval              sid             sion file handle or -1 if error occured
 */
void fsion_open_rank_c(char *fname,
                       char *file_mode, 
                       sion_int64 *chunksize, 
                       sion_int32 *fsblksize, 
                       int *rank, 
                       int *sid, 
                       int fname_len, 
                       int file_mode_len)
{
  /* Fortran strings are not NULL-terminated => insert \0 at the end */
  char     *fname_tmp, *fmode_tmp;

  fname_tmp = (char *) malloc((size_t) ((fname_len + 1) * sizeof(char)));
  fmode_tmp = (char *) malloc((size_t) ((file_mode_len + 1) * sizeof(char)));
  /* Copy the strings to the new buffers and pad with nulls */
  strncpy(fname_tmp, fname, fname_len);
  strncpy(fmode_tmp, file_mode, file_mode_len);
  fname_tmp[fname_len] = '\0';
  fmode_tmp[file_mode_len] = '\0';

  (*sid) = sion_open_rank(fname_tmp, fmode_tmp, chunksize, fsblksize, rank, NULL);

  /* Free the used memory */
  free(fname_tmp);
  free(fmode_tmp);
}

/*!
 * @brief Fortran procedure to close a sion file.
 *
 * This procedure closes a sion file which was opened in serial mode with sion_open or sion_open_rank.
 * In Write mode this function will also save all meta data to the meta data block of the sion file
 * at the beginning and the end of t
 * he file.
 * The function is a task local function, which can be called independently from other MPI tasks.
 *
 * @param[in]   sid     sion file handle
 *
 * @param[out]  ierr    1 if close is ok
 */
void fsion_close_c(int *sid, 
                   int *ierr)
{
  (*ierr) = sion_close(*sid);
}

/*!
 * @brief Fortran function that indicates the end of file for this task.
 *
 * This function indicates if the end of file is reached for this task.
 * This means that the file pointer is behind of the last bytes of last chunk of this task.
 * If all bytes of the current chunk are already read and there are more chunks available
 * for this task, sion_feof will advance the filepointer to the start position of the
 * next chunk in the sion file.
 * The function is a task local function, which can be called independently from other MPI tasks.
 *
 * @param[in]   sid     sion file handle
 * @param[out]  eof     1 if the End of the File is reached
 *
 * @retval      1 if end of last chunk reached
 *              0 otherwise
 */
void fsion_feof_c(int *sid, 
                  int *eof)
{

  *eof = sion_feof(*sid);
}

#define DFUNCTION "fsion_seek_c"
/*!
 * @brief Fortran procedure to set the file pointer to a new position.
 *
 * This procedure sets the file pointer to a new position according to the specified parameters.
 * It can only be used when the sion file was opened for reading in serial mode.
 * In Write mode currently only the rank can be modified!
 * SION_CURRENT_BLK and SION_CURRENT_POS are required
 *
 * @param[in]   sid             sion file handle
 * @param[in]   rank            rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]   currentblocknr  block number (SION_CURRENT_BLK to select the current block)
 * @param[in]   posinblk        position in the block (SION_CURRENT_POS to select the current position)
 *
 * @param[out]  ierr            1 if file pointer can be moved to new position
 *                              0 otherwise
 */
void fsion_seek_c(int *sid, 
                  int *rank, 
                  int *currentblocknr, 
                  sion_int64 *posinblk, 
                  int *ierr)
{

  DPRINTFP((2, DFUNCTION, -1, "params: %d %d %d %lld\n", *sid, *rank, *currentblocknr, *posinblk));

  (*ierr) = sion_seek(*sid, *rank, *currentblocknr, *posinblk);
}
#undef DFUNCTION

/*!
 * @brief Fortran procedure to ensure that enough space is available.
 *
 * This procedure ensures that there is enough space available for writing numbytes bytes
 * to the sion file. It allocates a new block at the end of the sion file of size chunksize
 * if numbytes cannot be written in the current block.
 * This procedure is only be needed if the caller function from the user program does not count
 * the number of bytes written. It is a task local function, which can be called independently
 * from other MPI tasks, and it moves in some cases the filepointer to a new position and flushes
 * also the local filepointer.
 *
 * @param[in]   sid     sion file handle
 * @param[in]   bytes   number of bytes which will be written
 *
 * @param[out]  ierr    1 if ok
 */
void fsion_ensure_free_space_c(int *sid, 
                               sion_int64 *bytes, 
                               int *ierr)
{

  (*ierr) = sion_ensure_free_space(*sid, *bytes);
}



/*!
 * @brief Fortran procedure to flush a sion file.
 *
 * This fortran procedure flushes the buffers and writes the data to the sion file.
 *
 * @param[in]   sid     sion file handle
 *
 * @param[out]  ierr    1 if ok
 */
void fsion_flush_c(int *sid, 
                   int *ierr)
{

  (*ierr) = sion_flush(*sid);

}


/*!@brief Fortran function that returns the number of bytes available in the current chunk.
 *
 * This function returns the number of bytes currently available in the current chunk (bytes not read).
 * It is a local function, which can be called independently from other MPI tasks.
 *
 * @param[in]   sid             sion file handle
 *
 * @returns     INTEGER*4       rc>0  number of bytes
 *                                              rc<=0 file position is after end of block
 */
sion_int64 fsion_bytes_avail_in_block_c(int *sid)
{

  return sion_bytes_avail_in_block(*sid);
}

/*!
 * @brief Fortran procedure that returns pointers to internal fields.
 *
 * This procedure returns pointers to internal fields, containing the number of chunks written
 * by each task (sion_chunkcount) and their sizes (sion_chunksizes).
 * This function is only needed if the sion file was opened for reading in serial mode.
 *
 * @param[in]   sid                     sion file handle
 * @param[out]  ntasks                  number of tasks wrote to the sion file
 * @param[out]  maxblocks               maximum number of chunks per tasks used
 * @param[out]  globalskip              distance in bytes between the first bytes
 *                                      of two subsequent chunks of a task
 * @param[out]  start_of_varheader      start position of the meta data block at the end of sion file
 * @param[out]  sion_localsizes         field containing requested chunk size of each task
 * @param[out]  sion_globalranks        field containing global unique id of each task
 * @param[out]  sion_chunkcount         field containing number of chunks used by each task
 * @param[out]  sion_chunksizes         field containing for each task and chunk the number of bytes
 *                                                                      used in this chunk
 *                                                                      e.g. access: sion_chunksizes[rank*chunks+chunknr]
 * @param[out]  ierr                            1 if ok
 */
void fsion_get_locations_c(int *sid,
                           int *ntasks,
                           int *maxblocks,
                           sion_int64 *globalskip,
                           sion_int64 *start_of_varheader,
                           sion_int64 **sion_localsizes,
                           sion_int64 **sion_globalranks, 
                           sion_int64 **sion_chunkcount, 
                           sion_int64 **sion_chunksizes, 
                           int *ierr)
{

  (*ierr) = sion_get_locations(*sid, ntasks, maxblocks, globalskip, start_of_varheader, sion_localsizes,
                               sion_globalranks, sion_chunkcount, sion_chunksizes);
}

/*!
 * @brief Fortran function that returns the current file position
 *
 * This fortran function returns an INTEGER*8 containing the current file possition.
 *
 * @param[in]   sid                     sion file handle (in)
 * @returns             INTEGER*8       current file position
 */
sion_int64 fsion_get_position_c(int *sid)
{

  return sion_get_position(*sid);
}

/* OTHER UNDOCUMENTED FUNCTIONS */

void fsion_get_current_locations_c(int *sid, 
                                   int *ntasks, 
                                   sion_int64 **sion_currentpos, 
                                   sion_int64 **sion_currentblocknr, 
                                   int *ierr)
{

  (*ierr) = sion_get_current_locations(*sid, ntasks, sion_currentpos, sion_currentblocknr);
}


void fsion_get_chunksizes_c(int *sid,
			    sion_int64 *chunksizes, 
			    int *ierr)
{
  _sion_filedesc *sion_filedesc;

  if ((_sion_vcdtype(*sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(*sid))) {
    fprintf(stderr, "invalid sion_filedesc, aborting %d ...\n", *sid);
    if (ierr) {
      *ierr=-1;
    }
    return;
  }
  if(sion_filedesc->all_chunksizes) {
    int i;
    for(i=0;i<sion_filedesc->ntasks;i++) {
      chunksizes[i]=sion_filedesc->all_chunksizes[i];
    }
  }

}

void fsion_get_globalranks_c(int *sid,
			     int *globalranks, 
			     int *ierr)
{
  _sion_filedesc *sion_filedesc;

  if ((_sion_vcdtype(*sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(*sid))) {
    fprintf(stderr, "invalid sion_filedesc, aborting %d ...\n", *sid);
    if (ierr) {
      *ierr=-1;
    }
    return;
  }
  if(sion_filedesc->all_globalranks) {
    int i;
    for(i=0;i<sion_filedesc->ntasks;i++) {
      globalranks[i]=sion_filedesc->all_globalranks[i];
    }
  }

}

void fsion_get_mapping_spec_c(int *sid,
			      int *mapping_size, 
			      int *numfiles, 
			      int *ierr)
{
  _sion_filedesc *sion_filedesc;
  if ((_sion_vcdtype(*sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(*sid))) {
    fprintf(stderr, "invalid sion_filedesc, aborting %d ...\n", *sid);
    if (ierr) {
      *ierr=-1;
    }
    return;
  }
  *mapping_size = sion_filedesc->mapping_size;
  *numfiles     = sion_filedesc->nfiles;

}

#define DFUNCTION "fsion_get_mapping_c"
void fsion_get_mapping_c(int *sid, 
                         sion_int32 *mapping, 
                         int *ierr)
{
  _sion_filedesc *sion_filedesc;

  if ((_sion_vcdtype(*sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(*sid))) {
    fprintf(stderr, "invalid sion_filedesc, aborting %d ...\n", *sid);
    *ierr=-1;
    return;
  }
  if(sion_filedesc->nfiles>1) {
    int rank;
    for (rank = 0; rank < sion_filedesc->mapping_size; rank++) {
      DPRINTFP((32, DFUNCTION, 0, " mapping[%d] = %d , %d \n", rank,sion_filedesc->mapping[rank*2+0],sion_filedesc->mapping[rank*2+1]));
      mapping[rank*2+0]=sion_filedesc->mapping[rank*2+0];
      mapping[rank*2+1]=sion_filedesc->mapping[rank*2+1];
    }
  } else {
    *ierr=-1;
  }
}
#undef DFUNCTION

/*!
 * @brief Fortran function that returns the current file number
 *
 * This fortran function returns an INTEGER*8 containing the current file possition.
 *
 * @param[in]   sid                     sion file handle (in)
 * @param[out]  filenumber              used for return value
 */
void fsion_get_fileno_c(int *sid, 
                        int *filenumber)
{
  _sion_filedesc *sion_filedesc;
  if ((_sion_vcdtype(*sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(*sid))) {
    fprintf(stderr, "invalid sion_filedesc, aborting %d ...\n", *sid);
    *filenumber=-1;
    return;
  }
  *filenumber = _sion_file_get_fd(sion_filedesc->fileptr);
  return;
}


/*!
 * @brief Fortran function that returns endianness of a sion file  (1-> big endian, 0 ->little endian)
 *
 * @param[in]   sid         sion file handle (in)
 * @param[out]  endianness  used for the return value
 */
void fsion_get_file_endianness_c(int *sid, 
                                 int *endianness)
{
  *endianness = sion_get_file_endianness(*sid);
  return;
}


/*!
 * @brief Fortran function that returns current endianness (1-> big endian, 0 ->little endian)
 *
 * @param[out]  endianness  sion file handle (out)
 */
void fsion_get_endianness_c(int *endianness)
{
  *endianness = sion_get_endianness();
  return;
}


/*!
 * @brief Fortran function that returns whether or not byte swapping is needed (1 -> needed, 0 -> not needed)
 *
 * @param[in]   sid     sion file handle (in)
 * @param[out]  needed  used for the return value
 */
void fsion_endianness_swap_needed_c(int *sid,
                                    int *needed)
{
  *needed = sion_endianness_swap_needed(*sid);
  return;
}


/*!
 * @brief Fortran procedure to swap endianness of data
 *
 * `n` elements of `size` bytes each are swapped if `do_swap` is `true`.
 * In-place swapping (`target == source`) is allowed.
 * If `target != source`, the buffers must not overlap.
 *
 * @param[inout]   target    pointer to the store location
 * @param[in]      source    pointer to the source location
 * @param[in]      size      size of the data
 * @param[in]      n         number of items of data to swap
 * @param[in]      do_swap   1 indicates that coversion should be done
 *
 * @param[out]  rc              number of objects converted, or zero
 *                              if an error occurs, or the end-of-file is reached
 */
void fsion_swap_c(void *target, 
                  void *source, 
                  int *size, 
                  int *n, 
                  int *do_swap, 
                  int *rc)
{
  (*rc) = 1;
  sion_swap( (void *) target, 
	     (void *) source,
	     (int) *size,
	     (int) *n,
	     (int) *do_swap
	     );
}
