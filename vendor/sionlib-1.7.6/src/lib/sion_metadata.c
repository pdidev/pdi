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
#include <string.h>
#include <time.h>
#include <assert.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_printts.h"
#include "sion_internal.h"
#include "sion_file.h"
#include "sion_filedesc.h"
#include "sion_metadata.h"

#define DFUNCTION "_sion_write_header"
/*!\brief Write the SION Meta Block 1
 *
 * @param  *sion_filedesc       pointer to internal data structure, contaiing all meta data 
 *
 * @retval SION_SUCESS if okay
 */
int _sion_write_header(_sion_filedesc *sion_filedesc)
{
  int       rc = SION_SUCCESS;
  size_t    nwrite;
  char     *lprefix;
  
  char        *sionstr = SION_ID;
  sion_uint32  endianness_writeval;
  sion_int32   sion_version, sion_version_patchlevel, sion_fileformat_version;
  

  DPRINTFP((2, DFUNCTION, -1, "enter write\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n",  _sion_file_get_position(sion_filedesc->fileptr)));

  /* SIONSTR */
  nwrite = _sion_file_write(sionstr, strlen(sionstr), sion_filedesc->fileptr);
  if (nwrite != strlen(sionstr)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(SIONID) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote sionstr nwrite=%lu\n", (unsigned long) nwrite));

  /* endianness */
  endianness_writeval = sion_filedesc->endianness;
  nwrite = _sion_file_write(&endianness_writeval, sizeof(endianness_writeval), sion_filedesc->fileptr);
  if (nwrite != sizeof(endianness_writeval)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(endianness) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote endianness 0x%x nwrite=%lu\n", endianness_writeval, (unsigned long) nwrite));

  /* VERSION Information */
  sion_version = 1000 * SION_MAIN_VERSION + SION_SUB_VERSION;
  sion_version_patchlevel = SION_VERSION_PATCHLEVEL;
  sion_fileformat_version = SION_FILEFORMAT_VERSION;

  nwrite = _sion_file_write(&sion_version, sizeof(sion_version), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_version)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(sion_version) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote sion_version %ld nwrite=%lu\n", sion_version, (unsigned long) nwrite));

  nwrite = _sion_file_write(&sion_version_patchlevel, sizeof(sion_version_patchlevel), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_version_patchlevel)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(sion_version_patchlevel) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote sion_version_patchlevel %ld nwrite=%lu\n", sion_version_patchlevel, (unsigned long) nwrite));

  nwrite = _sion_file_write(&sion_fileformat_version, sizeof(sion_fileformat_version), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_fileformat_version)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(sion_fileformat_version) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote sion_fileformat_version %ld nwrite=%lu\n", sion_fileformat_version, (unsigned long) nwrite));

  /* fsblksize */
  nwrite = _sion_file_write(&sion_filedesc->fsblksize, sizeof(sion_filedesc->fsblksize), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->fsblksize)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(fsblksize) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote fsblksize %ld nwrite=%lu\n", sion_filedesc->fsblksize, (unsigned long) nwrite));

  /* ntasks */
  nwrite = _sion_file_write(&sion_filedesc->ntasks, sizeof(sion_filedesc->ntasks), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->ntasks)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(ntasks) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote ntasks %ld nwrite=%lu\n", sion_filedesc->ntasks, (unsigned long) nwrite));

  /* nfiles */
  nwrite = _sion_file_write(&sion_filedesc->nfiles, sizeof(sion_filedesc->nfiles), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->nfiles)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(nfiles) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote nfiles %d nwrite=%lu\n", sion_filedesc->nfiles, (unsigned long) nwrite));

  /* filenumber */
  nwrite = _sion_file_write(&sion_filedesc->filenumber, sizeof(sion_filedesc->filenumber), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->filenumber)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(filenumber) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote filenumber %d nwrite=%lu\n", sion_filedesc->filenumber, (unsigned long) nwrite));

  /* flag1 --> keyval-mode since file format version 5 (upper 32bit) */
  sion_filedesc->flag1=(sion_int64) sion_filedesc->keyvalmode * 1<<32;
  nwrite = _sion_file_write(&sion_filedesc->flag1, sizeof(sion_filedesc->flag1), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->flag1)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(flag1) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote flag1 %lld nwrite=%lu\n", sion_filedesc->flag1, (unsigned long) nwrite));

  /* flag2 */
  nwrite = _sion_file_write(&sion_filedesc->flag2, sizeof(sion_filedesc->flag2), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->flag2)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(flag2) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote flag2 %d nwrite=%lu\n", sion_filedesc->flag2, (unsigned long) nwrite));
  
  /* prefix */
  if (sion_filedesc->prefix==NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(prefix) to file, data not available, aborting ...\n"));
  }
  lprefix = calloc(SION_FILENAME_LENGTH,1);
  if (lprefix == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot allocate temporary memory of size %lu (lprefix), aborting ...\n", (unsigned long) SION_FILENAME_LENGTH));
  }
  strncpy(lprefix,sion_filedesc->prefix,SION_FILENAME_LENGTH);
  nwrite = _sion_file_write(lprefix, SION_FILENAME_LENGTH, sion_filedesc->fileptr);
  if (nwrite != SION_FILENAME_LENGTH) {
    free(lprefix);
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(lprefix) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  free(lprefix); lprefix=NULL;
  DPRINTFP((32, DFUNCTION, -1, " wrote prefix     =%s\n", sion_filedesc->prefix));

  /* globalranks */
  if (sion_filedesc->all_globalranks==NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(globalranks) to file, data not available, aborting ...\n"));
  }
  nwrite = _sion_file_write(sion_filedesc->all_globalranks, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(globalranks) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote globalranks nwrite=%lu\n", (unsigned long) nwrite));

  /* chunksizes */
  if (sion_filedesc->all_chunksizes==NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(chunksizes) to file, data not available, aborting ...\n"));
  }
  nwrite = _sion_file_write(sion_filedesc->all_chunksizes, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(chunksizes) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote chunksizes nwrite=%lu\n", (unsigned long) nwrite));

  _sion_file_flush(sion_filedesc->fileptr);
  DPRINTFP((2, DFUNCTION, -1, "leave write\n"));

  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_write_header_var_info"
/*!\brief Write the SION Meta Block 1
 *
 * @param  *sion_filedesc       pointer to internal data structure, containing all meta data 
 *
 * @retval SION_SUCESS if okay
 */
int _sion_write_header_var_info(_sion_filedesc *sion_filedesc)
{
  int       rc = SION_SUCCESS;
  size_t    nwrite;

  DPRINTFP((2, DFUNCTION, -1, "enter write\n"));

  _sion_file_flush(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->end_of_header);
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " after set fp to end_of_header, fileptr is at position %14lld\n",  _sion_file_get_position(sion_filedesc->fileptr)));

  /* maxusedchunks */
  nwrite = _sion_file_write(&sion_filedesc->maxusedchunks, sizeof(sion_filedesc->maxusedchunks), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->maxusedchunks)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(maxusedchunks) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote maxusedchunks %d nwrite=%lu\n", sion_filedesc->maxusedchunks, (unsigned long) nwrite));

  /* start_of_varheader */
  nwrite = _sion_file_write(&sion_filedesc->start_of_varheader, sizeof(sion_filedesc->start_of_varheader), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_filedesc->start_of_varheader)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(start_of_varheader) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote start_of_varheader %d nwrite=%lu\n", sion_filedesc->start_of_varheader, (unsigned long) nwrite));

  _sion_file_flush(sion_filedesc->fileptr);
  DPRINTFP((2, DFUNCTION, -1, "leave write\n"));

  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_write_header_var_part_blocksizes"
/*!\brief Write the SION Meta Block 2
 *
 * @param  *sion_filedesc       pointer to internal data structure, containing all meta data 
 *
 * @retval SION_SUCESS if okay
 */
int _sion_write_header_var_part_blocksizes(_sion_filedesc *sion_filedesc)
{
  int       rc = SION_SUCCESS;
  size_t    nwrite;

  DPRINTFP((2, DFUNCTION, -1, "enter write\n"));

  _sion_file_flush(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->start_of_varheader);
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " after set fp to start_of_varheader, fileptr is at position %14lld\n",  _sion_file_get_position(sion_filedesc->fileptr)));

  /* all_blockcount */
  nwrite = _sion_file_write(sion_filedesc->all_blockcount, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(all_blockcount) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote all_blockcount field %d elements nwrite=%lu\n", sion_filedesc->ntasks, (unsigned long) nwrite));

  /* all_blocksizes */
  nwrite = _sion_file_write(sion_filedesc->all_blocksizes, sizeof(sion_int64) * sion_filedesc->ntasks * sion_filedesc->maxusedchunks, sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_int64) * sion_filedesc->ntasks * sion_filedesc->maxusedchunks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(all_blocksizes) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote all_blocksizes field %d elements  nwrite=%lu\n", sion_filedesc->ntasks * sion_filedesc->maxusedchunks, (unsigned long) nwrite));

  _sion_file_flush(sion_filedesc->fileptr);
  DPRINTFP((2, DFUNCTION, -1, "leave write\n"));

  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_write_header_var_part_mapping"
/*!\brief Write mapping into the SION Meta Block 2
 *
 * @param  *sion_filedesc       pointer to internal data structure, containing all meta data 
 * @param  *mapping_size        number of tasks in map
 * @param  *mapping             pointer to map
 *
 * @retval SION_SUCESS if okay
 */
int _sion_write_header_var_part_mapping(_sion_filedesc *sion_filedesc,
					sion_int32 mapping_size, 
					sion_int32 *mapping  )
{
  int       rc = SION_SUCCESS, lrank;
  size_t    nwrite;
  sion_int64 position;

  DPRINTFP((2, DFUNCTION, -1, "enter write\n"));

  position=sion_filedesc->start_of_varheader
    + sion_filedesc->ntasks * sizeof(sion_int64)
    + sion_filedesc->ntasks * sion_filedesc->maxusedchunks *sizeof(sion_int64);

  _sion_file_flush(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, position);
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " after set fp to start_of_varheader, fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  /* mapping_size */
  nwrite = _sion_file_write(&mapping_size, sizeof(sion_int32), sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_int32)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(mapping_size) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote mapping_size=%d elements nwrite=%lu\n", mapping_size, (unsigned long) nwrite));

  /* mapping */
  if(mapping_size>0) {
    nwrite = _sion_file_write(mapping, sizeof(sion_int32) * 2 * mapping_size, sion_filedesc->fileptr);
    if (nwrite != sizeof(sion_int32) * 2 * mapping_size) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot write header(mapping) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
    }
    DPRINTFP((32, DFUNCTION, -1, " wrote mapping field %d elements  nwrite=%lu\n", 2 * mapping_size, (unsigned long) nwrite*2*mapping_size));
    for (lrank = 0; lrank < mapping_size; lrank++) {
      DPRINTFP((2048, DFUNCTION, - 1, " mapping[%4d]=(%10ld,%10ld)\n", lrank,mapping[lrank*2],mapping[lrank*2+1]));
    }
  } else {
    DPRINTFP((32, DFUNCTION, -1, " wrote no mapping field\n"));
  }
  _sion_file_flush(sion_filedesc->fileptr);
  DPRINTFP((2, DFUNCTION, -1, "leave write\n"));

  return (rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_read_header_fix_part"
/*!\brief Read part of the SION Meta Block 1
 *
 * @param  *sion_filedesc       pointer to internal data structure, contaiing all meta data 
 *
 * @retval SION_SUCESS if okay
 */
int _sion_read_header_fix_part( _sion_filedesc *sion_filedesc )
{
  int       rc = SION_SUCCESS;
  size_t    nread;
  char     *sionstr, *lprefix;
  sion_uint32 lendianness;
  sion_int32 lfsblksize;
  sion_int32 Rsion_version, Rsion_version_patchlevel, Rsion_fileformat_version;

  DPRINTFP((2, DFUNCTION, -1, "enter read fix_part\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  /* SION id string */
  sionstr = (char *) malloc(strlen(SION_ID) * sizeof(char));
  nread = _sion_file_read(sionstr, strlen(SION_ID), sion_filedesc->fileptr);
  if (nread != strlen(SION_ID)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(SION_ID) from file, aborting ... (%lu!=%lu)\n",
            (unsigned long) nread, (unsigned long) strlen(SION_ID)));
  }
  DPRINTFP((32, DFUNCTION, -1, " sionstr  =>%c%c%c%c<\n", sionstr[0], sionstr[1], sionstr[2], sionstr[3]));
  /* Check if this is a sion file */
  if (strncmp(sionstr, SION_ID, strlen(SION_ID))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: not a sion file(ID: %c%c%c%c)\n", sionstr[0], sionstr[1], sionstr[2], sionstr[3]));
  }
  free(sionstr);

  /* endianness */
  nread = _sion_file_read(&lendianness, sizeof(lendianness), sion_filedesc->fileptr);
  if (nread != sizeof(lendianness)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header: cannot read header(endianness) from file, aborting ... (%lu!=%lu)\n",(unsigned long) nread, (unsigned long) sizeof(lendianness)));
  }
  sion_filedesc->endianness = lendianness;
  /* Check only first byte. Others are set for symmetry or user data */
  sion_filedesc->swapbytes = ((sion_filedesc->endianness & 1) != sion_get_endianness());
  DPRINTFP((32, DFUNCTION, -1, " endianness =0x%x swapbytes=%d\n", sion_filedesc->endianness, sion_filedesc->swapbytes));


  /* VERSION Information */
  nread = _sion_file_read(&Rsion_version, sizeof(Rsion_version), sion_filedesc->fileptr);
  if (nread != sizeof(Rsion_version)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(sion_version) from file, aborting ... (%lu!=%lu)\n", (unsigned long) nread, (unsigned long) sizeof(Rsion_version)));
  }
  sion_swap(&Rsion_version, &Rsion_version, sizeof(Rsion_version), 1, sion_filedesc->swapbytes);
  sion_filedesc->filesionversion=Rsion_version;
  DPRINTFP((32, DFUNCTION, -1, " sion_version=%d\n", Rsion_version));

  nread = _sion_file_read(&Rsion_version_patchlevel, sizeof(Rsion_version_patchlevel), sion_filedesc->fileptr);
  if (nread != sizeof(Rsion_version_patchlevel)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(sion_version_patchlevel) from file, aborting ... (%lu!=%lu)\n",
            (unsigned long) nread, (unsigned long) sizeof(Rsion_version_patchlevel)));
  }
  sion_swap(&Rsion_version_patchlevel, &Rsion_version_patchlevel, sizeof(Rsion_version_patchlevel), 1, sion_filedesc->swapbytes);
  sion_filedesc->filesionpatchlevel=Rsion_version_patchlevel;
  DPRINTFP((32, DFUNCTION, -1, " sion_version_patchlevel=%d\n", (int) Rsion_version_patchlevel));

  nread = _sion_file_read(&Rsion_fileformat_version, sizeof(Rsion_fileformat_version), sion_filedesc->fileptr);
  if (nread != sizeof(Rsion_fileformat_version)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(sion_fileformat_version) from file, aborting ... (%lu!=%lu)\n",
            (unsigned long) nread, (unsigned long) sizeof(Rsion_fileformat_version)));
  }
  sion_swap(&Rsion_fileformat_version, &Rsion_fileformat_version, sizeof(Rsion_fileformat_version), 1, sion_filedesc->swapbytes);
  sion_filedesc->fileversion=Rsion_fileformat_version;
  DPRINTFP((32, DFUNCTION, -1, " sion_fileformat_version=%d\n", (int) Rsion_fileformat_version));

  if (Rsion_fileformat_version != SION_FILEFORMAT_VERSION) {
    if(Rsion_fileformat_version<2) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sionlib: FATAL ERROR: file format version of file differs, aborting ... (%lu!=%lu)\n",
	      (unsigned long) Rsion_fileformat_version, (unsigned long) SION_FILEFORMAT_VERSION));
    } else {
      _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_WARN,"sionlib: FATAL WARNING: old file format version of file differs, trying to read ... (%lu!=%lu)\n",
	      (unsigned long) Rsion_fileformat_version, (unsigned long) SION_FILEFORMAT_VERSION);
    }
  }

  /* file block size */
  nread = _sion_file_read(&sion_filedesc->fsblksize, sizeof(lfsblksize), sion_filedesc->fileptr);

  if (nread != sizeof(sion_filedesc->fsblksize)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(fsblksize) from file, aborting ... (%lu!=%lu)\n",
            (unsigned long) nread, (unsigned long) sizeof(sion_filedesc->fsblksize)));
  }
  sion_swap(&sion_filedesc->fsblksize, &sion_filedesc->fsblksize, sizeof(sion_filedesc->fsblksize), 1, sion_filedesc->swapbytes);
  DPRINTFP((32, DFUNCTION, -1, " fsblksize=%d\n", sion_filedesc->fsblksize));

  /* number of tasks */
  nread = _sion_file_read(&sion_filedesc->ntasks, sizeof(sion_filedesc->ntasks), sion_filedesc->fileptr);
  if (nread != sizeof(sion_filedesc->ntasks)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(ntasks) from file, aborting ... (%lu!=%lu)\n",
            (unsigned long) nread, (unsigned long) sizeof(sion_filedesc->ntasks)));
  }
  sion_swap(&sion_filedesc->ntasks, &sion_filedesc->ntasks, sizeof(sion_filedesc->ntasks), 1, sion_filedesc->swapbytes);
  DPRINTFP((32, DFUNCTION, -1, " ntasks     =%d\n", sion_filedesc->ntasks));

  /* number of files */
  nread = _sion_file_read(&sion_filedesc->nfiles, sizeof(sion_filedesc->nfiles), sion_filedesc->fileptr);
  if (nread != sizeof(sion_filedesc->nfiles)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(nfiles) from file, aborting ... (%lu!=%lu)\n",
			    (unsigned long) nread, (unsigned long) sizeof(sion_filedesc->nfiles)));
  }
  sion_swap(&sion_filedesc->nfiles, &sion_filedesc->nfiles, sizeof(sion_filedesc->nfiles), 1, sion_filedesc->swapbytes);
  DPRINTFP((32, DFUNCTION, -1, " nfiles     =%d\n", sion_filedesc->nfiles));

  /* current file number */
  if(sion_filedesc->fileversion>=3) {
    nread = _sion_file_read(&sion_filedesc->filenumber, sizeof(sion_filedesc->filenumber), sion_filedesc->fileptr);
    if (nread != sizeof(sion_filedesc->filenumber)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(filenumber) from file, aborting ... (%lu!=%lu)\n",
			      (unsigned long) nread, (unsigned long) sizeof(sion_filedesc->filenumber)));
    }
    sion_swap(&sion_filedesc->filenumber, &sion_filedesc->filenumber, sizeof(sion_filedesc->filenumber), 1, sion_filedesc->swapbytes);
  } else {
    sion_filedesc->filenumber = 1;
  }
  DPRINTFP((32, DFUNCTION, -1, " filenumber =%d\n", sion_filedesc->filenumber));


  /* flag1 */
  if(Rsion_fileformat_version>=3) {
    nread = _sion_file_read(&sion_filedesc->flag1, sizeof(sion_filedesc->flag1), sion_filedesc->fileptr);
    if (nread != sizeof(sion_filedesc->flag1)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(flag1) from file, aborting ... (%lu!=%lu)\n",
			      (unsigned long) nread, (unsigned long) sizeof(sion_filedesc->flag1)));
    }
    sion_swap(&sion_filedesc->flag1, &sion_filedesc->flag1, sizeof(sion_filedesc->flag1), 1, sion_filedesc->swapbytes);
  } else {
    sion_filedesc->flag1 = _SION_FLAG1_NONE;
  }
  DPRINTFP((32, DFUNCTION, -1, " flag1 =%lld\n", sion_filedesc->flag1));

  if(Rsion_fileformat_version>=5) {
    /* flag1 --> keyval-mode since file format version 5 (upper 32bit) */
    sion_filedesc->keyvalmode=sion_filedesc->flag1>>32;
    DPRINTFP((32, DFUNCTION, -1, " keyvalmode=%d (%s)\n", (int) sion_filedesc->keyvalmode, sion_keyval_type_to_str(sion_filedesc->keyvalmode) ));
    if(sion_filedesc->keyvalmode==0) {
      sion_filedesc->keyvalmode=SION_KEYVAL_NONE;
      DPRINTFP((32, DFUNCTION, -1, " keyvalmode is not set correctly in file, setting default=%d (%s)\n", (int) sion_filedesc->keyvalmode, sion_keyval_type_to_str(sion_filedesc->keyvalmode) ));
    }
  } else {
    sion_filedesc->keyvalmode=SION_KEYVAL_NONE; /* not implemented before version 5 of file format */
    DPRINTFP((32, DFUNCTION, -1, " file format version <5, setting default keyvalmode=%d (%s)\n", (int) sion_filedesc->keyvalmode, sion_keyval_type_to_str(sion_filedesc->keyvalmode) ));
  }

    
  /* flag2 */
  if(Rsion_fileformat_version>=3) {
    nread = _sion_file_read(&sion_filedesc->flag2, sizeof(sion_filedesc->flag2), sion_filedesc->fileptr);
    if (nread != sizeof(sion_filedesc->flag2)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(flag2) from file, aborting ... (%lu!=%lu)\n",
			      (unsigned long) nread, (unsigned long) sizeof(sion_filedesc->flag2)));
    }
    sion_swap(&sion_filedesc->flag2, &sion_filedesc->flag2, sizeof(sion_filedesc->flag2), 1, sion_filedesc->swapbytes);
  } else {
    sion_filedesc->flag2 = _SION_FLAG2_NONE;
  }
  DPRINTFP((32, DFUNCTION, -1, " flag2 =%lld\n", sion_filedesc->flag2));

  /* file name prefix */
  lprefix = malloc(SION_FILENAME_LENGTH);
  nread = _sion_file_read(lprefix, SION_FILENAME_LENGTH, sion_filedesc->fileptr);
  if (nread != SION_FILENAME_LENGTH) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_fix_part: cannot read header(lprefix) from file, aborting ... (%lu!=%d)\n", (unsigned long) nread, SION_FILENAME_LENGTH));
  }
  sion_filedesc->prefix=strdup(lprefix);
  free(lprefix);
  DPRINTFP((32, DFUNCTION, -1, " prefix     =%s\n", sion_filedesc->prefix));


  /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "before purge");
  _sion_file_purge(sion_filedesc->fileptr);
  /*                                                                      */ DPRINTFTS(sion_filedesc->rank, "after  purge");

  sion_filedesc->end_of_header=_sion_file_get_position(sion_filedesc->fileptr);

  DPRINTFP((2, DFUNCTION, -1, "leave read fix_part\n"));

  return (rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_read_header_var_part"
/*!\brief Read the second part of SION Meta Block 1
 *
 * @param       *sion_filedesc  pointer to internal data structure, containing all meta data 
 *
 * @retval       SION_SUCESS if okay
 */
int _sion_read_header_var_part( _sion_filedesc *sion_filedesc )
{
  int       rc = SION_SUCCESS;
  size_t     nread;
  sion_int32 Rmaxchunks;
  
  DPRINTFP((32, DFUNCTION, -1, "enter read var_part\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  /* globalranks */
  nread = _sion_file_read(sion_filedesc->all_globalranks, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nread != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part: cannot read header(globalranks) from file, aborting ... (%d)\n", sion_filedesc->ntasks));
  }
  sion_swap(sion_filedesc->all_globalranks, sion_filedesc->all_globalranks, sizeof(sion_int64), sion_filedesc->ntasks, sion_filedesc->swapbytes);

  /* chunksizes */
  nread = _sion_file_read(sion_filedesc->all_chunksizes, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nread != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part: cannot read header(chunksizes) from file, aborting ... (%d!=%lu)\n", sion_filedesc->ntasks, (unsigned long) nread));
  }
  sion_swap(sion_filedesc->all_chunksizes, sion_filedesc->all_chunksizes, sizeof(sion_int64), sion_filedesc->ntasks, sion_filedesc->swapbytes);

  /* maxchunks -> maxusedchunks */
  nread = _sion_file_read(&Rmaxchunks, sizeof(Rmaxchunks), sion_filedesc->fileptr);
  if (nread != sizeof(Rmaxchunks)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part: cannot read header(maxchunks) from file, aborting ... (%lu!=%lu)\n",
			    (unsigned long) nread, (unsigned long) sizeof(Rmaxchunks)));
  }
  sion_swap(&Rmaxchunks, &Rmaxchunks, sizeof(Rmaxchunks), 1, sion_filedesc->swapbytes);
  DPRINTFP((32, DFUNCTION, -1, " maxchunks=%d\n", Rmaxchunks));

  sion_filedesc->maxusedchunks=Rmaxchunks;
  if(Rmaxchunks>sion_filedesc->maxchunks) _sion_realloc_filedesc_blocklist(sion_filedesc, Rmaxchunks);

  /* start_of_varheader */
  nread = _sion_file_read(&sion_filedesc->start_of_varheader, sizeof(sion_filedesc->start_of_varheader), sion_filedesc->fileptr);
  if (nread != sizeof(sion_filedesc->start_of_varheader)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part: cannot read header(start_of_varheader) from file, aborting ... (%lu!=%lu)\n",
			    (unsigned long) nread, (unsigned long) sizeof(sion_filedesc->start_of_varheader)));
  }
  if (sion_filedesc->start_of_varheader == 0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part: bad value for start_of_varheader (=%lu), aborting ...\n",
			    (unsigned long) sion_filedesc->start_of_varheader));
  }
  sion_swap(&sion_filedesc->start_of_varheader, &sion_filedesc->start_of_varheader, sizeof(sion_filedesc->start_of_varheader), 1, sion_filedesc->swapbytes);

  DPRINTFP((32, DFUNCTION, -1, "leave read var_part start_of_varheader=%lld\n", sion_filedesc->start_of_varheader));

  return (rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_rd_hdr_var_prt_blkcnt_to_field"
/*!\brief Read the block sizes from Meta Block 2
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 * @param       field_size      size of field
 * @param       field           field where blocksizes will be stored
 *
 * @retval       SION_SUCESS if okay
 */
int _sion_read_header_var_part_blockcount_to_field( _sion_filedesc *sion_filedesc,
						    int             field_size, 
						    sion_int64     *field           )
{
  int       rc = SION_SUCCESS;
  size_t    nread;
  
  DPRINTFP((32, DFUNCTION, -1, "enter\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  if (field_size < sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_blockcount_to_field: cannot read header(blocksizes) field too small, aborting ... (%d<%d)\n", field_size, sion_filedesc->ntasks));
  }
  /* set file pointer to start of Meta Block 2 */
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->start_of_varheader);

  nread = _sion_file_read(field, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nread != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_blockcount_to_field: cannot read blocksizes from meta block 2 of the file, aborting ... (%d,%d)\n", sion_filedesc->ntasks, nread));
  }
  sion_swap(field, field, sizeof(sion_int64), sion_filedesc->ntasks, sion_filedesc->swapbytes);
  

  DPRINTFP((32, DFUNCTION, -1, "leave \n"));

  return (rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_rd_hdr_vr_prt_nxtblksizes_to_field"
/*!\brief Read the next set of blocksizes from Meta Block 2
 *        Assuming that filepointer is at the correct position
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 * @param       field_size      size of field
 * @param       field           field where blocksizes will be stored
 *
 * @retval       SION_SUCESS if okay
 */
int _sion_read_header_var_part_nextblocksizes_to_field( _sion_filedesc *sion_filedesc,
							int             field_size, 
							sion_int64     *field           )
{
  size_t    nread;
  int       rc = SION_SUCCESS;

  DPRINTFP((32, DFUNCTION, -1, "enter\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  if (field_size < sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_nextblocksizes_to_field: cannot read header(blocksizes) field too small, aborting ... (%d<%d)\n", field_size, sion_filedesc->ntasks));
  }
  nread = _sion_file_read(field, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nread != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_nextblocksizes_to_field: cannot read blocksizes from meta block 2 of the file, aborting ... (%d,%d)\n", sion_filedesc->ntasks, nread));
  }
  sion_swap(field, field, sizeof(sion_int64), sion_filedesc->ntasks, sion_filedesc->swapbytes);
  

  DPRINTFP((32, DFUNCTION, -1, "leave \n"));

  return (rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_read_header_var_part_blocksizes"
/*!\brief Read the SION Meta Block 2
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 *
 * @retval      SION_SUCESS if okay
 */
int _sion_read_header_var_part_blocksizes( _sion_filedesc *sion_filedesc )
{
  int       rc = SION_SUCCESS;
  size_t    nread;
  int       i;

  DPRINTFP((32, DFUNCTION, -1, "enter read\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->start_of_varheader);

  /* all_blockcount */
  nread = _sion_file_read(sion_filedesc->all_blockcount, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nread != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_blocksizes: cannot read header from file(sion_blockcount), aborting ... (%d)\n", sion_filedesc->ntasks));
  }
  sion_swap(sion_filedesc->all_blockcount, sion_filedesc->all_blockcount, sizeof(sion_int64), sion_filedesc->ntasks, sion_filedesc->swapbytes);
  for (i = 0; i < sion_filedesc->ntasks; i++)
    DPRINTFP((2048, DFUNCTION, -1, " read, numblocks on task %02d is %10lld\n", i, sion_filedesc->all_blockcount[i]));

  /* all_blocksizes */
  nread = _sion_file_read(sion_filedesc->all_blocksizes, sizeof(sion_int64) * sion_filedesc->ntasks * sion_filedesc->maxusedchunks, sion_filedesc->fileptr);
  if (nread != sizeof(sion_int64) * sion_filedesc->ntasks * sion_filedesc->maxusedchunks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_blocksizes: cannot read header(sion_blocksizes) from file, aborting ... (%lu!=%lu)\n",
			    (unsigned long) sion_filedesc->ntasks * sion_filedesc->maxusedchunks, (unsigned long) nread));
  }
  sion_swap(sion_filedesc->all_blocksizes, sion_filedesc->all_blocksizes, sizeof(sion_int64), sion_filedesc->ntasks * sion_filedesc->maxusedchunks, sion_filedesc->swapbytes);

  for (i = 0; i < sion_filedesc->ntasks; i++)
    DPRINTFP((2048, DFUNCTION, -1, " read, blocksize[0] on task %02d is %10lld\n", i, sion_filedesc->all_blocksizes[0*sion_filedesc->ntasks+i]));

  DPRINTFP((32, DFUNCTION, -1, "leave read\n"));
  return (rc);

}

#undef DFUNCTION

#define DFUNCTION "_sion_read_header_var_part_mapping"
/*!\brief Read the mapping data at end of SION Meta Block 2
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 *
 *  file pointer will be set to the correct position
 *
 * @retval SION_SUCESS if okay
 */
int _sion_read_header_var_part_mapping( _sion_filedesc *sion_filedesc )
{
  int       rc = SION_SUCCESS;
  size_t    nread;
  sion_int64 position;

  DPRINTFP((32, DFUNCTION, -1, "enter read\n"));

  position=sion_filedesc->start_of_varheader
    + sion_filedesc->ntasks * sizeof(sion_int64)
    + sion_filedesc->ntasks * sion_filedesc->maxusedchunks *sizeof(sion_int64);
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, position);
  DPRINTFP((32, DFUNCTION, -1, "calculate position %lld + %d * %d = %lld\n",sion_filedesc->start_of_varheader,sion_filedesc->ntasks,sion_filedesc->maxusedchunks,position ));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  /* read mapping_size */
  nread = _sion_file_read(&sion_filedesc->mapping_size, sizeof(sion_filedesc->mapping_size), sion_filedesc->fileptr);
  if (nread != sizeof(sion_filedesc->mapping_size)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_mapping: cannot read header(mapping_size) from file, aborting ... (%d)\n", nread));
  }
  sion_swap(&sion_filedesc->mapping_size,&sion_filedesc->mapping_size, sizeof(sion_filedesc->mapping_size), 1, sion_filedesc->swapbytes);
  DPRINTFP((32, DFUNCTION, -1, "mapping_size=%ld\n", (long) sion_filedesc->mapping_size));
  
  /* allocate mapping vector */
  sion_filedesc->mapping = (sion_int32 *) malloc(sion_filedesc->mapping_size * 2 * sizeof(sion_int32));
  if (sion_filedesc->mapping == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_mapping: cannot allocate temporary memory of size %lu (mapping), aborting ...\n", (unsigned long) 2 * sion_filedesc->mapping_size * sizeof(sion_int64)));
  }
  DPRINTFP((32, DFUNCTION, -1, "alloc mapping vector size=%d (%lu bytes)\n", sion_filedesc->mapping_size, (long) sion_filedesc->mapping_size * 2 * sizeof(sion_int32)));

  /* read mapping */
  nread = _sion_file_read(sion_filedesc->mapping, sizeof(sion_int32) * sion_filedesc->mapping_size * 2, sion_filedesc->fileptr);
  DPRINTFP((32, DFUNCTION, -1, "read mapping, %d \n", nread));
  if (nread != (sizeof(sion_int32) * sion_filedesc->mapping_size * 2)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_mapping: cannot read header(mapping) from file, aborting ... (%d!=%lu)\n", sion_filedesc->mapping_size*2, (unsigned long) nread));
  }
  sion_swap(sion_filedesc->mapping,sion_filedesc->mapping, sizeof(sion_int32), sion_filedesc->mapping_size * 2, sion_filedesc->swapbytes);

  DPRINTFP((32, DFUNCTION, -1, "leave read\n"));

  return (rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_read_header_var_part_locations_rank"
/*!\brief Read the SION Meta Block 2
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 *
 * @retval SION_SUCESS if okay
 */
/* TODO: _sion_filedesc *sion_filedesc -> fileptr */
int _sion_read_header_var_part_blocksizes_rank(_sion_filedesc *sion_filedesc)
{
  int       rc = SION_SUCCESS;
  int       i, numblocks;
  size_t    nread;
  sion_int64 position;
  sion_int64 helpint64;

  DPRINTFP((32, DFUNCTION, -1, "enter read for rank %d of %d\n",sion_filedesc->rank,sion_filedesc->ntasks));

  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->start_of_varheader);

  /* read number of blocks */
  nread = _sion_file_read(&helpint64, sizeof(sion_int64), sion_filedesc->fileptr);
  sion_swap(&helpint64,&helpint64, sizeof(sion_int64), 1, sion_filedesc->swapbytes);
  numblocks = helpint64;
  if (nread != sizeof(sion_int64)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_locations_rank: cannot read header(numblocks) from file, aborting ... (%d)\n", sion_filedesc->ntasks));
  }
  sion_filedesc->maxusedchunks = numblocks;
  sion_filedesc->lastchunknr   = numblocks-1;

  /* read blocksizes */
  for (i = 0; i < numblocks; i++) {

    position = sion_filedesc->start_of_varheader 
      + (i + 1) * sion_filedesc->ntasks * sizeof(sion_int64) 
      + sion_filedesc->rank * sizeof(sion_int64);
    _sion_file_purge(sion_filedesc->fileptr);
    _sion_file_set_position(sion_filedesc->fileptr, position);

    nread = _sion_file_read(&helpint64, sizeof(sion_int64), sion_filedesc->fileptr);
    if (nread != sizeof(sion_int64)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_locations_rank: cannot read header(blocksizes) from file, aborting ... (%d!=%lu)\n", 1, (unsigned long) nread));
    }
    sion_swap(&helpint64,&helpint64, sizeof(sion_int64), 1, sion_filedesc->swapbytes);
    sion_filedesc->blocksizes[i] = helpint64;
    DPRINTFP((32, "_sion_read_header_var_part_locations_rank", -1, "read blocksizes[%i], %lld start_of_varheader=%lld\n", i, helpint64, sion_filedesc->start_of_varheader));

  }

  DPRINTFP((32, DFUNCTION, -1, "leave read\n"));

  return (rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_read_header_var_part_mapping_rank"
/*!\brief Read the mapping data at end of SION Meta Block 2
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 *
 *  file pointer will be set to the correct position
 *
 * @retval SION_SUCESS if okay
 */
int _sion_read_header_var_part_mapping_rank(_sion_filedesc *sion_filedesc)
{
  int       rc = SION_SUCCESS;
  int       msize;
  size_t    nread;
  sion_int64 position;
  sion_int32 helpint32;

  DPRINTFP((32, DFUNCTION, -1, "enter read for rank %d of %d\n",sion_filedesc->rank,sion_filedesc->ntasks));

  position=sion_filedesc->start_of_varheader
    + sion_filedesc->ntasks * sizeof(sion_int64)
    + sion_filedesc->ntasks * sion_filedesc->maxusedchunks *sizeof(sion_int64);
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, position);
  DPRINTFP((32, DFUNCTION, -1, "calculate position %lld + %d * %d = %lld\n",sion_filedesc->start_of_varheader,sion_filedesc->ntasks,sion_filedesc->maxusedchunks,position ));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  /* read mapping_size */
  nread = _sion_file_read(&helpint32, sizeof(sion_int32), sion_filedesc->fileptr);
  if (nread != sizeof(sion_int32)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_mapping_rank: cannot read header(mapping_size) from file, aborting ... (%d)\n", sion_filedesc->ntasks));
  }
  sion_swap(&helpint32,&helpint32, sizeof(sion_int32), 1, sion_filedesc->swapbytes);
  msize = helpint32;
  DPRINTFP((32, "_sion_read_header_var_part_mapping_rank", -1, "read mapping_size %d \n", msize));
  
  if((sion_filedesc->rank<0) || (sion_filedesc->rank>=msize)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_mapping_rank: wrong rank %d not between 0 .. %d aborting ...\n", sion_filedesc->rank,msize));
  }

  /* move to position in mapping table for rank */
  position=sion_filedesc->start_of_varheader
    + sion_filedesc->ntasks * sizeof(sion_int64)
    + sion_filedesc->ntasks * sion_filedesc->maxusedchunks *sizeof(sion_int64)
    + sizeof(sion_int32)
    + 2*sion_filedesc->rank*sizeof(sion_int32);

  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, position);
 
  /* allocate mapping vector of length 1 */
  sion_filedesc->mapping_size=1;
  /* allocate mapping vector */
  sion_filedesc->mapping = (sion_int32 *) malloc(sion_filedesc->mapping_size * 2 * sizeof(sion_int32));
  if (sion_filedesc->mapping == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_mapping: cannot allocate temporary memory of size %lu (mapping), aborting ...\n", (unsigned long) 2 * sion_filedesc->mapping_size * sizeof(sion_int64)));
  }
  DPRINTFP((32, DFUNCTION, -1, "alloc mapping vector size=%d (%lu bytes)\n", sion_filedesc->mapping_size, (long) sion_filedesc->mapping_size * 2 * sizeof(sion_int32)));

  /* read mapping for rank */
  nread = _sion_file_read(sion_filedesc->mapping, sizeof(sion_int32) * sion_filedesc->mapping_size * 2, sion_filedesc->fileptr);
  DPRINTFP((32, DFUNCTION, -1, "read mapping, %d \n", nread));
  if (nread != (sizeof(sion_int32) * sion_filedesc->mapping_size * 2)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_mapping: cannot read header(mapping) from file, aborting ... (%d!=%lu)\n", sion_filedesc->mapping_size*2, (unsigned long) nread));
  }
  sion_swap(sion_filedesc->mapping,sion_filedesc->mapping, sizeof(sion_int32), sion_filedesc->mapping_size * 2, sion_filedesc->swapbytes);

  DPRINTFP((32, DFUNCTION, -1, "leave read filenumber=%d lrank=%d\n",sion_filedesc->mapping[0], sion_filedesc->mapping[1]));

  return (rc);

}
#undef DFUNCTION


#define DFUNCTION "_sion_wr_hdr_var_prt_blkcnt_from_field"
/*!\brief Write the block sizes from Meta Block 2
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 * @param       field_size      size of field
 * @param       field           field where blocksizes will be stored
 *
 * @retval       SION_SUCESS if okay
 */
int _sion_write_header_var_part_blockcount_from_field( _sion_filedesc *sion_filedesc,
						       int             field_size, 
						       sion_int64     *field           )
{
  size_t nwrite;
  int    rc=SION_SUCCESS;

  DPRINTFP((32, DFUNCTION, -1, "enter\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  if (field_size < sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_read_header_var_part_blockcount_to_field: cannot read header(blocksizes) field too small, aborting ... (%d<%d)\n", field_size, sion_filedesc->ntasks));
  }
  /* set file pointer to start of Meta Block 2 */
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->start_of_varheader);

  nwrite = _sion_file_write(field, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header_var_part_blockcount_from_field: cannot write header(all_blockcount) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote all_blockcount field %d elements nwrite=%lu\n", sion_filedesc->ntasks, (unsigned long) nwrite));

  DPRINTFP((32, DFUNCTION, -1, "leave \n"));

  return (rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_wr_hdr_vr_prt_nxtblksizes_from_field"
/*!\brief Write the next set of blocksizes from Meta Block 2
 *        Assuming that filepointer is at the correct position
 *
 * @param       *sion_filedesc  pointer to internal data structure, contaiing all meta data 
 * @param       field_size      size of field
 * @param       field           field where blocksizes will be stored
 *
 * @retval       SION_SUCESS if okay
 */
int _sion_write_header_var_part_nextblocksizes_from_field( _sion_filedesc *sion_filedesc,
							   int             field_size, 
							   sion_int64     *field           )
{
  int    rc=SION_SUCCESS;
  size_t nwrite;

  DPRINTFP((32, DFUNCTION, -1, "enter\n"));
  DPRINTFP((16, DFUNCTION, sion_filedesc->rank, " fileptr is at position %14lld\n", _sion_file_get_position(sion_filedesc->fileptr)));

  if (field_size < sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header_var_part_nextblocksizes_from_field: cannot write header(blocksizes) field too small, aborting ... (%d<%d)\n", field_size, sion_filedesc->ntasks));
  }
  nwrite = _sion_file_write(field, sizeof(sion_int64) * sion_filedesc->ntasks, sion_filedesc->fileptr);
  if (nwrite != sizeof(sion_int64) * sion_filedesc->ntasks) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_write_header_var_part_nextblocksizes_from_field: cannot write header(all_blocksizes) to file, aborting ... (%lu)\n", (unsigned long) nwrite));
  }
  DPRINTFP((32, DFUNCTION, -1, " wrote all_blocksizes field %d elements nwrite=%lu\n", sion_filedesc->ntasks, (unsigned long) nwrite));

  DPRINTFP((32, DFUNCTION, -1, "leave \n"));

  return (rc);

}
#undef DFUNCTION

