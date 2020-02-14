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
 *
 * MPI Implementation
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

#include "mpi.h"

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"
#include "sion_flags.h"

#ifdef SION_MPI

#include "sion_generic.h"

#include "sion_mpi.h"
#include "sion_mpi_internal_gen.h"

#include "sion_mpi_cb_gen.h"

int _sion_mpi_api_aid = -1;

/*!
 * @brief Open a sion file using MPI.
 *
 * This function opens a sion file using MPI. It processes the MPI
 * specific parts and then passes its arguments on to
 * sion_generic_paropen().
 *
 * For more description please see the \ref sion_paropen_mpi_description
 * "description" of sion_paropen_mpi.
 *
 * @param[in]       fname        name of file, should be equal on all tasks
 * @param[in]       file_mode    like the type parameter of fopen. See
 *                               \ref file_mode_description "file mode description" for details
 * @param[in,out]   numFiles     number of multi files to use (-1 for automatic choosing from local communicator)
 * @param[in]       gComm        global MPI communicator (typically MPI_COMM_WORLD)
 * @param[in]       lComm        local MPI communicator (= gComm if no adaption to I/O nodes is needed)
 *                               lComm should be a partition of gComm, i.e. a disjoint cover
 * @param[in,out]   chunksize    maximum size to be written with single write call
 * @param[in,out]   fsblksize    file system block size. Must be equal on all processes (-1 for automatic)
 * @param[in,out]   globalrank   global rank of process
 *                               any globally unique id for current task. It
 *                               will be stored in sion file. Useful if comm is
 *                               not MPI_COMM_WORLD (typically:
 *                               globalrank = rank in MPI_COMM_WORLD)
 * @param[out]      fileptr      file pointer (NULL for not using an external
 *                               which allows for more optimisation)
 * @param[out]      newfname     return value for actual file name if using
 *                               multi files. NULL does not return the name. If
 *                               passed non NULL pointer the memory needs to be
 *                               freed by the user.
 *
 * @retval          sid          sion file handle or -1 if error occurred
 */
int sion_paropen_mpi(const char*     fname,
                     const char*     file_mode,
                     int*            numFiles,
                     MPI_Comm        gComm,
                     const MPI_Comm* lComm,
                     sion_int64*     chunksize,
                     sion_int32*     fsblksize,
                     int*            globalrank,
                     FILE**          fileptr,
                     char**          newfname
                     )
{
  int        rc = SION_NOT_SUCCESS, sid = SION_ID_UNDEF;
  int        filenumber, gtasks, gRank, lRank, lSize;

  _mpi_api_commdata *gen_gcomm;

  _sion_flags_store* flags_store = NULL;

  MPI_Comm_size(gComm, &gtasks);
  MPI_Comm_rank(gComm, &gRank);

  DPRINTFP((1, "sion_paropen_mpi", gRank, "enter parallel open of file %s\n", fname));
  
  /* check parameters */
  if (lComm == NULL) {
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: No lComm variable given"));
  }
  if (numFiles == NULL) {
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: No numFiles variable given"));
  }
  
  flags_store = _sion_parse_flags(file_mode);
  /* parse file mode */
  if ( ! flags_store ) {
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: could not parse file mode in %s, aborting ...\n", file_mode));
  }

  /* register callbacks for generic interface */
  if(_sion_mpi_api_aid<0) _sion_mpi_api_aid=_sion_register_callbacks_mpi();


  if (flags_store->mask&_SION_FMODE_WRITE) {
    /* file mode WRITE */

    if (*numFiles <= 0) {
      /* lComm contains local communicator */
      if (_sion_flags_get(flags_store, "collmsa")) {
        _sion_flags_destroy_store(&flags_store);
        return _sion_errorprint_mpi(SION_ID_NOT_VALID, _SION_ERROR_RETURN, "sion_paropen_mpi: numFiles <= 0 not supported with MSA aware collectives enabled");
      }

      rc = _sion_get_info_from_splitted_comm_mpi(gComm, *lComm, numFiles, &filenumber, &lRank, &lSize);
      if(rc != SION_SUCCESS) {
        _sion_flags_destroy_store(&flags_store);
        return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: error in _sion_get_info_from_splitted_comm_mpi"));
      }
      DPRINTFP((1, "sion_paropen_mpi", gRank, "%d local communicators found\n", *numFiles));

    } else {
      /* number of files is given */
      if (_sion_flags_get(flags_store, "collmsa")) {
        lRank = lSize = -1; /* will be set by sion_generic_paropen */
        rc = SION_SUCCESS;
      } else {
        rc = _sion_gen_info_from_gcomm_mpi(*numFiles, gComm, &filenumber, &lRank, &lSize);
      }

      if(rc != SION_SUCCESS) {
        _sion_flags_destroy_store(&flags_store);
        return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: error in _sion_gen_info_from_gcomm_mpi"));
      }
      DPRINTFP((1, "sion_paropen_mpi", gRank, "Global communicator divided in %d local communicators\n", *numFiles));
    }

    /* overwrite globalrank set by user, necessary for multi-file support */
    *globalrank = gRank;

  } else if (flags_store->mask&_SION_FMODE_READ) {
    /* file mode READ */
    /* nothing to do info will be returned by generic paropen */

    /* set to gRank, current rank in global communicator, this is
       different to older versions of SIONlib, where globalrank comes
       from file in read case */
    *globalrank = gRank;

    
    if(! (flags_store->mask&_SION_FMODE_BUDDY) ) { 
      lRank=lSize=-1; 		/* will be set by sion_generic_paropen */
    } else {
      /* lvomm must be given for buddy checkpointing */
      rc = _sion_get_info_from_splitted_comm_mpi(gComm, *lComm, numFiles, &filenumber, &lRank, &lSize);
      if(rc != SION_SUCCESS) return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: error in _sion_get_info_from_splitted_comm_mpi"));
      DPRINTFP((1, "sion_paropen_mpi", gRank, "%d local communicators found\n", *numFiles));
    }

  } else {
    
    _sion_flags_destroy_store(&flags_store);
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: unknown file mode"));
  }
  
  /* create generic communicator container */
  gen_gcomm = (_mpi_api_commdata *) malloc(sizeof(_mpi_api_commdata));
  if (gen_gcomm == NULL) {
    _sion_flags_destroy_store(&flags_store);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate mpi internal data structure of size %lu (_mpi_api_commdata), aborting ...\n", 
			    (unsigned long) sizeof(_mpi_api_commdata)));
  }
  gen_gcomm->comm=gComm;
  gen_gcomm->commset=1;
  gen_gcomm->local=0;
  gen_gcomm->rank=gRank;
  gen_gcomm->size=gtasks;
  gen_gcomm->lcommgroup=NULL;
    
  _sion_flags_destroy_store(&flags_store);

  DPRINTFP((1, "sion_paropen_mpi", gRank, "enter parallel open of %d files (current name %s) in %s mode\n", *numFiles, fname, file_mode));
  sid = sion_generic_paropen(_sion_mpi_api_aid, fname, file_mode, chunksize, fsblksize, gen_gcomm, gRank, gtasks, &filenumber, numFiles, &lRank, &lSize, 
			     fileptr, newfname);
  DPRINTFP((1, "sion_paropen_mpi", gRank, "leave parallel open of %d files in %s mode #tasks=%d sid=%d\n", *numFiles, file_mode, lSize, sid));

  /* test return code from internal open */
  if ( sid == SION_ID_NOT_VALID ) {
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mpi: invalid return code from internal open %d", rc));
  }

  DPRINTFP((1, "sion_paropen_mpi", gRank, "leave parallel open of file %s sid=%d\n", fname, sid));

  return (sid);
}


/*!
 * @brief Close a sion file using MPI.
 *
 * For more description please see the \ref sion_parclose_mpi_description
 * "description" of sion_parclose_mpi.
 *
 * @param[in]   sid   sion file handle
 *
 * @retval   success value (SION_SUCCESS or SION_NOT_SUCCESS)
 */
int sion_parclose_mpi(int sid)
{
  int       rc = 0;

  DPRINTFP((1, "sion_parclose_mpi", _SION_DEFAULT_RANK, "enter parallel close of sid %d\n", sid));

  rc = sion_generic_parclose(sid);

  DPRINTFP((1, "sion_parclose_mpi", _SION_DEFAULT_RANK, "leave parallel close of sid %d rc=%d\n", sid, rc));

  return (rc);
}

int sion_parreinit_mpi(  int sid,
			 sion_int64 chunksize )
{
  int       rc = 0;

  DPRINTFP((1, "sion_parreinit_mpi", _SION_DEFAULT_RANK, "enter parallel reinit of sid %d\n", sid));

  rc = sion_generic_parreinit(sid, chunksize);

  DPRINTFP((1, "sion_parreinit_mpi", _SION_DEFAULT_RANK, "leave parallel reinit of sid %d rc=%d\n", sid, rc));

  return (rc);
}

int sion_paropen_mapped_mpi(    char          *fname,
				const char    *file_mode,
				int           *numFiles,
				MPI_Comm       gComm,
				int           *nlocaltasks,
				int          **globalranks,
				sion_int64   **chunksizes,
				int          **mapping_filenrs, 
				int          **mapping_lranks,
				sion_int32    *fsblksize,
				FILE         **fileptr) {

  int       sid=SION_ID_UNDEF;
  int       gtasks, gRank;
  char      *lprefix;
  _mpi_api_commdata *gen_gcomm;
  _sion_flags_store* flags_store = NULL;

  MPI_Comm_size(gComm, &gtasks);
  MPI_Comm_rank(gComm, &gRank);
  
  DPRINTFP((1, "sion_paropen_mapped_mpi", gRank, "enter parallel open of file %s\n", fname));

  /* check parameters */
  if (numFiles == NULL) {
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mapped_mpi: No numFiles variable given"));
  }

  lprefix = calloc(SION_FILENAME_LENGTH,1);
  if (lprefix == NULL) {
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mapped_mpi: cannot allocate temporary memory of size %lu (lprefix), aborting ...\n", (unsigned long) SION_FILENAME_LENGTH));
  }

  flags_store = _sion_parse_flags(file_mode);
  if ( ! flags_store ) {
    free(lprefix);
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mapped_mpi: could not parse file mode in %s, aborting ...\n", file_mode));
  }

  /* register callbacks for generic interface */
  if(_sion_mpi_api_aid<0) _sion_mpi_api_aid=_sion_register_callbacks_mpi();
  
  if (flags_store->mask&_SION_FMODE_WRITE) {
    /* file mode WRITE */
    
    if (*numFiles <= 0) {
      _sion_flags_destroy_store(&flags_store);
      free(lprefix);
      return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mapped_mpi: numFiles variable <= 0 not allowed for mapped files in write mode"));
    }

    /* prefix must be used in generic open function */
    strcpy(lprefix, fname);

  }
  else if (flags_store->mask&_SION_FMODE_READ) {
    /* file mode READ */
    /* nothing to do here so far, filenumbers and mapping will be determined by in generic routine */

  } else {
    _sion_flags_destroy_store(&flags_store);
    free(lprefix);
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mapped_mpi: unknown file mode"));
  }
  _sion_flags_destroy_store(&flags_store);

  /* create generic communicator container */
  gen_gcomm = (_mpi_api_commdata *) malloc(sizeof(_mpi_api_commdata));
  if (gen_gcomm == NULL) {
    free(lprefix);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"cannot allocate mpi internal data structure of size %lu (_mpi_api_commdata), aborting ...\n", 
			    (unsigned long) sizeof(_mpi_api_commdata)));
  }
  gen_gcomm->comm=gComm;
  gen_gcomm->commset=1;
  gen_gcomm->local=0;
  gen_gcomm->rank=gRank;
  gen_gcomm->size=gtasks;
  gen_gcomm->lcommgroup=NULL;
 

  DPRINTFP((1, "sion_paropen_mapped_mpi", gRank, "enter parallel open of %d files (current name %s) in %s mode (sid=%d)\n", *numFiles, fname, file_mode, sid));
  sid=sion_generic_paropen_mapped(_sion_mpi_api_aid, fname, file_mode, numFiles, gen_gcomm, gRank, gtasks, nlocaltasks, globalranks, chunksizes, 
				 mapping_filenrs, mapping_lranks, fsblksize, fileptr);
  DPRINTFP((1, "sion_paropen_mapped_mpi", gRank, "leave parallel open of %d files in %s mode #tasks=%d sid=%d\n", *numFiles, file_mode, *nlocaltasks, sid));

  /* test return code from internal open */
  if ( sid == SION_ID_NOT_VALID ) {
    free(lprefix);
    return(_sion_errorprint_mpi(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_mapped_mpi: invalid return code from internal open %d", sid));
  }

  if(lprefix) free(lprefix);
  DPRINTFP((1, "sion_paropen_mapped_mpi", gRank, "leave parallel open of file %s sid=%d\n", fname, sid));


  return(sid);
}

int sion_parclose_mapped_mpi(   int      sid  ) {
  int       rc = 0;
 
  DPRINTFP((1, "sion_parclose_mapped_mpi", _SION_DEFAULT_RANK, "enter parallel close of sid %d\n", sid));

  rc = sion_generic_parclose_mapped(sid);

  DPRINTFP((1, "sion_parclose_mapped_mpi", _SION_DEFAULT_RANK, "leave parallel close of sid %d rc=%d\n", sid, rc));
  
  return(rc);
}

/* end of ifdef MPI */
#endif
