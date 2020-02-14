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

#include "mpi.h"

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"

#ifdef SION_MPI

#include "sion_generic.h"
#include "sion_generic_internal.h"

#include "sion_mpi.h"
#include "sion_mpi_internal_gen.h"

#include "sion_mpi_cb_gen.h"


/* wrapper for old SIONlib MPI function */
size_t sion_coll_fwrite_mpi(const void *data, size_t size, size_t nitems, int sid) {
  return(sion_coll_fwrite(data,size,nitems,sid));
}


/* wrapper for old SIONlib MPI function */
size_t sion_coll_fread_mpi(void *data, size_t size, size_t nitems, int sid) {
  return(sion_coll_fread(data,size,nitems,sid));
}


/* wrapper for old SIONlib MPI function */
int sion_paropen_comms_mpi(char *fname,
                           const char *file_mode,
                           int *numFiles,
                           MPI_Comm gComm,
                           MPI_Comm lComm, sion_int64 *chunksize, sion_int32 *fsblksize, int *globalrank, FILE **fileptr, char *newfname)
{
  *numFiles=0;
  return(sion_paropen_mpi(fname,file_mode,numFiles,gComm,&lComm,chunksize,fsblksize,globalrank,fileptr,&newfname));
}

int sion_paropen_multi_mpi(char *fname,
                           const char *file_mode,
                           int *numFiles,
                           MPI_Comm gComm,
                           MPI_Comm *lComm, sion_int64 *chunksize, sion_int32 *fsblksize, int *globalrank, FILE **fileptr, char *newfname)
{
  return(sion_paropen_mpi(fname,file_mode,numFiles,gComm,lComm,chunksize,fsblksize,globalrank,fileptr,&newfname));
}


/* obsolate functions for transaction, not in public interface */
int sion_startof_transaction_mpi(   int      sid  ) {
  int       rc = SION_SUCCESS;
  int       grank, lrank, lsize, blocksize, step=0;
  MPI_Comm  gComm, lComm;
  MPI_Status status;
  _sion_filedesc *sion_filedesc;
  const char *t;
  _sion_generic_gendata *sion_gendata;
  _mpi_api_commdata     *gcommgroup,*lcommgroup;
  
  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint_mpi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_parclose_mpi: invalid sion_filedesc %d", sid));
  }
  sion_gendata = (_sion_generic_gendata *)sion_filedesc->dataptr;
  gcommgroup   = (_mpi_api_commdata *) sion_gendata->comm_data_global;
  lcommgroup   = (_mpi_api_commdata *) sion_gendata->comm_data_local;
  gComm=gcommgroup->comm;
  lComm=lcommgroup->comm;
  MPI_Comm_rank(gComm, &grank);
  MPI_Comm_rank(lComm, &lrank);
  MPI_Comm_size(lComm, &lsize);

  t = _sion_getenv("SION_SERBLOCKSIZE");
  if (t) blocksize= atoi(t);
  else if(lsize>128) blocksize= 32;
  else if(lsize>64)  blocksize= 16;
  else if(lsize>16)  blocksize= 8;
  else blocksize=lsize;
  lcommgroup->blocksize=blocksize;

  /*                                                                      */ DPRINTFTS(grank, "enter start of transaction of sid");
  DPRINTFP((1, "sion_startof_transaction_mpi", grank, "enter start of transaction of sid %d blocksize=%d lrank=%d\n", sid,blocksize,lrank));

  lcommgroup->ts=_sion_get_time();

  if(lrank>blocksize) {
    if(sion_filedesc->filenumber==0)    fprintf(stderr,"sion_startof_transaction_mpi ts=%8.4fs on grank=%5d lrank=%3d waiting for message from %5d\n",_sion_get_time()-lcommgroup->ts,
						grank,lrank,lrank-blocksize);
    MPI_Recv(&step, 1, MPI_INT, lrank-blocksize, 1430, lComm, &status);
  }

  lcommgroup->step=step;
  if(sion_filedesc->filenumber==0)  fprintf(stderr,"sion_startof_transaction_mpi ts=%8.4fs on grank=%5d lrank=%3d starting transaction (blocksize=%d, step=%d)\n",_sion_get_time()-lcommgroup->ts,
					    grank,lrank,blocksize,step);
  DPRINTFP((1, "sion_startof_transaction_mpi", grank, "leave start of transaction of sid %d\n", sid));
  /*                                                                      */ DPRINTFTS(grank, "leave start of transaction of sid");
  return(rc);
}

int sion_endof_transaction_mpi  (   int      sid  ) {
  int       rc = SION_SUCCESS;
  int       grank, lrank, lsize, blocksize, step=1;
  MPI_Comm  gComm, lComm;
  _sion_filedesc *sion_filedesc;
  _sion_generic_gendata *sion_gendata;
  _mpi_api_commdata     *gcommgroup,*lcommgroup;

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint_mpi(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_parclose_mpi: invalid sion_filedesc %d", sid));
  }
  sion_gendata = (_sion_generic_gendata *)sion_filedesc->dataptr;
  gcommgroup   = (_mpi_api_commdata *) sion_gendata->comm_data_global;
  lcommgroup   = (_mpi_api_commdata *) sion_gendata->comm_data_local;
  gComm=gcommgroup->comm;
  lComm=lcommgroup->comm;
  MPI_Comm_rank(gComm, &grank);
  MPI_Comm_rank(lComm, &lrank);
  MPI_Comm_size(lComm, &lsize);

  blocksize=lcommgroup->blocksize;
  step= lcommgroup->step+1;

  /*                                                                      */ DPRINTFTS(grank, "enter end of transaction of sid");
  DPRINTFP((1, "sion_endof_transaction_mpi", grank, "enter end of transaction of sid %d blocksize=%d lrank=%d\n", sid,blocksize,lrank));


  if(lrank+blocksize<lsize) {
    if(sion_filedesc->filenumber==0) fprintf(stderr,"sion_endof_transaction_mpi   ts=%8.4fs on grank=%5d lrank=%3d sending step %2d  to %5d\n",_sion_get_time()-lcommgroup->ts,
					     grank,lrank,step,lrank+blocksize);
    MPI_Send(&step, 1, MPI_INT, lrank+blocksize, 1430, lComm);
  }

  if(sion_filedesc->filenumber==0) fprintf(stderr,"sion_endof_transaction_mpi   ts=%8.4fs on grank=%5d lrank=%3d end of transaction step=%2d\n",_sion_get_time()-lcommgroup->ts,
					   grank,lrank,step);

  DPRINTFP((1, "sion_endof_transaction_mpi", grank, "leave end of transaction of sid %d\n", sid));
  /*                                                                      */ DPRINTFTS(grank, "leave end of transaction of sid");
  return(rc);
}


/* end of ifdef MPI */
#endif
