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

#include "mpi.h"

#define USE_PMPIno
#ifdef USE_PMPI
#define MPI_Comm_rank      PMPI_Comm_rank
#define MPI_Comm_size      PMPI_Comm_size	  
#define MPI_Gather	   PMPI_Gather	  
#define MPI_Scatter	   PMPI_Scatter	  
#define MPI_Bcast	   PMPI_Bcast	  
#define MPI_Barrier	   PMPI_Barrier	  
#define MPI_Comm_split     PMPI_Comm_split    
#define MPI_Send           PMPI_Send
#define MPI_Recv           PMPI_Recv
#endif

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

#include "sion_mpi_cb_gen.h"

#ifdef SION_MPI

int _sion_register_callbacks_mpi(void) {
  int aid=0;
  aid=sion_generic_create_api("SIONlib_MPI_API");
  
 
  sion_generic_register_create_local_commgroup_cb(aid,&_sion_mpi_create_lcg_cb);
  sion_generic_register_free_local_commgroup_cb(aid,&_sion_mpi_free_lcg_cb);

  sion_generic_register_barrier_cb(aid,&_sion_mpi_barrier_cb);
  sion_generic_register_bcastr_cb(aid,&_sion_mpi_bcastr_cb);
  sion_generic_register_gatherr_cb(aid,&_sion_mpi_gatherr_cb);
  sion_generic_register_scatterr_cb(aid,&_sion_mpi_scatterr_cb);
  sion_generic_register_gathervr_cb(aid,&_sion_mpi_gathervr_cb);
  sion_generic_register_scattervr_cb(aid,&_sion_mpi_scattervr_cb);
  sion_generic_register_gather_and_execute_cb(aid,&_sion_mpi_gather_process_cb);
  sion_generic_register_execute_and_scatter_cb(aid,&_sion_mpi_process_scatter_cb);
  sion_generic_register_get_capability_cb(aid,&_sion_mpi_get_capability_cb);

  return(aid);
} 

int _sion_mpi_create_lcg_cb(void **local_commgroup, void *global_commgroup, 
			    int grank, int gsize, 
			    int lrank, int lsize,
			    int filenumber, int numfiles
			    )
{
  int       rc=0;
  _mpi_api_commdata* sapi_global = (_mpi_api_commdata *) global_commgroup;
  _mpi_api_commdata* commgroup=NULL;
  int create_lcomm=1, set_in_global=1, mrank=0, msize=1, color;
  
  DPRINTFP((256, "_mpi_create_lcg_cb", grank, " split now comm: grank=%d gsize=%d filenumber=%d, numfiles=%d, lrank=%d lsize=%d \n", 
	    grank, gsize, filenumber, numfiles, lrank, lsize));

  if(global_commgroup==NULL) {
    fprintf(stderr,"_mpi_create_lcg_cb: error no global commgroup given, aborting  ...\n");
    return(-1);
  }
  if(*local_commgroup!=NULL) {
    fprintf(stderr,"_mpi_create_lcg_cb: error local commgroup already initialized, aborting  ...\n");
    return(-1);
  }

  /* is local commgroup already set by calling program? */
  if(sapi_global->lcommgroup!=NULL) {
    /* use this communicator */
    if(sapi_global->lcommgroup->commset==0) {
      *local_commgroup=sapi_global->lcommgroup;
      create_lcomm=0;set_in_global=0; /* all is done */
      sapi_global->lcommgroup->commset=1;
    } else {
      create_lcomm=1;set_in_global=0; /* another communicator will be created */
    }
  }

  if(create_lcomm) {
    
    /* create new communicator */
    commgroup = (_mpi_api_commdata *) malloc(sizeof(_mpi_api_commdata));
    if (commgroup == NULL) {
      fprintf(stderr,"_mpi_create_lcg_cb: cannot allocate memory for local commgroup of size %lu, aborting ...\n",
	      (unsigned long) sizeof(_mpi_api_commdata));
      return(-1);
    }
    color=filenumber;
    if(filenumber==-1) color=MPI_UNDEFINED;

    rc = MPI_Comm_split(sapi_global->comm, color, lrank, &commgroup->comm);
    DPRINTFP((256, "_mpi_create_lcg_cb", grank, " rc=%d from MPI_Comm_split(comm,%d,%d,&newcomm)\n",rc,color,lrank));
    commgroup->local=1;  commgroup->commset=1; commgroup->lcommgroup=NULL;
    commgroup->commcreated=1;
    commgroup->rank=lrank;
    commgroup->size=lsize;
    
  }

  if(set_in_global) {
    sapi_global->lcommgroup=commgroup; /* needed for collective calls */
  }
  
  *local_commgroup=commgroup;

  if((filenumber!=-1) && commgroup) {
    MPI_Comm_rank(commgroup->comm, &mrank);
    MPI_Comm_size(commgroup->comm, &msize);
  }
  
  DPRINTFP((256, "_mpi_create_lcg_cb", grank, " leave rc=%d rank %d of %d\n",rc,mrank,msize));
  
  return rc;
}

int _sion_mpi_free_lcg_cb(void *local_commgroup) {
  int       rc = 0;
  _mpi_api_commdata* commgroup = (_mpi_api_commdata *) local_commgroup;
  
  if ( (commgroup->commset) && (commgroup->commcreated) ) {
    DPRINTFP((256, "_mpi_free_lcg_cb", commgroup->rank, " free now comm\n"));
    rc=MPI_Comm_free(&commgroup->comm);
    DPRINTFP((256, "_mpi_free_lcg_cb", commgroup->rank, " free now comm rc=%d\n",rc));
  }
  free(commgroup);
  return rc;
}

int _sion_mpi_barrier_cb(void *commdata)
{
  int       rc;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  rc = MPI_Barrier(commp);
  return rc;
}

int _sion_mpi_bcastr_cb(void *data, void *commdata, int dtype, int nelem, int root)
{
  int       rc;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  switch (dtype) {
  case _SION_INT32:
    rc = MPI_Bcast((sion_int32 *) data, nelem, SION_MPI_INT32, root, commp);
    break;
  case _SION_INT64:
    rc = MPI_Bcast((sion_int64 *) data, nelem, SION_MPI_INT64, root, commp);
    break;
  case _SION_CHAR:
    rc = MPI_Bcast((char *) data, nelem, MPI_CHAR, root, commp);
    break;
  default:
    rc = MPI_Bcast((sion_int64 *) data, nelem, SION_MPI_INT64, root, commp);
    break;
  }
  return rc;
}

int _sion_mpi_gatherr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  int       rc;
  int       size, rank;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;

  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, "_mpi_gatherr_cb", rank, " gatherr on %d of %d nelem=%d root=%d\n", rank, size, nelem, root));

  /* Dummy selects the type of gather */
  switch (dtype) {
  case _SION_INT32:
    rc = MPI_Gather((sion_int32 *) indata, nelem, SION_MPI_INT32, (sion_int32 *) outdata, nelem, SION_MPI_INT32, root, commp);
    break;
  case _SION_INT64:
    rc = MPI_Gather((sion_int64 *) indata, nelem, SION_MPI_INT64, (sion_int64 *) outdata, nelem, SION_MPI_INT64, root, commp);
    break;
  case _SION_CHAR:
    rc = MPI_Gather((char *) indata, nelem, MPI_CHAR, (char *) outdata, nelem, MPI_CHAR, root, commp);
    break;
  default:
    rc = MPI_Gather((sion_int64 *) indata, nelem, SION_MPI_INT64, (sion_int64 *) outdata, nelem, SION_MPI_INT64, root, commp);
    break;
  }


  return rc;
}

int _sion_mpi_scatterr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  int       rc;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  ONLY_DEBUG(int rank=sapi->rank);

  DPRINTFP((256, "_mpi_scatterr_cb", rank, " starting nelem=%d root=%d\n", nelem, root));

  switch (dtype) {
  case _SION_INT32:
    rc = MPI_Scatter((sion_int32 *) indata, nelem, SION_MPI_INT32, (sion_int32 *) outdata, nelem, SION_MPI_INT32, root, commp);
    break;
  case _SION_INT64:
    rc = MPI_Scatter((sion_int64 *) indata, nelem, SION_MPI_INT64, (sion_int64 *) outdata, nelem, SION_MPI_INT64, root, commp);
    break;
  case _SION_CHAR:
    rc = MPI_Scatter((char *) indata, nelem, MPI_CHAR, (char *) outdata, nelem, MPI_CHAR, root, commp);
    break;
  default:
    rc = MPI_Scatter((sion_int64 *) indata, nelem, SION_MPI_INT64, (sion_int64 *) outdata, nelem, SION_MPI_INT64, root, commp);
    break;
  }

  return rc;
}

int _sion_mpi_gathervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  int       rc;
  int       size, rank, t, offset;
  int       *displs=NULL;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  

  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, "_mpi_gathervr_cb", rank, " input nelem=%d root=%d indata=%x, outdata=%x\n", nelem, root, indata, outdata));

  /* compute displs and counts */
  if(rank==root) {
    displs = (int *) malloc(size * sizeof(int));
    if (displs == NULL) {
      fprintf(stderr,"_mpi_gathervr_cb: cannot allocate temporary memory of size %zu (displs), aborting ...\n",(size_t) size * sizeof(int));
      return(-1);
    }
    offset=0;
    for(t=0;t<size;t++) {
      displs[t]=offset;
      offset+=counts[t];
      /* DPRINTFP((256, "_mpi_gathervr_cb", rank, " after MPI_Gather %2d -> dpls=%2d count=%d\n", t,displs[t],counts[t])); */
    }
  }

  /* Dummy selects the type of gather */
  switch (dtype) {
  case _SION_INT32:
    rc = MPI_Gatherv((sion_int32 *) indata, nelem, SION_MPI_INT32, (sion_int32 *) outdata, counts, displs, SION_MPI_INT32, root, commp);
    break;
  case _SION_INT64:
    rc = MPI_Gatherv((sion_int64 *) indata, nelem, SION_MPI_INT64, (sion_int64 *) outdata, counts, displs, SION_MPI_INT64, root, commp);
    break;
  case _SION_CHAR:
    rc = MPI_Gatherv((char *) indata, nelem, MPI_CHAR, (sion_int32 *) outdata, counts, displs, MPI_CHAR, root, commp);
    break;
  default:
    rc = MPI_Gatherv((sion_int64 *) indata, nelem, SION_MPI_INT64, (sion_int64 *) outdata, counts, displs, SION_MPI_INT64, root, commp);
    break;
  }

  if(rank==root) {
    if(displs) free(displs);
  }
  return rc;
}

int _sion_mpi_scattervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  int       rc;
  int       size, rank, t, offset;
  int       *displs=NULL;
  _mpi_api_commdata* sapi= (_mpi_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  

  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, "_mpi_scattervr_cb", rank, " input nelem=%d root=%d\n", nelem, root));

  /* compute offset */
  if(rank==root) {
    displs = (int *) malloc(size * sizeof(int));
    if (displs == NULL) {
      fprintf(stderr,"_mpi_scattervr_cb: cannot allocate temporary memory of size %zu (displs), aborting ...\n",(size_t) size * sizeof(int));
      return(-1);
    }
    offset=0;
    for(t=0;t<size;t++) {
      displs[t]=offset;
      offset+=counts[t];
      DPRINTFP((256, "_mpi_scattervr_cb", rank, " after MPI_Gather %2d -> dpls=%2d sendcounts=%d\n", t,displs[t],counts[t]));
    }
  }

  /* Dummy selects the type of gather */
  switch (dtype) {
  case _SION_INT32:
    rc = MPI_Scatterv((sion_int32 *) outdata, counts, displs, SION_MPI_INT32, (sion_int32 *) indata, nelem, SION_MPI_INT32, root, commp);
    break;
  case _SION_INT64:
    rc = MPI_Scatterv((sion_int64 *) outdata, counts, displs, SION_MPI_INT64, (sion_int64 *) indata, nelem, SION_MPI_INT64, root, commp);
    break;
  case _SION_CHAR:
    rc = MPI_Scatterv((char *) outdata, counts, displs, MPI_CHAR, (sion_int32 *) indata, nelem, MPI_CHAR, root, commp);
    break;
  default:
    rc = MPI_Scatterv((sion_int64 *) outdata, counts, displs, SION_MPI_INT64, (sion_int64 *) indata, nelem, SION_MPI_INT64, root, commp);
    break;
  }

  if(rank==root) {
    if(displs) free(displs);
  }
  return rc;
}

/* end of ifdef MPI */
#endif
