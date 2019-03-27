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
 * \brief Hybrid (MPI+OpenMP) callbacks to generic interface
 * \author David Montoya
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
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"

#include "sion_ompi_cb_gen.h"
#include "sion_ompi_internal_gen.h"

#ifdef SION_OMPI

#include "omp.h"


static void *__ompi_global_pointer;
static int _sion_opmi_grc=SION_SUCCESS;

int _sion_ompi_size_of_dtype(int dtype);
void * __sion_ompi_share_ptr(void * in_ptr);


int _sion_register_callbacks_ompi(void) {
  int aid=0;
  aid=sion_generic_create_api("SIONlib_OMPI_API");
  
 
  sion_generic_register_create_local_commgroup_cb(aid,&_sion_ompi_create_lcg_cb);
  sion_generic_register_free_local_commgroup_cb(aid,&_sion_ompi_free_lcg_cb);

  sion_generic_register_barrier_cb(aid,&_sion_ompi_barrier_cb);
  sion_generic_register_bcastr_cb(aid,&_sion_ompi_bcastr_cb);
  sion_generic_register_gatherr_cb(aid,&_sion_ompi_gatherr_cb);
  sion_generic_register_scatterr_cb(aid,&_sion_ompi_scatterr_cb);
  sion_generic_register_gathervr_cb(aid,&_sion_ompi_gathervr_cb);
  sion_generic_register_scattervr_cb(aid,&_sion_ompi_scattervr_cb);
  sion_generic_register_gather_and_execute_cb(aid,&_sion_ompi_gather_process_cb);
  sion_generic_register_execute_and_scatter_cb(aid,&_sion_ompi_process_scatter_cb);
  sion_generic_register_get_capability_cb(aid,&_sion_ompi_get_capability_cb);

  return(aid);
} 

#define DFUNCTION "_sion_ompi_create_lcg_cb"
int _sion_ompi_create_lcg_cb(void **local_commgroup, void *global_commgroup, 
			     int grank, int gsize, 
			     int lrank, int lsize,
			     int filenumber, int numfiles
			     )
{
  int       rc=SION_STD_SUCCESS;
  _ompi_api_commdata* sapi_global = (_ompi_api_commdata *) global_commgroup;
  _ompi_api_commdata* commgroup=NULL;
  int create_lcomm=1, set_in_global=1, mrank=0, msize=1, color;
    
  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " split now comm on master: grank=%d gsize=%d filenumber=%d, numfiles=%d, lrank=%d lsize=%d \n", 
	    grank, gsize, filenumber, numfiles, lrank, lsize));
#pragma omp master
  {
    _sion_opmi_grc=SION_STD_SUCCESS;
    DPRINTFP((256, "_mpi_create_lcg_cb", _SION_DEFAULT_RANK, " I'm on master\n",rc));
  }
    
  if(global_commgroup==NULL) {
    fprintf(stderr,"_mpi_create_lcg_cb: error no global commgroup given, aborting  ...\n");
    return(SION_STD_NOT_SUCCESS);
  }
  if(*local_commgroup!=NULL) {
    fprintf(stderr,"_mpi_create_lcg_cb: error local commgroup already initialized, aborting  ...\n");
    return(SION_STD_NOT_SUCCESS);
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
    commgroup = (_ompi_api_commdata *) malloc(sizeof(_ompi_api_commdata));
    if (commgroup == NULL) {
      fprintf(stderr,"_ompi_create_lcg_cb: cannot allocate memory for commgroup of size %zu, aborting ...\n",
	      sizeof(_ompi_api_commdata));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }

    commgroup->commset=0; commgroup->lcommgroup=NULL;
    commgroup->commcreated=0;
    commgroup->rank=lrank;
    commgroup->size=lsize;
    commgroup->num_threads=sapi_global->num_threads;
    commgroup->thread_num=sapi_global->thread_num;

#pragma omp master
    {
      color=filenumber;
      if(filenumber==-1) color=MPI_UNDEFINED;
      _sion_opmi_grc = MPI_Comm_split(sapi_global->comm, color, lrank, &commgroup->comm);
      DPRINTFP((256, "_ompi_create_lcg_cb", grank, " rc=%d from MPI_Comm_split(comm,%d,%d,&newcomm)\n",rc,color,lrank));
      commgroup->local=1;  commgroup->commset=1; 
    } /* omp master */
    

  if(set_in_global) {
    sapi_global->lcommgroup=commgroup; /* needed for collective calls */
  }
  
  *local_commgroup=commgroup;
  
  }

  {
#pragma omp barrier
  }

#pragma omp master
    {
      if(filenumber!=-1) {
	MPI_Comm_rank(commgroup->comm, &mrank);
	MPI_Comm_size(commgroup->comm, &msize);
      }
    
      DPRINTFP((256, "_mpi_create_lcg_cb", grank, " leave rc=%d rank %d of %d\n",rc,mrank,msize));
    }

  rc=_sion_opmi_grc;
  return rc;
}
#undef DFUNCTION

#define DFUNCTION "_sion_ompi_free_lcg_cb"
int _sion_ompi_free_lcg_cb(void *local_commgroup) {
  int       rc=SION_STD_SUCCESS;
  _ompi_api_commdata* commgroup = (_ompi_api_commdata *) local_commgroup;
#pragma omp master
  {
    _sion_opmi_grc=SION_STD_SUCCESS;

    if ( (commgroup->commset) && (commgroup->commcreated) ) {
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " free now comm\n"));
      _sion_opmi_grc=MPI_Comm_free(&commgroup->comm);
      DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " free now comm rc=%d\n",_sion_opmi_grc));
    }
    free(commgroup);
  }

  {
#pragma omp barrier
  }
  rc=_sion_opmi_grc;
  return rc;
}
#undef DFUNCTION


#define DFUNCTION "_sion_ompi_barrier_cb"
int _sion_ompi_barrier_cb(void *commdata)
{
  int       rc=SION_STD_SUCCESS;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm commgroup;
  {
#pragma omp barrier
  }
#pragma omp master
  {
    commgroup = sapi->comm;
    DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " performing MPI barrier now\n"));
    _sion_opmi_grc = MPI_Barrier(commgroup);
  }

  {
#pragma omp barrier
  }
  rc=_sion_opmi_grc;
  {
#pragma omp barrier
  }
  return rc;
}
#undef DFUNCTION


#define DFUNCTION "_sion_ompi_bcastr_cb"
int _sion_ompi_bcastr_cb(void *data, void *commdata, int dtype, int nelem, int root)
{
  int       rc=SION_STD_SUCCESS;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm commgroup;
  void *help;
  
  /* first distribute data to master threads (over MPI) */
#pragma omp master
  {
    commgroup = sapi->comm;
    switch (dtype) {
    case _SION_INT32:
      _sion_opmi_grc = MPI_Bcast((sion_int32 *) data, nelem, SION_MPI_INT32, root, commgroup);
      break;
    case _SION_INT64:
      _sion_opmi_grc = MPI_Bcast((sion_int64 *) data, nelem, SION_MPI_INT64, root, commgroup);
      break;
    case _SION_CHAR:
      _sion_opmi_grc = MPI_Bcast((char *) data, nelem, MPI_CHAR, root, commgroup);
      break;
    default:
      _sion_opmi_grc = MPI_Bcast((sion_int64 *) data, nelem, SION_MPI_INT64, root, commgroup);
      break;
    }

  }

 
  /* share data ptr among threads */
  help=__sion_ompi_share_ptr((void *) data);
  
  /* copy data to local val on non-master threads */
  if((omp_get_thread_num()!=root) && (help != NULL)) {
    memcpy(data,help,nelem*_sion_ompi_size_of_dtype(dtype));
  }

  {
#pragma omp barrier
  }
  rc=_sion_opmi_grc;
  {
#pragma omp barrier
  }

return rc;
}
#undef DFUNCTION


/* master receives data in outdata, number of OpenMP threads is equal on all tasks */
#define DFUNCTION "_sion_ompi_gatherr_cb"
int _sion_ompi_gatherr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  int       rc=SION_STD_SUCCESS;
  int       mroot;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm commgroup;
  void    *helpdata, *help;
  ONLY_DEBUG(int rank=sapi->rank;)
  ONLY_DEBUG(int size=sapi->size;)

  mroot=_sion_map_rank_ompi_to_mpi(root,sapi->num_threads);

  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " starting on %d of %d nelem=%d root=%d (MPI: %d)\n", rank, size, nelem, root, mroot));

  /* allocate temp buffer */
#pragma omp master
  {
    _sion_opmi_grc=SION_STD_SUCCESS;

    helpdata = (int *) malloc(sapi->num_threads * nelem * _sion_ompi_size_of_dtype(dtype));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %lu (helpdata), aborting ...\n",
	      (unsigned long) sapi->num_threads * nelem * _sion_ompi_size_of_dtype(dtype));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  }

  /* share data ptr among threads, internal barrier */
  help=__sion_ompi_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmi_grc) return(_sion_opmi_grc);
  
  /* copy data from indata on non-master threads */
  memcpy((char *)help+sapi->thread_num*nelem*_sion_ompi_size_of_dtype(dtype), /* to */
	 indata,		                                      /* from */
	 nelem*_sion_ompi_size_of_dtype(dtype));

  {
#pragma omp barrier
  }
  
  /* gather on MPI level on master thread */
#pragma omp master
  {
    commgroup = sapi->comm;
    switch (dtype) {
    case _SION_INT32:
      _sion_opmi_grc = MPI_Gather((sion_int32 *) help, sapi->num_threads*nelem, SION_MPI_INT32, (sion_int32 *) outdata, sapi->num_threads*nelem, SION_MPI_INT32, mroot, commgroup);
      break;
    case _SION_INT64:
      _sion_opmi_grc = MPI_Gather((sion_int64 *) help, sapi->num_threads*nelem, SION_MPI_INT64, (sion_int64 *) outdata, sapi->num_threads*nelem, SION_MPI_INT64, mroot, commgroup);
      break;
    case _SION_CHAR:
      _sion_opmi_grc = MPI_Gather((char *) help,       sapi->num_threads*nelem, MPI_CHAR,       (char *) outdata,       sapi->num_threads*nelem, MPI_CHAR, mroot, commgroup);
      break;
    default:
      _sion_opmi_grc = MPI_Gather((sion_int64 *) help, sapi->num_threads*nelem, SION_MPI_INT64, (sion_int64 *) outdata, sapi->num_threads*nelem, SION_MPI_INT64, mroot, commgroup);
      break;
    }

    free(helpdata);
  }

  /* sychronize */
  {
#pragma omp barrier
  }
  rc=_sion_opmi_grc;
  {
#pragma omp barrier
  }

  return rc;
}
#undef DFUNCTION

/* indata (root) -> outdata */
#define DFUNCTION "_sion_ompi_scatterr_cb"
int _sion_ompi_scatterr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  int       rc=SION_STD_SUCCESS, mroot;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm commgroup;
  void    *helpdata, *help;
  ONLY_DEBUG(int rank=sapi->rank;)
  ONLY_DEBUG(int size=sapi->size;)

  mroot=_sion_map_rank_ompi_to_mpi(root,sapi->num_threads);

  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " starting on %d of %d nelem=%d root=%d (MPI: %d)\n", rank, size, nelem, root, mroot));

  /* allocate temp buffer */
#pragma omp master
  {
    _sion_opmi_grc=0;

    helpdata = (int *) malloc(sapi->num_threads * nelem * _sion_ompi_size_of_dtype(dtype));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_scatterr_cb: cannot allocate temporary memory of size %lu (helpdata), aborting ...\n",
	      (unsigned long) sapi->num_threads * nelem * _sion_ompi_size_of_dtype(dtype));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  }

  /* share data ptr among threads, internal barrier */
  help=__sion_ompi_share_ptr((void *) helpdata);
  
  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);

  {
#pragma omp barrier
  }
  
  /* scatter data on MPI level */
#pragma omp master
  {
    commgroup = sapi->comm;
  switch (dtype) {
    case _SION_INT32:
      _sion_opmi_grc = MPI_Scatter((sion_int32 *) indata, sapi->num_threads*nelem, SION_MPI_INT32, (sion_int32 *) help, sapi->num_threads*nelem, SION_MPI_INT32, mroot, commgroup);
      break;
    case _SION_INT64:
      _sion_opmi_grc = MPI_Scatter((sion_int64 *) indata, sapi->num_threads*nelem, SION_MPI_INT64, (sion_int64 *) help, sapi->num_threads*nelem, SION_MPI_INT64, mroot, commgroup);
      break;
    case _SION_CHAR:
      _sion_opmi_grc = MPI_Scatter((char *) indata, sapi->num_threads*nelem, MPI_CHAR, (char *) help, sapi->num_threads*nelem, MPI_CHAR, mroot, commgroup);
      break;
    default:
      _sion_opmi_grc = MPI_Scatter((sion_int64 *) indata, sapi->num_threads*nelem, SION_MPI_INT64, (sion_int64 *) help, sapi->num_threads*nelem, SION_MPI_INT64, mroot, commgroup);
      break;
    }

  } /* omp master */

  /* synchronize */
  {
#pragma omp barrier
  }

  /* copy data from indata on non-master threads */
  memcpy(outdata,                                                      /* to */ 
	 (char *)help+sapi->thread_num*nelem*_sion_ompi_size_of_dtype(dtype), /* from */ 
	 nelem*_sion_ompi_size_of_dtype(dtype));

  /* synchronize */
  {
#pragma omp barrier
  }

  /* free temp buffer on master */
#pragma omp master
  {
    free(helpdata);
  }

  rc=_sion_opmi_grc;
  {
#pragma omp barrier
  }

  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " leaving nelem=%d root=%d, rc=%d\n", nelem, root, rc));

  return rc;
}
#undef DFUNCTION



/* master receives data in outdata, number of OpenMP threads is equal on all tasks */
#define DFUNCTION "_sion_ompi_gathervr_cb"
int _sion_ompi_gathervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  int       rc=SION_STD_SUCCESS;
  int       m, t, offset, mroot, mcount, toffset;
  int      *mcounts=NULL,*mdispls=NULL;
  int      *tcounts=NULL,*tdispls=NULL;
  void     *helpdata, *help;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm  commgroup;
  int rank=sapi->rank; 
  int size=sapi->size; 

  mroot=_sion_map_rank_ompi_to_mpi(root,sapi->num_threads);

  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " input nelem=%d root=%d indata=%x, outdata=%x\n", nelem, root, indata, outdata));


  /* STEP1: collect counts on thread level */
#pragma omp master
  {
    helpdata = (int *) malloc(sapi->num_threads * sizeof(int));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (helpdata), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  tcounts=__sion_ompi_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);
 
  tcounts[sapi->thread_num]=nelem;

  /* STEP2: calculate offsets on thread level */
#pragma omp master
  {
    helpdata = (int *) malloc(sapi->num_threads * sizeof(int));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (helpdata), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  tdispls=__sion_ompi_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);

#pragma omp master
  {
    offset=0;
    for(t=0;t<size;t++) {
      tdispls[t]=offset;
      offset+=tcounts[t];
    }
    mcount=tdispls[size=1];
  } /* omp master */

  /* synchronize */
  {
#pragma omp barrier
  }

  /* STEP3: get offset on thread level */
  toffset=tdispls[sapi->thread_num];
  

  /* STEP4: allocate temp buffer on master */
#pragma omp master
  {
    helpdata = (int *) malloc(mcount * _sion_ompi_size_of_dtype(dtype));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %lu (helpdata), aborting ...\n",
	      (unsigned long) mcount * _sion_ompi_size_of_dtype(dtype));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* STEP5: gather data on thread level */

  /* share data ptr among threads, internal barrier */
  help=__sion_ompi_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);
  
  /* copy data from indata on non-master threads */
  memcpy((char *)help+toffset*_sion_ompi_size_of_dtype(dtype), /* to */
	 indata,                                      /* from */
	 nelem*_sion_ompi_size_of_dtype(dtype));

  
  
  /* STEP6: gather data on MPI level */
#pragma omp master
  {
    /* allocate compute displs and counts */
    if(rank==root) {

      mcounts = (int *) malloc(size * sizeof(int));
      if (mcounts == NULL) {
	fprintf(stderr,"_mpi_gathervr_cb: cannot allocate temporary memory of size %zu (mcounts), aborting ...\n",(size_t) size * sizeof(int));
	_sion_opmi_grc=SION_STD_NOT_SUCCESS;
      }

      if(_sion_opmi_grc==SION_SUCCESS) {
	mdispls = (int *) malloc(size * sizeof(int));
	if (mdispls == NULL) {
	  fprintf(stderr,"_mpi_gathervr_cb: cannot allocate temporary memory of size %zu (mdispls), aborting ...\n",(size_t) size * sizeof(int));
	  _sion_opmi_grc=SION_STD_NOT_SUCCESS;
	}
      }

      /* calculate counts and displs on MPI level */
      if(_sion_opmi_grc==SION_SUCCESS) {
	for(m=0;m<size;m++) {
	  mcounts[m]=0;
	  for(t=0;t<sapi->num_threads;t++) {
	    mcounts[m]+=counts[m*sapi->num_threads+t];
	  }
	}

	offset=0;
	for(m=0;m<size;m++) {
	  mdispls[m]=offset;
	  offset+=mcounts[m];
	  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " after MPI_Gather %2d -> dpls=%2d count=%d\n", m,mdispls[m],mcounts[m]));
	}
      }
    
    }
  } /* omp master */
  
  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);
 

  /* call MPI gatherv */
#pragma omp master
  {
    commgroup = sapi->comm;
    switch (dtype) {
    case _SION_INT32:
      _sion_opmi_grc = MPI_Gatherv((sion_int32 *) help, mcount, SION_MPI_INT32, (sion_int32 *) outdata, mcounts, mdispls, SION_MPI_INT32, mroot, commgroup);
      break;
    case _SION_INT64:
      _sion_opmi_grc = MPI_Gatherv((sion_int64 *) help, mcount, SION_MPI_INT64, (sion_int64 *) outdata, mcounts, mdispls, SION_MPI_INT64, mroot, commgroup);
      break;
    case _SION_CHAR:
      _sion_opmi_grc = MPI_Gatherv((char *) help, mcount, MPI_CHAR, (sion_int32 *) outdata, mcounts, mdispls, MPI_CHAR, mroot, commgroup);
      break;
    default:
      _sion_opmi_grc = MPI_Gatherv((sion_int64 *) help, mcount, SION_MPI_INT64, (sion_int64 *) outdata, mcounts, mdispls, SION_MPI_INT64, mroot, commgroup);
      break;
    }

  } /* omp master */

#pragma omp master
  {
    if(tcounts) free(tcounts);
    if(tdispls) free(tdispls);
    if(help) free(help);

    if(rank==root) {
      if(mcounts) free(mcounts);
      if(mdispls) free(mdispls);
    }
  }

  /* synchronize */
  {
#pragma omp barrier
  }
  rc=_sion_opmi_grc;
  {
#pragma omp barrier
  }

  return rc;
}
#undef DFUNCTION



/* outdata (root) -> indata */
#define DFUNCTION "_sion_ompi_scatterr_cb"
int _sion_ompi_scattervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  int       rc=SION_STD_SUCCESS;
  int       m, t, offset, mroot, mcount, toffset;
  int      *mcounts=NULL,*mdispls=NULL;
  int      *tcounts=NULL,*tdispls=NULL;
  void     *helpdata, *help;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm  commgroup;
  int rank=sapi->rank; 
  int size=sapi->size; 
  
  mroot=_sion_map_rank_ompi_to_mpi(root,sapi->num_threads);

  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " input nelem=%d root=%d indata=%x, outdata=%x\n", nelem, root, indata, outdata));


  /* STEP1: collect counts on thread level */
#pragma omp master
  {
    _sion_opmi_grc=SION_STD_SUCCESS;

    helpdata = (int *) malloc(sapi->num_threads * sizeof(int));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (helpdata), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  tcounts=__sion_ompi_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);
 
  tcounts[sapi->thread_num]=nelem;

  /* STEP2: calculate offsets on thread level */
#pragma omp master
  {
    helpdata = (int *) malloc(sapi->num_threads * sizeof(int));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (helpdata), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  tdispls=__sion_ompi_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);

#pragma omp master
  {
    offset=0;
    for(t=0;t<size;t++) {
      tdispls[t]=offset;
      offset+=tcounts[t];
    }
    mcount=tdispls[size=1];
  } /* omp master */

  /* synchronize */
  {
#pragma omp barrier
  }

  /* STEP3: get offset on thread level */
  toffset=tdispls[sapi->thread_num];


  /* STEP4: allocate temp buffer on master */
#pragma omp master
  {
    helpdata = (int *) malloc(mcount * _sion_ompi_size_of_dtype(dtype));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %lu (helpdata), aborting ...\n",
	      (unsigned long) mcount * _sion_ompi_size_of_dtype(dtype));
      _sion_opmi_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  help=__sion_ompi_share_ptr((void *) helpdata);

  /* STEP5: scatter data on MPI level */
#pragma omp master
  {
    /* allocate compute displs and counts */
    if(rank==root) {

      mcounts = (int *) malloc(size * sizeof(int));
      if (mcounts == NULL) {
	fprintf(stderr,"_mpi_gathervr_cb: cannot allocate temporary memory of size %zu (mcounts), aborting ...\n",(size_t) size * sizeof(int));
	_sion_opmi_grc=SION_STD_NOT_SUCCESS;
      }

      if(_sion_opmi_grc==SION_SUCCESS) {
	mdispls = (int *) malloc(size * sizeof(int));
	if (mdispls == NULL) {
	  fprintf(stderr,"_mpi_gathervr_cb: cannot allocate temporary memory of size %zu (mdispls), aborting ...\n",(size_t) size * sizeof(int));
	  _sion_opmi_grc=SION_STD_NOT_SUCCESS;
	}
      }

      /* calculate counts and displs on MPI level */
      if(_sion_opmi_grc==SION_SUCCESS) {
	for(m=0;m<size;m++) {
	  mcounts[m]=0;
	  for(t=0;t<sapi->num_threads;t++) {
	    mcounts[m]+=counts[m*sapi->num_threads+t];
	  }
	}

	offset=0;
	for(m=0;m<size;m++) {
	  mdispls[m]=offset;
	  offset+=mcounts[m];
	  DPRINTFP((256, DFUNCTION, _SION_DEFAULT_RANK, " after MPI_Gather %2d -> dpls=%2d count=%d\n", m,mdispls[m],mcounts[m]));
	}
      }
    
    }
  } /* omp master */
  
  /* check return code from malloc */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);

  /* call MPI scatterv */
#pragma omp master
  {
    commgroup = sapi->comm;    
    switch (dtype) {
    case _SION_INT32:
      _sion_opmi_grc = MPI_Scatterv((sion_int32 *) outdata, mcounts, mdispls, SION_MPI_INT32, (sion_int32 *) help, mcount, SION_MPI_INT32, mroot, commgroup);
      break;
    case _SION_INT64:
      _sion_opmi_grc = MPI_Scatterv((sion_int64 *) outdata, mcounts, mdispls, SION_MPI_INT64, (sion_int64 *) help, mcount, SION_MPI_INT64, mroot, commgroup);
      break;
    case _SION_CHAR:
      _sion_opmi_grc = MPI_Scatterv((char *) outdata, mcounts, mdispls, MPI_CHAR, (sion_int32 *) help, mcount, MPI_CHAR, mroot, commgroup);
      break;
    default:
      _sion_opmi_grc = MPI_Scatterv((sion_int64 *) outdata, mcounts, mdispls, SION_MPI_INT64, (sion_int64 *) help, mcount, SION_MPI_INT64, mroot, commgroup);
      break;
    }

  } /* omp master */

  /* check return code from MPI call */
  if(_sion_opmi_grc!=SION_STD_SUCCESS) return(_sion_opmi_grc);
  
  /* STEP6: scatterv data on thread level */

  
  /* copy data from indata on non-master threads */
  memcpy(indata,                                         /* to */
	 (char *)help+toffset*_sion_ompi_size_of_dtype(dtype),   /* from */
	 nelem*_sion_ompi_size_of_dtype(dtype));
  
  
  /* cleanup */
#pragma omp master
  {
    if(tcounts) free(tcounts);
    if(tdispls) free(tdispls);
    if(help) free(help);

    if(rank==root) {
      if(mcounts) free(mcounts);
      if(mdispls) free(mdispls);
    }
  }

  /* synchronize */
  {
#pragma omp barrier
  }
  rc=_sion_opmi_grc;
  {
#pragma omp barrier
  }

  return rc;
}
#undef DFUNCTION



/* share in_ptr given on master with all other threads, return value is the shared ptr */
#define DFUNCTION "__sion_ompi_share_ptr"
void * __sion_ompi_share_ptr(void * in_ptr) {
  void *out_ptr;
  
#pragma omp master
  __ompi_global_pointer = in_ptr;

 
  {
#pragma omp barrier
  }

  out_ptr=__ompi_global_pointer;

  return(out_ptr);

}
#undef DFUNCTION

int _sion_ompi_size_of_dtype(int dtype) {
  switch (dtype) {
  case _SION_INT32: return(sizeof(sion_int32));			  break;
  case _SION_INT64: return(sizeof(sion_int64));			  break;
  case _SION_CHAR:  return(sizeof(char));			  break;
  default:          return(sizeof(sion_int64));
  }
}

/* end of ifdef SION_OMPI */
#endif
