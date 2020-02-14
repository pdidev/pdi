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

#include "sion_ompi_internal_gen.h"
#include "sion_ompi_cb_gen.h"

#ifdef SION_OMPI

static void *__ompicol_global_pointer;
static int _sion_opmicol_grc=SION_SUCCESS;

int _sion_ompicol_size_of_dtype(int dtype);
void * __sion_ompicol_share_ptr(void * in_ptr);

#define DFUNCTION "_ompi_gather_execute_cb"
int _sion_ompi_gather_process_cb(const void *indata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				 void *commdata,  int collector, int range_start, int range_end, int sid, 
				 int process_cb(const void *,sion_int64 *, int ) ) {
  int       rc=SION_SUCCESS;
  int       t, startsignal=1,mrank,mt,tt, mcollector;
  MPI_Status status;
  char      *p, *buffer; 
  void      *helpdata;
  const void *p_data;
  void       const **indatas;
  sion_int64 **specs, *p_spec;
  sion_int64 bytestorecv, bytestosend, datasize;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm   commgroup;
  int rank=sapi->rank; 

  DPRINTFP((256, DFUNCTION, rank, " input collector=%d range_start=%d range_end=%d sid=%d\n", collector,range_start,range_end, sid));


  /* STEP1: collect info on thread level: specs -> pointer to spec on thread, indatas -> pointer to indata on thread */
#pragma omp master
  {
    _sion_opmicol_grc=SION_STD_SUCCESS;

    helpdata = (void *) malloc(sapi->num_threads * sizeof(sion_int64 *));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (helpdata), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int *));
      _sion_opmicol_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  specs = (sion_int64 **)__sion_ompicol_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmicol_grc!=SION_STD_SUCCESS) return(_sion_opmicol_grc);

  /* store Info about spec */
  specs[sapi->thread_num]= spec;

  /* synchronize */
  {
#pragma omp barrier
  }

#pragma omp master
  {
    helpdata = (void *) malloc(sapi->num_threads * sizeof(const void *));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (tcounts), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int *));
      _sion_opmicol_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  indatas = (void const **)__sion_ompicol_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmicol_grc!=SION_STD_SUCCESS) return(_sion_opmicol_grc);

  /* store info about spec */
  indatas[sapi->thread_num] = indata;


  /* synchronize */
  {
#pragma omp barrier
  }

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "store SPECS[%d]=%x (%x)\n", sapi->thread_num,specs[sapi->thread_num], spec));
  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "store DATAS[%d]=%x (%x)\n", sapi->thread_num,indatas[sapi->thread_num], indata));
  
  
#pragma omp master
  {
    for(tt=0;tt<sapi->num_threads;tt++) {
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "MASTER SPECS[%d]=%x\n", tt,specs[tt]));
      DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "MASTER DATAS[%d]=%x\n", tt,indatas[tt]));
    }
  }  
  
  /* STEP2: proceed on MPI level, master threads have all needed info  */
#pragma omp master
  {
    commgroup = sapi->comm;    
    
    if(rank == collector) {
      /* its the collector */
      
      mrank=_sion_map_rank_ompi_to_mpi(rank,sapi->num_threads);
 
      /* allocate buffer */
      buffer = (char *) malloc(fsblksize * sizeof(char));
      if (buffer == NULL) {
	_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_mpi_gather_process_cb: cannot allocate temporary memory of size %lu (buffer), aborting ...\n",
				(unsigned long) fsblksize * sizeof(char));
	_sion_opmicol_grc=SION_STD_NOT_SUCCESS;
      }
      
      /* scan all other tasks */
      for(t=range_start;t<=range_end;t++) {
	
	mt=_sion_map_rank_ompi_to_mpi(t,sapi->num_threads);
	
	
	if(mt==mrank) {
	  /* thread is on same MPI rank */
	  tt=_sion_map_rank_ompi_to_thread_num(t,sapi->num_threads);

	  /* process data directly */
	  p_spec=specs[tt];
	  p_data=indatas[tt];
	  _sion_opmicol_grc=process_cb(p_data,p_spec, sid);
	  
	} else {

	  /* receive spec */
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wait for spec from %d\n", mt));
	  MPI_Recv(spec, spec_len, SION_MPI_INT64, mt, 1534, commgroup, &status);
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector got spec from %d (%lld,%lld)\n", 
		    mt, (long long) spec[0], (long long) spec[1]));

	  /* send signal to send data */
	  if(spec[0]!=-1) { 	/* no error on sender */
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector send signal to %d\n", mt));
	    MPI_Send(&startsignal, 1, MPI_INT, mt, 1535, commgroup);
	  }

	  /* get and write data */
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector start to process data of size %lld at offset %lld\n", 
		    (long long) spec[1], (long long) spec[0]));

	  bytestorecv=spec[1];
      
	  /* loop over data parts */
	  while(bytestorecv>0) {
	    if(bytestorecv>fsblksize) datasize=fsblksize;
	    else                      datasize=bytestorecv;

	    /* receive portion or all data */
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wait for data block from %d\n", mt));
	    MPI_Recv(buffer, datasize, MPI_CHAR, mt, 1536, commgroup, &status);
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector got data block from %d datasize=%lld bytestorecv=%lld\n", 
		      mt, (long long) datasize, (long long) bytestorecv));

	    spec[1]=datasize; 	/* adjust size */

	    /* process data with callback function */
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector process data block of size %lld at pos %lld\n", 
		      (long long) spec[1], (long long) spec[0]));
	
	    _sion_opmicol_grc=process_cb(buffer,spec, sid);
	
	    if(_sion_opmicol_grc != SION_STD_SUCCESS) {
	      _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_ompi_gather_process_cb: problems writing data ...\n");
	    }

	    /* advance counter */
	    bytestorecv-=datasize;spec[0]+=datasize;


	  }
	} /* not on local MPI rank */
      
      }
    
      /* remove buffer */
      if (buffer) free(buffer);

    } else {
      /* its a sender */

      mcollector=_sion_map_rank_ompi_to_mpi(collector,sapi->num_threads);

      /* send data for all threads on MPI rank */
      for(tt=0;tt<sapi->num_threads;tt++) {

	/* send spec to collector */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender send spec to %d (%lld,%lld)\n", 
		  collector,(long long) specs[tt][0], (long long) specs[tt][1]));
	rc=MPI_Send(specs[tt], spec_len, SION_MPI_INT64, mcollector, 1534, commgroup);
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender sent spec to %d rc=%d\n", mcollector,rc));
	
	/* wait for start signal */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender wait for signal from %d\n", mcollector));
	MPI_Recv(&startsignal, 1, MPI_INT, mcollector, 1535, commgroup, &status);
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender got signal from %d\n", mcollector));

	/* send data in chunks of fsblksize */
	bytestosend=specs[tt][1];
	p=(char *) indatas[tt];
	while(bytestosend>0) {
	  if(bytestosend>fsblksize) datasize=fsblksize;
	  else                      datasize=bytestosend;
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender send data block to %d of size %lld\n", mcollector, (long long) datasize));
	  MPI_Send(p, datasize, MPI_CHAR, mcollector, 1536, commgroup);
	  bytestosend-=datasize;p+=datasize;
	}
      }	/* for all threads */
    } /* sender */

  } /* omp master */

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave collector=%d rc=%d\n", collector, rc ));

  /* synchronize */
  {
#pragma omp barrier
  }
  rc=_sion_opmicol_grc;
  {
#pragma omp barrier
  }
  return rc;
}
#undef DFUNCTION


#define DFUNCTION "_ompi_process_scatter_cb"
int _sion_ompi_process_scatter_cb(void *outdata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				  void *commdata,  int collector, int range_start, int range_end, int sid, 
				  int process_cb(void *,sion_int64 *, int ) ) {
  int       rc=SION_SUCCESS;
  int       t, startsignal=1, count, mrank, mt, tt, mcollector;
  MPI_Status status;
  char      *p, *buffer; 
  void      *helpdata;
  void       **outdatas;
  sion_int64 **specs;
  sion_int64 bytestorecv, bytestosend, datasize;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  MPI_Comm   commgroup;
  int rank=sapi->rank; 

  DPRINTFP((256, DFUNCTION, rank, " input collector=%d range_start=%d range_end=%d sid=%d\n", collector,range_start,range_end, sid));

  /* STEP1: collect info on thread level: specs -> pointer to spec on thread, outdatas -> pointer to outdata on thread */
#pragma omp master
  {
    _sion_opmicol_grc=SION_STD_SUCCESS;

    helpdata = (void *) malloc(sapi->num_threads * sizeof(sion_int64 *));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (helpdata), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int *));
      _sion_opmicol_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  specs = (sion_int64 **)__sion_ompicol_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmicol_grc!=SION_STD_SUCCESS) return(_sion_opmicol_grc);

  /* store Info about spec */
  specs[sapi->thread_num]= spec;

  /* synchronize */
  {
#pragma omp barrier
  }

#pragma omp master
  {
    helpdata = (void *) malloc(sapi->num_threads * sizeof(void *));
    if (helpdata == NULL) {
      fprintf(stderr,"_sion_ompi_gathervr_cb: cannot allocate temporary memory of size %zu (helpdata), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int *));
      _sion_opmicol_grc=SION_STD_NOT_SUCCESS;
    }
  } /* omp master */

  /* share data ptr among threads, internal barrier */
  outdatas = (void **)__sion_ompicol_share_ptr((void *) helpdata);

  /* check return code from malloc */
  if(_sion_opmicol_grc!=SION_STD_SUCCESS) return(_sion_opmicol_grc);

  /* store info about spec */
  outdatas[sapi->thread_num] = outdata;

  /* synchronize */
  {
#pragma omp barrier
  }

  
  /* STEP2: proceed on MPI level, master threads have all needed info  */
#pragma omp master
  {
    commgroup = sapi->comm;    
    
    
    if(rank == collector) {
      /* its the collector */
 
      /* allocate buffer */
      buffer = (char *) malloc(fsblksize * sizeof(char));
      if (buffer == NULL) {
	_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_ompi_gather_process_cb: cannot allocate temporary memory of size %lu (buffer), aborting ...\n",
				(unsigned long) fsblksize * sizeof(char));
	_sion_opmicol_grc=SION_STD_NOT_SUCCESS;
      }

      mrank=_sion_map_rank_ompi_to_mpi(rank,sapi->num_threads);

      /* scan all other tasks */
      for(t=range_start;t<=range_end;t++) {

	mt=_sion_map_rank_ompi_to_mpi(t,sapi->num_threads);

	if(mt==mrank) {

	  /* thread is on same MPI rank */
	  tt=_sion_map_rank_ompi_to_thread_num(t,sapi->num_threads);

	  /* process data directly */
	  _sion_opmicol_grc=process_cb(outdatas[tt],specs[tt], sid);
	  
	} else {
	  
	  /* receive spec */
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector wait for spec from %d\n", t));
	  MPI_Recv(spec, spec_len, SION_MPI_INT64, mt, 1534, commgroup, &status);
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector got spec from %d (%lld,%lld)\n", 
		    t, (long long) spec[0], (long long) spec[1]));
      
	  /* send signal to send data */
	  if(spec[0]>=0) { 	/*  sender waits for data */
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector send signal to %d\n", t));
	    MPI_Send(&startsignal, 1, MPI_INT, mt, 1535, commgroup);
	  }
	  
	  /* get and send data */
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector start to proces data of size %lld at offset %lld\n", 
		    (long long) spec[1], (long long) spec[0]));

	  bytestosend=spec[1];
      
	  /* loop over data parts */
	  while(bytestosend>0) {
	    
	    if(bytestosend>fsblksize) datasize=fsblksize;
	    else                      datasize=bytestosend;
	    
	    spec[1]=datasize; 	/* adjust size */

	    /* process data with callback function */
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector process data block of size %lld at pos %lld\n", 
		      (long long) spec[1], (long long) spec[0]));
	
	    _sion_opmicol_grc=process_cb(buffer,spec, sid);
	    
	    if(_sion_opmicol_grc != SION_STD_SUCCESS) {
	      _sion_errorprint(SION_STD_NOT_SUCCESS,_SION_ERROR_RETURN,"_ompi_gather_process_cb: problems writing data ...\n");
	    }

	    /* send portion or all data */
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector send for data block to %d\n", mt));
	    MPI_Send(buffer, datasize, MPI_CHAR, mt, 1536, commgroup);
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "collector sent data block to %d datasize=%lld bytestorecv=%lld\n", 
		      mt, (long long) datasize, (long long) bytestosend));

	    /* advance counter */
	    bytestosend-=datasize;spec[0]+=datasize;
	   
	  } /* while */
	}   /* not on local MPI rank */
      }	    /* for all tasks */
      
      /* remove buffer */
      if (buffer) free(buffer);

    } else {
      /* its a sender */

      mcollector=_sion_map_rank_ompi_to_mpi(collector,sapi->num_threads);

      /* send data for all threads on MPI rank */
      for(tt=0;tt<sapi->num_threads;tt++) {

	/* send spec to collector */
	DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender send spec to %d (%lld,%lld)\n", 
		  mcollector,(long long) specs[tt][0], (long long) specs[tt][1]));
	MPI_Send(specs[tt], spec_len, SION_MPI_INT64, mcollector, 1534, commgroup);

	if(specs[tt][0]>0) { 		/* no error in sion_feof */
	
	  /* wait for start signal */
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender wait for signal from %d\n", collector));
	  MPI_Recv(&startsignal, 1, MPI_INT, mcollector, 1535, commgroup, &status);
	  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender got signal from %d\n", collector));
      
	  /* send data in chunks of fsblksize */
	  bytestorecv=specs[tt][1];
	  p=(char *) outdata;
	  while(bytestorecv>0) {
	    if(bytestorecv>fsblksize) datasize=fsblksize;
	    else                      datasize=bytestorecv;
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender recv data block from %d of size %lld\n", mcollector, (long long) datasize));
	    MPI_Recv(p, datasize, MPI_CHAR, mcollector, 1536, commgroup, &status);
	    MPI_Get_count(&status,MPI_CHAR,&count);
	
	    DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "sender recv data block from %d of size %lld (%d)\n", mcollector, (long long) datasize, count));
	    bytestorecv-=datasize;p+=datasize;
	  
	  } /* while */
	}  /* spec[0]>0 */
      }	/* for all threads */
    }	/* sender */
    
  } /* omp master */

  
  /* synchronize */
  {
#pragma omp barrier
  }
  rc=_sion_opmicol_grc;
  {
#pragma omp barrier
  }

  DPRINTFP((4, DFUNCTION, _SION_DEFAULT_RANK, "leave collector=%d rc=%d\n", collector, rc ));

  return rc;
}
#undef DFUNCTION


  /* share in_ptr given on master with all other threads, return value is the shared ptr */
#define DFUNCTION "__sion_ompi_share_ptr"
  void * __sion_ompicol_share_ptr(void * in_ptr) {
    void *out_ptr;
  
#pragma omp master
    __ompicol_global_pointer = in_ptr;

 
    {
#pragma omp barrier
    }

    out_ptr=__ompicol_global_pointer;

    return(out_ptr);

  }
#undef DFUNCTION


#define DFUNCTION "_sion_ompi_get_capability_cb"
int _sion_ompi_get_capability_cb(void *commdata )
{
  int       rc=SION_CAPABILITY_NONE;
  _ompi_api_commdata* sapi= (_ompi_api_commdata *) commdata;
  
  if(sapi->thread_num==0) {
    rc=SION_CAPABILITY_FULL;
    DPRINTFP((256, DFUNCTION, sapi->rank, "FULL capability\n"));
  } else {
    rc=SION_CAPABILITY_ONLY_SENDER;
    DPRINTFP((256, DFUNCTION, sapi->rank, "ONLY SENDER capability\n"));
  }
  return rc;
}
#undef DFUNCTION


  int _sion_ompicol_size_of_dtype(int dtype) {
    switch (dtype) {
    case _SION_INT32: return(sizeof(sion_int32));			  break;
    case _SION_INT64: return(sizeof(sion_int64));			  break;
    case _SION_CHAR:  return(sizeof(char));			  break;
    default:          return(sizeof(sion_int64));
    }
  }

  /* end of ifdef SION_OMPI */
#endif

