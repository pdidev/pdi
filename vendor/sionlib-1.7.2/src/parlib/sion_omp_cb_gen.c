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
 * \brief OpenMP callbacks to generic interface
 * 
 * \author Wolfgang Frings
 *         David Montoya
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

#include "sion_omp_cb_gen.h"

#ifdef SION_OMP

#include "omp.h"

static void *__omp_global_pointer;

int _sion_omp_size_of_dtype(int dtype);

int __sion_omp_get_info_from_other(void *data, sion_int64 *spec, int spec_len, 
				   void *commdata,  int collector, int range_start, int range_end,
				   sion_int64 ***p_spec, char ***p_indata  );

int _sion_register_callbacks_omp(void) {
  int aid=0;
  aid=sion_generic_create_api("SIONlib_OMP_API");
  
 
  sion_generic_register_create_local_commgroup_cb(aid,&_sion_omp_create_lcg_cb);
  sion_generic_register_free_local_commgroup_cb(aid,&_sion_omp_free_lcg_cb);

  sion_generic_register_barrier_cb(aid,&_sion_omp_barrier_cb);
  sion_generic_register_bcastr_cb(aid,&_sion_omp_bcastr_cb);
  sion_generic_register_gatherr_cb(aid,&_sion_omp_gatherr_cb);
  sion_generic_register_scatterr_cb(aid,&_sion_omp_scatterr_cb);
  sion_generic_register_gathervr_cb(aid,&_sion_omp_gathervr_cb);
  sion_generic_register_scattervr_cb(aid,&_sion_omp_scattervr_cb);
  sion_generic_register_gather_and_execute_cb(aid,&_sion_omp_gather_process_cb);
  sion_generic_register_execute_and_scatter_cb(aid,&_sion_omp_process_scatter_cb);
  sion_generic_register_get_capability_cb(aid,&_sion_omp_get_capability_cb);

  return(aid);
} 

int _sion_omp_create_lcg_cb(void **local_commgroup, void *global_commgroup, 
			    int grank, int gsize, 
			    int lrank, int lsize,
			    int filenumber, int numfiles
			  )
{
  int       rc=SION_STD_SUCCESS;
  
  DPRINTFP((256, "_sion_omp_create_lcg_cb", grank, " DUMMY for split global comm: grank=%d gsize=%d filenumber=%d, numfiles=%d, lrank=%d lsize=%d \n", 
	    grank, gsize, filenumber, numfiles, lrank, lsize));
  *local_commgroup=global_commgroup;

  return rc;
}

int _sion_omp_free_lcg_cb(void *local_commgroup) {
  int       rc=SION_STD_SUCCESS;
  DPRINTFP((256, "_omp_free_lcg_cb", _SION_DEFAULT_RANK, " DUMMY for free local comm\n"));
  return rc;
}


int _sion_omp_barrier_cb(void *commdata)
{
  int       rc=SION_STD_SUCCESS;
  {
#pragma omp barrier
  }
  return rc;
}

int _sion_omp_bcastr_cb(void *data, void *commdata, int dtype, int nelem, int root)
{
  int       rc=SION_STD_SUCCESS;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;
  
  

  if(sapi->thread_num==root) {
    __omp_global_pointer = data;
  }
  
  /* threads sync global pointer */
  
  {
#pragma omp barrier
  }
  
  if((sapi->thread_num!=root) && (__omp_global_pointer != NULL)) {
    memcpy(data,__omp_global_pointer,nelem*_sion_omp_size_of_dtype(dtype));
  }
  {
#pragma omp barrier
  }
  
  return rc;
}


/* indata: data that comes into fuinction, outdata: data that will be created by function */
int _sion_omp_gatherr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{

  int       rc=SION_STD_SUCCESS;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;
  
  /* set global pointer to outdata */
  if(sapi->thread_num==root) {
    __omp_global_pointer = outdata;
  }

  /* synchronize */
  {
#pragma omp barrier
  }


  /* copy indata -> outdata */
  memcpy((char *)__omp_global_pointer+sapi->thread_num*nelem*_sion_omp_size_of_dtype(dtype),
	 indata,
	 nelem*_sion_omp_size_of_dtype(dtype)
	 );

  /* synchronize again */
  {
#pragma omp barrier
  }
  return rc;
}

int _sion_omp_scatterr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{

  int       rc=SION_STD_SUCCESS;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;
  
  /* set global pointer to indata */
  if(sapi->thread_num==root) {
    __omp_global_pointer = indata;
  }


  /* synchronize */
  {
#pragma omp barrier
  }
  
  /* copy outdata <- indata */
  memcpy( outdata,
	  (char *)__omp_global_pointer+sapi->thread_num*nelem*_sion_omp_size_of_dtype(dtype),
	  nelem*_sion_omp_size_of_dtype(dtype)
	  );
  
  /* synchronize again */
  {
#pragma omp barrier
  }

  return rc;
}


int _sion_omp_gathervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{

  int       rc=SION_STD_SUCCESS;
  int       *displs=NULL;
  int       t, offset;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;

  /* compute offsets sizes for each thread and store these values in a temporary vector */
  if(sapi->thread_num==root) {

    displs = (int *) malloc(sapi->num_threads * sizeof(int));
    if (displs == NULL) {
      fprintf(stderr,"__sion_omp_gathervr_cb: cannot allocate temporary memory of size %zu (displs), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int));
      return(-1);
    }
    offset=0;
    for(t=0;t<sapi->num_threads;t++) {
      displs[t]=offset;
      offset+=counts[t];
      DPRINTFP((256, "_sion_omp_gathervr_cb", sapi->thread_num, " after compute %2d -> dpls=%2d count=%d\n", t,displs[t],counts[t]));
    }
    
    /* set global pointer to temporary vector */
    __omp_global_pointer = displs;
  }

  /* synchronize */
  {
#pragma omp barrier
  }

  /* get offset */
  offset= ((int*)__omp_global_pointer)[sapi->thread_num];  

  /* synchronize again */
  {
#pragma omp barrier
  }

  /* free temporary vector and set global pointer to outdata */
  if(sapi->thread_num==root) {
    if(displs) free(displs);

  }

  /* set global pointer to temporary vector */
  __omp_global_pointer = outdata;
  

  /* synchronize again */
  {
#pragma omp barrier
  }

  /* copy indata -> outdata */
  memcpy(( char * ) __omp_global_pointer + offset * _sion_omp_size_of_dtype(dtype),
	 indata,
	 nelem*_sion_omp_size_of_dtype(dtype)
	 );

  /* synchronize again */
  {
#pragma omp barrier
  }
  return rc;
}


int _sion_omp_scattervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{

  int       rc=SION_STD_SUCCESS;
  int       *displs=NULL;
  int       t, offset;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;

  /* compute offsets for each thread and store these values in a temporary vector */
  if(sapi->thread_num==root) {

    displs = (int *) malloc(sapi->num_threads * sizeof(int));
    if (displs == NULL) {
      fprintf(stderr,"__sion_omp_gathervr_cb: cannot allocate temporary memory of size %zu (displs), aborting ...\n",
	      (size_t) sapi->num_threads * sizeof(int));
      return(-1);
    }
    offset=0;
    for(t=0;t<sapi->num_threads;t++) {
      displs[t]=offset;
      offset+=counts[t];
      DPRINTFP((256, "_sion_omp_gathervr_cb", sapi->thread_num, " after compute %2d -> dpls=%2d count=%d\n", t,displs[t],counts[t]));
    }
    
    /* set global pointer to temporary vector */
    __omp_global_pointer = displs;
  }

  /* synchronize */
  {
#pragma omp barrier
  }

  /* get offset */
  offset= ((int*)__omp_global_pointer)[sapi->thread_num];  

  /* synchronize again */
  {
#pragma omp barrier
  }

  /* free temporary vector and set global pointer to outdata */
  if(sapi->thread_num==root) {
    if(displs) free(displs);

  }

  /* set global pointer to temporary vector */
  __omp_global_pointer = indata;
  

  /* synchronize again */
  {
#pragma omp barrier
  }

  /* copy indata -> outdata */
  memcpy(outdata,
	 ( char * ) __omp_global_pointer + offset * _sion_omp_size_of_dtype(dtype),
	 nelem*_sion_omp_size_of_dtype(dtype)
	 );

  /* synchronize again */
  {
#pragma omp barrier
  }
  return rc;
}


int _sion_omp_gather_process_cb(const void *indata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				void *commdata,  int collector, int range_start, int range_end, int sid, 
				int process_cb(const void *,sion_int64 *, int ) ) {
  int       rc=SION_STD_SUCCESS;
  
  sion_int64 **p_spec=NULL;
  char       **p_indata=NULL;
  int       t, trank;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;


  __sion_omp_get_info_from_other((void*) indata, spec, spec_len, 
				 commdata,  collector, range_start, range_end,
				 &p_spec, &p_indata  );
  

  /* process */
  if(sapi->thread_num==collector) {

    /* scan all other tasks */
    for(t=range_start;t<=range_end;t++) {
      trank=t-range_start;
      DPRINTFP((256, "_sion_omp_gather_process_cb", -1, "on master t=%d spec=%ld,%ld,%ld \n",trank,
		*(p_spec[trank]+0), *(p_spec[trank]+1), *(p_spec[trank]+2)));

      rc=process_cb(p_indata[trank],p_spec[trank], sid);
    }

    if(rc != SION_STD_SUCCESS) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_omp_gather_process_cb: problems writing data ...\n"));
    }

    if(p_spec) free(p_spec);
    if(p_indata) free(p_indata);

  }  

  /* synchronize */
  {
#pragma omp barrier
  }
  

  return(rc);
}

int _sion_omp_process_scatter_cb(void *outdata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				 void *commdata,  int collector, int range_start, int range_end, int sid, 
				 int process_cb(void *,sion_int64 *, int ) ) {
  int       rc=SION_STD_SUCCESS;
  
  sion_int64 **p_spec=NULL;
  char       **p_indata=NULL;
  int       t, trank;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;


  __sion_omp_get_info_from_other((void*) outdata, spec, spec_len, 
				 commdata,  collector, range_start, range_end,
				 &p_spec, &p_indata  );
  

  /* process */
  if(sapi->thread_num==collector) {

    /* scan all other tasks */
    for(t=range_start;t<=range_end;t++) {
      trank=t-range_start;
      DPRINTFP((256, "_sion_omp_gather_process_cb", -1, "on master t=%d spec=%ld,%ld,%ld \n",trank,
		*(p_spec[trank]+0), *(p_spec[trank]+1), *(p_spec[trank]+2)));

      rc=process_cb(p_indata[trank],p_spec[trank], sid);
    }

    if(rc != SION_STD_SUCCESS) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_omp_gather_process_cb: problems writing data ...\n"));
    }

    if(p_spec) free(p_spec);
    if(p_indata) free(p_indata);

  }  

  /* synchronize */
  {
#pragma omp barrier
  }



  return(rc);
}

int __sion_omp_get_info_from_other(void *data, sion_int64 *spec, int spec_len, 
				   void *commdata,  int collector, int range_start, int range_end,
				   sion_int64 ***p_spec, char ***p_indata  ) {
  int       rc=SION_SUCCESS;

  sion_int64 **tspec;
  char       const **tdata;
  int       num_sender, my_trank;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;

  num_sender = range_end-range_start+1;
  my_trank   = sapi->thread_num-range_start;

  /* collect offsets and sizes for each thread */
  if(sapi->thread_num==collector) {

    *p_spec   = (sion_int64 **) malloc(num_sender * sizeof(sion_int64 *));
    if (*p_spec == NULL) {
      fprintf(stderr,"_sion_omp_gather_process_cb: cannot allocate temporary memory of size %zu (p_spec), aborting ...\n",
	      (size_t) num_sender * sizeof(sion_int64 *));
      return(-1);
    }
    *p_indata = (char **) malloc(num_sender * sizeof(char *));
    if (*p_indata == NULL) {
      fprintf(stderr,"_sion_omp_gather_process_cb: cannot allocate temporary memory of size %zu (p_indata), aborting ...\n",
	      (size_t) num_sender * sizeof(char *));
      return(-1);
    }
    
    /* set global pointer to temporary vector */
    __omp_global_pointer = *p_spec;
  }

  /* synchronize */
  {
#pragma omp barrier
  }
  
  if(sapi->thread_num!=collector) {
    tspec  = (sion_int64 **) __omp_global_pointer;
    tspec[my_trank] = spec;
    DPRINTFP((256, "_sion_omp_gather_process_cb", -1, "on sender t=%d spec=%ld,%ld,%ld \n",my_trank,
	      *(tspec[my_trank]+0), *(tspec[my_trank]+1), *(tspec[my_trank]+2)));
    
  }

  /* synchronize */
  {
#pragma omp barrier
  }
  
  if(sapi->thread_num==collector) {
    /* set global pointer to temporary vector */
    __omp_global_pointer = *p_indata;
  } 

  /* synchronize */
  {
#pragma omp barrier
  }

  if(sapi->thread_num!=collector) {
    tdata  = (char const **) __omp_global_pointer;
    tdata[my_trank] = data; 
  }

  /* synchronize */
  {
#pragma omp barrier
  }


  return(rc);
}

int _sion_omp_get_capability_cb(void *commdata )
{
  int       rc=SION_CAPABILITY_NONE;
  _omp_api_commdata* sapi= (_omp_api_commdata *) commdata;
  
  if(sapi->thread_num==0) {
    rc=SION_CAPABILITY_FULL;
    DPRINTFP((256, "_sion_omp_get_capability_cb", sapi->thread_num, "FULL capability\n"));
  } else {
    rc=SION_CAPABILITY_ONLY_SENDER;
    DPRINTFP((256, "_sion_omp_get_capability_cb", sapi->thread_num, "ONLY SENDER capability\n"));
  }
  return rc;
}


int _sion_omp_size_of_dtype(int dtype) {
  switch (dtype) {
  case _SION_INT32: return(sizeof(sion_int32));			  break;
  case _SION_INT64: return(sizeof(sion_int64));			  break;
  case _SION_CHAR:  return(sizeof(char));			  break;
  default:          return(sizeof(sion_int64));
  }
}

/* end of ifdef OPENMP */
#endif
