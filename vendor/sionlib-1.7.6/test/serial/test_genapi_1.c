/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>

#include "sion.h"

/* Simple test if genapi work in a serial context */

struct _sample_api_commdata_struct {
  int      commset;
  int      local;
  int      rank;
  int      size;
};
typedef struct _sample_api_commdata_struct _sample_api_commdata;

int __sample_check_comm(void *commdata) {
  int       rc=SION_SUCCESS;

  _sample_api_commdata* sapi= (_sample_api_commdata *) commdata;

  int rank=sapi->rank; 
  int size=sapi->size; 

  DPRINTFP((256, "__sample_check_comm", rank, " check comm on %d of %d\n", rank, size));

  if(rank!=0) {
    fprintf(stderr,"_sample_create_lcg_cb: error global commgroup rank !=0 (%d)  ...\n",rank);
    return(!SION_SUCCESS);
  }

  if(size!=1) {
    fprintf(stderr,"_sample_create_lcg_cb: error global commgroup size > 1 (%d)  ...\n",size);
    return(!SION_SUCCESS);
  }
  return(rc);
 
}


int _sample_create_lcg_cb(void **local_commgroup, void *global_commgroup, 
			  int grank, int gsize, 
			  int lrank, int lsize,
			  int filenumber, int numfiles
			  )
{
  int       rc=SION_SUCCESS;
  _sample_api_commdata* commgroup=NULL;

  DPRINTFP((256, "_sample_create_lcg_cb", grank, " split now comm: grank=%d gsize=%d filenumber=%d, numfiles=%d, lrank=%d lsize=%d sapi_local->commset=%d\n", 
	    grank, gsize, filenumber, numfiles, lrank, lsize, sapi_local->commset));

  if(global_commgroup==NULL) {
    fprintf(stderr,"_sample_create_lcg_cb: error no global commgroup given, aborting  ...\n");
    return(!SION_SUCCESS);
  }
  if(*local_commgroup!=NULL) {
    fprintf(stderr,"_sample_create_lcg_cb: error local commgroup already initialized, aborting  ...\n");
    return(!SION_SUCCESS);
  }

  if(grank!=0) {
    fprintf(stderr,"_sample_create_lcg_cb: error global commgroup rank !=0 (%d)  ...\n",grank);
    return(!SION_SUCCESS);
  }

  if(gsize!=1) {
    fprintf(stderr,"_sample_create_lcg_cb: error global commgroup size > 1 (%d)  ...\n",gsize);
    return(!SION_SUCCESS);
  }

  if(lrank!=0) {
    fprintf(stderr,"_sample_create_lcg_cb: error local commgroup rank !=0 (%d)  ...\n",lrank);
    return(!SION_SUCCESS);
  }

  if(lsize!=1) {
    fprintf(stderr,"_sample_create_lcg_cb: error local commgroup size > 1 (%d)  ...\n",lsize);
    return(!SION_SUCCESS);
  }

  commgroup = (_sample_api_commdata *) malloc(sizeof(_sample_api_commdata));
  if (commgroup == NULL) {
    fprintf(stderr,"_sample_create_lcg_cb: cannot allocate memory for local commgroup of size %lu, aborting ...\n",(unsigned long) sizeof(_sample_api_commdata));
    return(!SION_SUCCESS);
  }
  *local_commgroup=commgroup;
  commgroup->rank=lrank;
  commgroup->size=lsize;
  commgroup->local=1;  commgroup->commset=1;
 
  /* do nothing, there is only one task  */
  DPRINTFP((256, "_sample_create_lcg_cb", grank, " return with rc=%d \n",rc));
  return rc;
}

int _sample_free_lcg_cb(void *local_commgroup) {
  int       rc=SION_SUCCESS;
  _sample_api_commdata* commgroup = (_sample_api_commdata *) local_commgroup;
  
  if(commgroup->commset) {
    DPRINTFP((256, "_sample_free_lcg_cb", commgroup->rank, " free now comm rc=%d\n",rc));
  }
  free(commgroup);
  return rc;
}
int _sample_barrier_cb(void *commdata)
{
  int       rc=SION_SUCCESS;

  if(__sample_check_comm(commdata)!=SION_SUCCESS) return(!SION_SUCCESS);

  DPRINTFP((256, "_sample_barrier_cb", commgroup->rank, "barrier call (dummy)\n"));
  return rc;
}

int _sample_bcastr_cb(void *data, void *commdata, int dtype, int nelem, int root)
{
  int       rc=SION_SUCCESS;

  if(__sample_check_comm(commdata)!=SION_SUCCESS) return(!SION_SUCCESS);

  DPRINTFP((256, "_sample_bcastr_cb", commgroup->rank, "bcast call (dummy)\n"));

  return rc;
}


int _sample_gatherr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  int       rc=SION_SUCCESS;
  void*     ret;
  
  DPRINTFP((256, "_sample_gatherr_cb", rank, " gatherr nelem=%d root=%d\n", nelem, root));

  if(__sample_check_comm(commdata)!=SION_SUCCESS) return(!SION_SUCCESS);

  switch (dtype) {
  case _SION_INT32:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int32) );
    break;
  case _SION_INT64:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  case _SION_CHAR:
    ret=memcpy(outdata,indata, nelem*sizeof(char) );
    break;
  default:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  }

  if(ret!=outdata) rc=!SION_SUCCESS; 
  
  return rc;
}

int _sample_scatterr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  void*     ret;

  if(__sample_check_comm(commdata)!=SION_SUCCESS) return(!SION_SUCCESS);

  DPRINTFP((256, "_sample_scatterr_cb", -1, " starting nelem=%d root=%d\n", nelem, root));


  switch (dtype) {
  case _SION_INT32:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int32) );
    break;
  case _SION_INT64:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  case _SION_CHAR:
    ret=memcpy(outdata,indata, nelem*sizeof(char) );
    break;
  default:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  }

  return (ret == outdata) ? SION_SUCCESS : !SION_SUCCESS;
}

int _sample_gathervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  void*     ret;
  
  if(__sample_check_comm(commdata)!=SION_SUCCESS) return(!SION_SUCCESS);

  DPRINTFP((256, "_sample_gathervr_cb", -1, " input nelem=%d root=%d indata=%x, outdata=%x\n", nelem, root, indata, outdata));

  switch (dtype) {
  case _SION_INT32:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int32) );
    break;
  case _SION_INT64:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  case _SION_CHAR:
    ret=memcpy(outdata,indata, nelem*sizeof(char) );
    break;
  default:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  }

  return (ret == outdata) ? SION_SUCCESS : !SION_SUCCESS;
}

int _sample_scattervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  void*     ret;
  
  if(__sample_check_comm(commdata)!=SION_SUCCESS) return(!SION_SUCCESS);


  DPRINTFP((256, "_sample_gathervr_cb", -1, " input nelem=%d root=%d indata=%x, outdata=%x\n", nelem, root, indata, outdata));

  switch (dtype) {
  case _SION_INT32:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int32) );
    break;
  case _SION_INT64:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  case _SION_CHAR:
    ret=memcpy(outdata,indata, nelem*sizeof(char) );
    break;
  default:
    ret=memcpy(outdata,indata, nelem*sizeof(sion_int64) );
    break;
  }

  return (ret == outdata) ? SION_SUCCESS : !SION_SUCCESS;
}


int main(int argc, char **argv)
{
  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  int aid,rank;

  DPRINTFP((1, "test_genapi_1", rank, "start API generation on rank %d\n",rank));

  /* -------------------------- */
  /* TEST 0: define new API     */
  /* -------------------------- */
  {
    rank=0;

    aid=sion_generic_create_api("SampleSerialAPI");
    printf("on rank %d: new API: aid=%d\n",rank,aid);  

    sion_generic_register_create_local_commgroup_cb(aid,&_sample_create_lcg_cb);
    sion_generic_register_free_local_commgroup_cb(aid,&_sample_free_lcg_cb);

    sion_generic_register_barrier_cb(aid,&_sample_barrier_cb);
    sion_generic_register_bcastr_cb(aid,&_sample_bcastr_cb);
    sion_generic_register_gatherr_cb(aid,&_sample_gatherr_cb);
    sion_generic_register_scatterr_cb(aid,&_sample_scatterr_cb);
    sion_generic_register_gathervr_cb(aid,&_sample_gathervr_cb);
    sion_generic_register_scattervr_cb(aid,&_sample_scattervr_cb);

    printf("on rank %d: END of TEST 0\n",rank);

    sion_generic_free_api(aid);

  }

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  
  return(0);
  
}
