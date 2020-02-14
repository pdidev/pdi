/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

struct _sample_api_commdata_struct {
  MPI_Comm comm;
  int      commset;
  int      local;
  int      rank;
  int      size;
};
typedef struct _sample_api_commdata_struct _sample_api_commdata;


int _sample_create_lcg_cb(void **local_commgroup, void *global_commgroup, 
			  int grank, int gsize, 
			  int lrank, int lsize,
			  int filenumber, int numfiles
			  )
{
  int       rc;
  _sample_api_commdata* sapi_global = (_sample_api_commdata *) global_commgroup;
  _sample_api_commdata* commgroup=NULL;

  DPRINTFP((256, "_sample_create_lcg_cb", grank, " split now comm: grank=%d gsize=%d filenumber=%d, numfiles=%d, lrank=%d lsize=%d \n", 
	    grank, gsize, filenumber, numfiles, lrank, lsize));

  if(global_commgroup==NULL) {
    fprintf(stderr,"_sample_create_lcg_cb: error no global commgroup given, aborting  ...\n");
    return(-1);
  }
  if(*local_commgroup!=NULL) {
    fprintf(stderr,"_sample_create_lcg_cb: error local commgroup already initialized, aborting  ...\n");
    return(-1);
  }
  commgroup = (_sample_api_commdata *) malloc(sizeof(_sample_api_commdata));
  if (commgroup == NULL) {
    fprintf(stderr,"_sample_create_lcg_cb: cannot allocate memory for local commgroup of size %lu, aborting ...\n",(unsigned long) sizeof(_sample_api_commdata));
    return(-1);
  }
  *local_commgroup=commgroup;
  rc = MPI_Comm_split(sapi_global->comm, filenumber, lrank, &commgroup->comm);
  DPRINTFP((256, "_sample_create_lcg_cb", grank, " rc=%d from MPI_Comm_split\n",rc));
  commgroup->local=1;  commgroup->commset=1;
  commgroup->rank=lrank;
  commgroup->size=lsize;
  return rc;
}

int _sample_free_lcg_cb(void *local_commgroup) {
  int       rc = MPI_SUCCESS;
  _sample_api_commdata* commgroup = (_sample_api_commdata *) local_commgroup;
  
  if(commgroup->commset) {
    DPRINTFP((256, "_sample_free_lcg_cb", commgroup->rank, " free now comm\n"));
    rc=MPI_Comm_free(&commgroup->comm);
    DPRINTFP((256, "_sample_free_lcg_cb", commgroup->rank, " free now comm rc=%d\n",rc));
  }
  free(commgroup);
  return rc;
}
int _sample_barrier_cb(void *commdata)
{
  int       rc;
  _sample_api_commdata* sapi= (_sample_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  rc = MPI_Barrier(commp);
  return rc;
}

int _sample_bcastr_cb(void *data, void *commdata, int dtype, int nelem, int root)
{
  int       rc;
  _sample_api_commdata* sapi= (_sample_api_commdata *) commdata;
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

int _sample_gatherr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  int       rc;
  int       size, rank;
  _sample_api_commdata* sapi= (_sample_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;

  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, "_sample_gatherr_cb", rank, " gatherr on %d of %d nelem=%d root=%d\n", rank, size, nelem, root));

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

int _sample_scatterr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root)
{
  int       rc, t;
  _sample_api_commdata* sapi= (_sample_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  int rank=sapi->rank; 
  int size=sapi->size; 

  DPRINTFP((256, "_sample_scatterr_cb", rank, " starting nelem=%d root=%d\n", nelem, root));

  if(rank==root) {
    for(t=0;t<nelem*size;t++) {
      DPRINTFP((256, "_sample_scatterr_cb", rank, " before scatter: %d -> %d\n", t,(int) ((int *) indata)[t]));
    }
  }

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

int _sample_gathervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  int       rc;
  int       size, rank, t, offset;
  int       *displs=NULL;
  _sample_api_commdata* sapi= (_sample_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  

  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, "_sample_gathervr_cb", rank, " input nelem=%d root=%d indata=%x, outdata=%x\n", nelem, root, indata, outdata));

  /* collect nelems */
  if(rank==root) {
    displs = (int *) malloc(size * sizeof(int));
    if (displs == NULL) {
      fprintf(stderr,"_sample_gathervr_cb: cannot allocate temporary memory of size %lu (displs), aborting ...\n",(unsigned long) size * sizeof(int));
      return(-1);
    }
    offset=0;
    for(t=0;t<size;t++) {
      displs[t]=offset;
      offset+=counts[t];
      DPRINTFP((256, "_sample_gathervr_cb", rank, " after MPI_Gather %2d -> dpls=%2d count=%d\n", t,displs[t],counts[t]));
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

int _sample_scattervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root)
{
  int       rc;
  int       size, rank, t, offset;
  int       *displs=NULL;
  _sample_api_commdata* sapi= (_sample_api_commdata *) commdata;
  MPI_Comm commp = sapi->comm;
  

  MPI_Comm_rank(commp, &rank);
  MPI_Comm_size(commp, &size);

  DPRINTFP((256, "_sample_scattervr_cb", rank, " input nelem=%d root=%d\n", nelem, root));

  /* collect nelems */
  if(rank==root) {
    displs = (int *) malloc(size * sizeof(int));
    if (displs == NULL) {
      fprintf(stderr,"_sample_scattervr_cb: cannot allocate temporary memory of size %lu (displs), aborting ...\n",(unsigned long) size * sizeof(int));
      return(-1);
    }
    offset=0;
    for(t=0;t<size;t++) {
      displs[t]=offset;
      offset+=counts[t];
      DPRINTFP((256, "_sample_scattervr_cb", rank, " after MPI_Gather %2d -> dpls=%2d sendcounts=%d\n", t,displs[t],counts[t]));
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
