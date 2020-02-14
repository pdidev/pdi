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
 * Generic API \ref generic_api_page
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
#include "sion_flags.h"

#include "sion_generic_internal.h"
#include "sion_generic_apidesc.h"
#include "sion_generic_mapped.h"
#include "sion_generic_buddy.h"
#include "sion_generic.h"


int sion_generic_create_api( char *name ) {
  int apiid = SION_ID_UNDEF;
  _sion_generic_apidesc *sion_apidesc;

  sion_apidesc=_sion_generic_alloc_apidesc();
  if(!sion_apidesc) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_create_api: problems to alloc apidesc, aborting ...\n"));
  }
  _sion_generic_init_apidesc(sion_apidesc);
  
  if (name == NULL) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_create_api: name for API not given, aborting ...\n"));
  }
  
  sion_apidesc->name=strdup(name);

  apiid = _sion_newvcd(sion_apidesc,SION_APIDESCRIPTOR);
  sion_apidesc->aid=apiid;
  sion_apidesc->level=SION_GENERIC_API_LEVEL_NONE;

  DPRINTFP((2, "_sion_generic_create_api", _SION_DEFAULT_RANK, "API created with name %s apiid=%d\n", sion_apidesc->name, apiid));
  
  return(apiid);
}

int sion_generic_free_api( int aid ) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;

  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_free_api: invalid sion_apidesc %d", aid));
  }
  _sion_freevcd(aid);
  rc=_sion_generic_free_apidesc(sion_apidesc);

  return(rc);
}

int sion_generic_register_create_local_commgroup_cb(int aid, int create_lcg_cb(void **, void *, int, int, int, int, int, int)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_create_local_commgroup_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->create_lcg_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_create_local_commgroup_cb: update_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->create_lcg_cb=create_lcg_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_create_local_commgroup_cb", _SION_DEFAULT_RANK, "create_lcg_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_free_local_commgroup_cb(int aid, int free_lcg_cb(void *)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_free_local_commgroup_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->free_lcg_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_free_local_commgroup_cb: update_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->free_lcg_cb=free_lcg_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_free_local_commgroup_cb", _SION_DEFAULT_RANK, "free_lcg_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}


int sion_generic_register_barrier_cb(int aid, int barrier_cb(void *)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_barrier_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->barrier_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_barrier_cb: barrier_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->barrier_cb=barrier_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_barrier_cb", _SION_DEFAULT_RANK, "barrier_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_bcastr_cb(int aid, int bcastr_cb(void *,void *, int,int,int)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_bcastr_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->bcastr_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_bcastr_cb: bcastr_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->bcastr_cb=bcastr_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_bcastr_cb", _SION_DEFAULT_RANK, "bcastr_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_gatherr_cb(int aid, int gatherr_cb(void *,void *,void *, int,int,int)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_gatherr_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->gatherr_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_gatherr_cb: gatherr_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->gatherr_cb=gatherr_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_gatherr_cb", _SION_DEFAULT_RANK, "gatherr_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_gathervr_cb(int aid, int gathervr_cb(void *,void *,void *, int, int *, int, int)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_gathervr_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->gathervr_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_gathervr_cb: gathervr_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->gathervr_cb=gathervr_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_gathervr_cb", _SION_DEFAULT_RANK, "gathervr_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_scatterr_cb(int aid, int scatterr_cb(void *,void *,void *, int,int,int)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_scatterr_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->scatterr_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_scatterr_cb: scatterr_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->scatterr_cb=scatterr_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_scatterr_cb", _SION_DEFAULT_RANK, "scatterr_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_scattervr_cb(int aid, int scattervr_cb(void *,void *,void *, int, int *, int, int)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_scattervr_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->scattervr_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_scattervr_cb: scattervr_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->scattervr_cb=scattervr_cb;
 
  _sion_generic_update_api_level(sion_apidesc);

  DPRINTFP((2, "sion_generic_register_scattervr_cb", _SION_DEFAULT_RANK, "scattervr_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_gather_and_execute_cb(int aid, int gather_execute_cb(const void *, sion_int64*, int, sion_int64, 
									       void *, int, int, int, int,
									       int process_cb(const void *,sion_int64 *, int))) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_gather_and_execute_cb: invalid sion_apidesc %d", aid));
  }
  
  if(sion_apidesc->gather_execute_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_gather_and_execute_cb: scattervr_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->gather_execute_cb=gather_execute_cb;
 
  _sion_generic_update_api_level(sion_apidesc);

  DPRINTFP((2, "sion_generic_register_gather_and_execute_cb", _SION_DEFAULT_RANK, "gather_execute_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_execute_and_scatter_cb(int aid, int execute_scatter_cb(void *, sion_int64*, int, sion_int64, 
									         void *, int, int, int, int,
									         int process_cb(void *,sion_int64 *, int))) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_execute_and_scatter_cb: invalid sion_apidesc %d", aid));
  }
  
  if(sion_apidesc->execute_scatter_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_execute_and_scatter_cb: scattervr_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->execute_scatter_cb=execute_scatter_cb;
 
  DPRINTFP((2, "sion_generic_register_execute_and_scatter_cb", _SION_DEFAULT_RANK, "gather_execute_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_get_multi_filename_cb(int aid, char *get_multi_filename_cb(const char *,int)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc=0;

  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(-1,_SION_ERROR_RETURN,"sion_generic_register_get_multi_filename_cb: invalid sion_apidesc %d", aid));
  }

  if(sion_apidesc->get_multi_filename_cb!=NULL) {
    return(_sion_errorprint(-1,_SION_ERROR_RETURN,"sion_generic_register_get_multi_filename_cb: get_multi_filename_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->get_multi_filename_cb=get_multi_filename_cb;

  _sion_generic_update_api_level(sion_apidesc);

  DPRINTFP((2, "sion_generic_register_get_multi_filename_cb", -1, "get_multi_filename_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}

int sion_generic_register_get_capability_cb(int aid, int get_capability_cb(void *)) {
  _sion_generic_apidesc *sion_apidesc;
  int rc = SION_SUCCESS;
  
  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_get_capability_cb: invalid sion_apidesc %d", aid));
  }
  
  if(sion_apidesc->get_capability_cb!=NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_generic_register_get_capability_cb: get_capability_cb already registered for apiid=%d", aid));
  }
  sion_apidesc->get_capability_cb=get_capability_cb;

  _sion_generic_update_api_level(sion_apidesc);
 
  DPRINTFP((2, "sion_generic_register_get_capability_cb", _SION_DEFAULT_RANK, "get_capability_cb registered for apiid=%s (%d)\n", sion_apidesc->name, aid));

  return(rc);
}


/*!
 * @brief Open a sion file a generic interface.
 *
 * For a description please see the \ref sion_paropen_mpi_description
 * "description" of sion_paropen_mpi.
 *
 * @param[in]       aid          sion API id
 * @param[in]       fname        name of file, should be equal on all tasks
 * @param[in]       file_mode    like the type parameter of fopen (currently recognized options: "rb", "wb")
 * @param[in,out]   chunksize    maximum size to be written with single write call
 * @param[in,out]   fsblksize    file system block size (-1 for automatic)
 * @param[in]       gcommgroup   global communicator
 * @param[in]       grank        global rank of process
 * @param[in]       gsize        size of global communicator
 * @param[in,out]   filenumber   file number (for multiple file mode)
 * @param[in,out]   numfiles     number of files to use (-1 for automatic choosing from local communicator)
 * @param[in]       lrank        local rank of process
 * @param[in]       lsize        size of local communicator
 * @param[in,out]   fileptr      file pointer (NULL for not using an external file pointer)
 * @param[out]      newfname     return value for actual file name if using multiple files
 *
 * @retval          sid          sion file handle or -1 if error occured
 */
int sion_generic_paropen(int         aid,
                         const char* fname,
                         const char* file_mode,
                         sion_int64* chunksize,
                         sion_int32* fsblksize,
                         void*       gcommgroup,
                         int         grank,
                         int         gsize,
                         int*        filenumber,
                         int*        numfiles,
                         const int*  lrank,
                         const int*  lsize,
                         FILE**      fileptr,
                         char**      newfname
                         )
{
  int        rc, sid = SION_ID_UNDEF;
  sion_int32 lfsblksize;
  sion_int64 lchunksize;
  int        help_globalrank;  
  char       *nfname=NULL,*lprefix=NULL;
  _sion_filedesc        *sion_filedesc;
  _sion_generic_apidesc *sion_apidesc;
  _sion_generic_gendata *sion_gendata;
  _sion_flags_store* flags_store = NULL;

  /*                                                                      */ DPRINTFTS(grank, "enter sion_generic_paropen");
  DPRINTFP((1, "sion_generic_paropen", grank, "enter generic parallel open of FILE %s (global: %d of %d, local: %d of %d, file: %d of %d )\n", 
	    fname, grank, gsize, *lrank, *lsize, *filenumber, *numfiles));

  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_register_scattervr_cb: invalid sion_apidesc %d", aid));
  }

  if ( (sion_apidesc->level!=SION_GENERIC_API_LEVEL_STD) && 
       (sion_apidesc->level!=SION_GENERIC_API_LEVEL_FULL) ) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: API %s not correctly initalized, aborting",sion_apidesc->name));
  }

  /* check parameters */
  if ( (grank < 0 ) || (grank >= gsize) ) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: global rank %d not valid (0..%d)",grank,gsize));
  }

#ifdef HACK_SCALASCA  
  /* hack for scalasca */
  if(strstr(fname,".def")!=NULL) {
    lfsblksize=65536;
    lchunksize=65536;
  } else {
#endif
    lfsblksize=*fsblksize;
    lchunksize=*chunksize;
#ifdef HACK_SCALASCA  
  }
#endif
  DPRINTFP((1, "sion_generic_paropen", grank, "setting fsblksize = %d for file %s\n", lfsblksize, fname));

  /* create data container */
  sion_gendata=_sion_generic_alloc_gendata();
  if(!sion_gendata) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: problems to alloc gendata, aborting ...\n"));
  }
  _sion_generic_init_gendata(sion_gendata);

  sion_gendata->aid=aid;
  sion_gendata->apidesc=sion_apidesc;
  sion_gendata->comm_data_global=gcommgroup;
  sion_gendata->comm_data_local=NULL;
  sion_gendata->grank=grank;
  sion_gendata->gsize=gsize;

  flags_store = _sion_parse_flags(file_mode);
  if ( ! flags_store ) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: could not parse file mode in %s, aborting ...\n", file_mode));
  }

  if (flags_store->mask&_SION_FMODE_WRITE) {


    if (lrank == NULL) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: No lrank variable given"));
    }
    if (lsize == NULL) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: No lsize variable given"));
    }
    if ( !_sion_flags_get(flags_store, "collmsa") && ((*lrank < 0 ) || (*lrank >= *lsize)) ) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: local rank %d not valid (0..%d)",*lrank,*lsize));
    }
    sion_gendata->lrank=*lrank;
    sion_gendata->lsize=*lsize;
    
    if (filenumber == NULL) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: No filenumber variable given"));
    }
    if (numfiles == NULL) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: No numfiles variable given"));
    }
    if ( !_sion_flags_get(flags_store, "collmsa") && ((*filenumber < 0 ) || (*filenumber >= *numfiles)) ) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: file number %d not valid (0..%d)",*filenumber,*numfiles));
    }
    sion_gendata->filenumber=*filenumber;
    sion_gendata->numfiles=*numfiles;
    if (_sion_flags_get(flags_store, "collmsa")) {
      if (SION_SUCCESS != _sion_generic_renumber_collmsa(sion_gendata, flags_store)) {
        return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: error in MSA aware collective renumbering, aborting ...\n"));
      }
    }

    lprefix = calloc(SION_FILENAME_LENGTH,1);
    if (lprefix == NULL) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: cannot allocate temporary memory of size %lu (lprefix), aborting ...\n", (unsigned long) SION_FILENAME_LENGTH));
    }

    strcpy(lprefix, fname);

  } else if (flags_store->mask&_SION_FMODE_READ) {
    /* file mode READ */

    DPRINTFP((1, "sion_generic_paropen", grank, "READ: buddy=%d\n", flags_store->mask&_SION_FMODE_BUDDY));

    /* get number of files and filenumber */
    if(! (flags_store->mask&_SION_FMODE_BUDDY) ) { 

      rc = _sion_generic_get_and_distribute_info_from_file(sion_gendata, fname);
      if (_sion_flags_get(flags_store, "collmsa") && (sion_gendata->numfiles == 1)) {
        if (SION_SUCCESS != _sion_generic_renumber_collmsa(sion_gendata, flags_store)) {
          return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: error in MSA aware collective renumbering, aborting ...\n"));
        }
      }
      *numfiles=sion_gendata->numfiles;
      *filenumber=sion_gendata->filenumber;
      if(rc != SION_SUCCESS) {
	_sion_flags_destroy_store(&flags_store);
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: error in _sion_generic_get_and_distribute_info_from_file"));
      }

    } else {
      /* buddy will determine file layout later, but need local commgroup to optimize file operations */

      sion_gendata->lrank=*lrank;
      sion_gendata->lsize=*lsize;
      
      if (filenumber == NULL) return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: No filenumber variable given"));
      if (numfiles == NULL)   return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: No numfiles variable given"));
      if ( (*filenumber < 0 ) || (*filenumber >= *numfiles) ) {
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: file number %d not valid (0..%d)",*filenumber,*numfiles));
      }
      sion_gendata->filenumber=*filenumber;
      sion_gendata->numfiles=*numfiles;
      
    }

  
  } else {
    _sion_flags_destroy_store(&flags_store);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: unknown file mode"));
  }

  /* generate local commgroup according to information from file (read) or parameter (write) */
  rc=sion_apidesc->create_lcg_cb(&sion_gendata->comm_data_local,sion_gendata->comm_data_global,
				 sion_gendata->grank,sion_gendata->gsize,
				 sion_gendata->lrank,sion_gendata->lsize,
				 sion_gendata->filenumber,sion_gendata->numfiles);
  if(rc) {
    _sion_flags_destroy_store(&flags_store);
    if(lprefix) free(lprefix);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_paropen_generic: error in create_local_commgroup callback function"));
  }

  /* set up parameters of call to generic open  */
  nfname=(sion_apidesc->get_multi_filename_cb?sion_apidesc->get_multi_filename_cb:_sion_get_multi_filename)(fname,sion_gendata->filenumber);
  help_globalrank=sion_gendata->grank;

  
  sid = _sion_newvcd(NULL,SION_FILEDESCRIPTOR);
    
  if(! (flags_store->mask&_SION_FMODE_BUDDY) ) {
    
    /* STANDARD  */
    DPRINTFP((1, "sion_generic_paropen", grank, "call parallel open of %d files (current name %s) in %s mode\n", sion_gendata->numfiles, nfname, file_mode));
    rc=_sion_paropen_generic_one_file(sid, nfname, flags_store, lprefix, &sion_gendata->numfiles, &sion_gendata->filenumber, &lchunksize, &lfsblksize, 
				       sion_gendata->lrank, sion_gendata->lsize, &help_globalrank, 
				      _SION_INTERNAL_FLAG_NORMAL, fileptr, sion_gendata, NULL);
    DPRINTFP((1, "sion_generic_paropen", sion_gendata->grank, "leave parallel open of %d files in %s mode #tasks=%d sid=%d globalrank=%d\n", sion_gendata->numfiles, 
	      file_mode, sion_gendata->lsize, sid, sion_gendata->grank));
    
  } else {
    
    /* branch BUDDY Checkpointing  */
    DPRINTFP((1, "sion_generic_paropen", grank, "call parallel buddy open of %d files (current name %s) in %s mode\n", sion_gendata->numfiles, nfname, file_mode));
    rc=_sion_paropen_generic_buddy(sid, fname, flags_store, lprefix, &sion_gendata->numfiles, &sion_gendata->filenumber, &lchunksize, &lfsblksize, 
				   sion_gendata->lrank, sion_gendata->lsize, &help_globalrank, 
				   fileptr, sion_gendata);
    DPRINTFP((1, "sion_generic_paropen", sion_gendata->grank, "leave parallel buddy open of %d files in %s mode #tasks=%d sid=%d globalrank=%d rc=%d\n", sion_gendata->numfiles, 
	      file_mode, sion_gendata->lsize, sid, sion_gendata->grank, rc));
   
  }

  _sion_flags_destroy_store(&flags_store);

  /* test sid and get internal data structure */
  if ((rc<0) || (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    DPRINTFP((1, "sion_generic_paropen", sion_gendata->grank, "invalid rc %d or sion_filedesc %d", rc, sid));
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen: invalid rc %d or sion_filedesc %d", rc, sid));
  }
  
  /* store additional data */
  sion_filedesc->dataptr=sion_gendata;

  if(newfname!=NULL) {
    *newfname=nfname;
  } else {
    if(nfname) free(nfname);
  }
  
  /* OUTPUT parameters */
  *fsblksize  = lfsblksize;
  *chunksize  = lchunksize;

  /* Bug? */

  DPRINTFP((64, "sion_generic_paropen", grank, "lprefix=%s (%x)\n", lprefix,lprefix));
  if(lprefix) free(lprefix);

  DPRINTFP((1, "sion_generic_paropen", grank, "leave parallel open of file %s sid=%d\n", fname, sid));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_generic_paropen");

  return (sid);
}


int sion_generic_parclose(int sid) {
  ONLY_DEBUG(int grank);
  int       mapping_size=0, rc=SION_SUCCESS;
  sion_int32 *mapping=NULL;
  _sion_filedesc *sion_filedesc;
  _sion_generic_gendata *sion_gendata;
  _sion_generic_apidesc *sion_apidesc;

  
  /*                                                                      */ DPRINTFTS(-1, "enter sion_generic_parclose");

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_parclose: invalid sion_filedesc %d", sid));
  }
  sion_gendata=sion_filedesc->dataptr;
  sion_apidesc=sion_gendata->apidesc;

  ONLY_DEBUG(grank=sion_gendata->grank);

  DPRINTFP((1, "sion_generic_parclose", grank, "enter parallel close of sid %d (%d file)\n", sid, sion_filedesc->nfiles));

  /* collect mapping information on rank 0 of first file, mapping=NULL for all others */
  _sion_generic_collect_mapping(sion_filedesc,&mapping_size,&mapping);

  if(! sion_filedesc->usebuddy ) {
    rc = _sion_parclose_generic( sid, sion_filedesc->rank, sion_filedesc->ntasks, mapping_size, mapping, _SION_INTERNAL_FLAG_NORMAL, 
				 sion_gendata, NULL );
  } else {
    rc = _sion_parclose_generic_buddy( sid, sion_filedesc->rank, sion_filedesc->ntasks, mapping_size, mapping, sion_gendata );
  }


  if (sion_apidesc->free_lcg_cb && sion_gendata->comm_data_local) {
    sion_apidesc->free_lcg_cb(sion_gendata->comm_data_local);
  }
  _SION_SAFE_FREE(mapping, NULL);

  _sion_freevcd(sid);

  sion_apidesc->barrier_cb(sion_gendata->comm_data_global);

  _SION_SAFE_FREE(sion_gendata, NULL);

  DPRINTFP((1, "sion_generic_parclose", grank, "leave parallel close of sid %d\n", sid));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_generic_parclose");

  return (rc);
}


int sion_generic_paropen_mapped(int         aid,
				char       *fname,
				const char *file_mode,
				int        *numFiles,
				void       *gcommgroup,  
				int         grank,
				int         gsize,
				int         *nlocaltasks,
				int        **globalranks,
				sion_int64 **chunksizes,
				int        **mapping_filenrs, 
				int        **mapping_lranks,
				sion_int32 *fsblksize,
				FILE      **fileptr ) 
{
  int        rc, sid = SION_ID_UNDEF;
  char       *lprefix=NULL;
  _sion_filedesc        *sion_filedesc;
  _sion_generic_apidesc *sion_apidesc;
  _sion_generic_gendata *sion_gendata;
  _sion_flags_store* flags_store = NULL;

  /*                                                                      */ DPRINTFTS(grank, "enter sion_generic_paropen_mapped");
  DPRINTFP((1, "sion_generic_paropen_mapped", grank, "enter generic parallel mapped open of file %s (global: %d of %d, nlocaltasks %d)\n", 
	    fname, grank, gsize, *nlocaltasks));

  if ( (aid<0) || (_sion_vcdtype(aid) != SION_APIDESCRIPTOR) || !(sion_apidesc = _sion_vcdtovcon(aid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_register_scattervr_cb: invalid sion_apidesc %d", aid));
  }

  if ((!sion_apidesc->create_lcg_cb) || (!sion_apidesc->free_lcg_cb) || (!sion_apidesc->barrier_cb) ||  (!sion_apidesc->bcastr_cb) ||  
      (!sion_apidesc->gatherr_cb) ||  (!sion_apidesc->gathervr_cb) ||   (!sion_apidesc->scatterr_cb) ||  (!sion_apidesc->scattervr_cb) ) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen_mapped: API %s not correctly initalized, aborting",sion_apidesc->name));
  }

  /* check parameters */
  if ( (grank < 0 ) || (grank >= gsize) ) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen_mapped: global rank %d not valid (0..%d)",grank,gsize));
  }

  /* create data container */
  sion_gendata=_sion_generic_alloc_gendata();
  if(!sion_gendata) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen_mapped: problems to alloc gendata, aborting ...\n"));
  }
  _sion_generic_init_gendata(sion_gendata);

  sion_gendata->aid=aid;
  sion_gendata->apidesc=sion_apidesc;
  sion_gendata->comm_data_global=gcommgroup;
  sion_gendata->grank=grank;
  sion_gendata->gsize=gsize;
  sion_gendata->lrank=-1;
  sion_gendata->lsize=-1;
  sion_gendata->filenumber=-1;
  sion_gendata->numfiles=-1;
  sion_gendata->comm_data_local=NULL;

  flags_store = _sion_parse_flags(file_mode);
  if ( ! flags_store ) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen_mapped: could not parse file mode in %s, aborting ...\n", file_mode));
  }

  if (flags_store->mask&_SION_FMODE_WRITE) {


    

    lprefix = calloc(SION_FILENAME_LENGTH,1);
    if (lprefix == NULL) {
      _sion_flags_destroy_store(&flags_store);
      return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen_mapped: cannot allocate temporary memory of size %lu (lprefix), aborting ...\n", (unsigned long) SION_FILENAME_LENGTH));
    }
    strcpy(lprefix, fname);

  } else if (flags_store->mask&_SION_FMODE_READ) {
    /* file mode READ */
    /* nothing to do here so far, filenumbers and mapping will be determined by in generic routine */

  } else {
    _sion_flags_destroy_store(&flags_store);
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen_mapped: unknown file mode"));
  }

  sid = _sion_newvcd(NULL,SION_FILEDESCRIPTOR);

  DPRINTFP((1, "sion_generic_paropen_mapped", grank, "enter parallel mapped open  in %s mode\n", file_mode));
  rc=_sion_paropen_mapped_generic(sid, fname, flags_store->mask, lprefix, 
				  numFiles, nlocaltasks, globalranks, chunksizes, mapping_filenrs, mapping_lranks, 
				  fsblksize, grank, gsize, _SION_INTERNAL_FLAG_NORMAL,
				  fileptr, sion_gendata);
  sion_gendata->numfiles=*numFiles;
  DPRINTFP((1, "sion_generic_paropen_mapped", sion_gendata->grank, "leave parallel open of %d files in %s mode #tasks=%d sid=%d globalrank=%d\n", sion_gendata->numfiles, 
	                               file_mode, sion_gendata->lsize, sid, sion_gendata->grank));

  _sion_flags_destroy_store(&flags_store);

  /* test sid again and get internal data structure */
  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_paropen_mapped: invalid sion_filedesc %d", sid));
  }
  
  /* store additional data */
  sion_filedesc->dataptr=sion_gendata;

  /* Bug? */

  DPRINTFP((1, "sion_generic_paropen_mapped", grank, "lprefix=%s (%x)\n", lprefix,lprefix));
  if(lprefix) free(lprefix);

  DPRINTFP((1, "sion_generic_paropen_mapped", grank, "leave parallel open of file %s sid=%d\n", fname, sid));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_generic_paropen_mapped");

  return (rc);
}


int sion_generic_parclose_mapped(int sid)
{
  int       grank, gsize, rc;
  _sion_filedesc *sion_filedesc;
  _sion_generic_gendata *sion_gendata;

  /*                                                                      */ DPRINTFTS(-1, "enter sion_generic_parclose_mapped");
  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_parclose_mapped: invalid sion_filedesc %d", sid));
  }
  sion_gendata=sion_filedesc->dataptr;

  grank=sion_gendata->grank;
  gsize=sion_gendata->gsize;

  DPRINTFP((1, "sion_generic_parclose_mapped", grank, "enter parallel close of sid %d\n", sid));

  DPRINTFP((1, "sion_generic_parclose_mapped", grank, "closing %d file(s)\n", sion_filedesc->nfiles));

  rc = _sion_parclose_mapped_generic(sid, grank, gsize,sion_gendata);

  _sion_freevcd(sid);

  _sion_generic_free_gendata(sion_gendata);

  DPRINTFP((1, "sion_generic_parclose_mapped", grank, "leave parallel close of sid %d\n", sid));
  /*                                                                      */ DPRINTFTS(-1, "leave sion_generic_parclose_mapped");

  return (rc);
}


int _sion_generic_get_and_distribute_info_from_file(  _sion_generic_gendata *sion_gendata, const char *fname)
{
  int       sid = -1,  ntasks = -1, nfiles = -1, t = 0;
  int       rc = SION_SUCCESS;
  FILE     *fileptr;
  sion_int32 fsblksize;
  int      *tasksinfile;
  int         mapping_size = -1;
  sion_int32 *mapping = NULL;
  sion_int32 lpos[2];
  _sion_generic_apidesc *sion_apidesc;

  sion_apidesc=sion_gendata->apidesc;

  if(sion_gendata->grank == 0) {
    /* open and get mapping of sion file */
    DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "before open\n"));
    sid=_sion_open_read(fname,_SION_FMODE_READ|_SION_FMODE_ANSI,_SION_READ_MASTER_ONLY_OF_MULTI_FILES,
			&ntasks,&nfiles,NULL,&fsblksize,NULL,&fileptr);
    /* sid = sion_open(fname, "br", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &fileptr); */
    if(sid>=0) {
      DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "after open\n"));
      rc=sion_get_mapping(sid,&mapping_size,&mapping,&sion_gendata->numfiles);
      DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "sion file %d files\n", sion_gendata->numfiles));
    } else {
      sion_gendata->numfiles=-1;
    }
  }

  /* each task has to know if more than file was used in sion file */
  sion_apidesc->bcastr_cb(&sion_gendata->numfiles, sion_gendata->comm_data_global, _SION_INT32, 1, 0);

  if((sion_gendata->grank == 0) && (sion_gendata->numfiles>1)) {
    if(mapping_size!=sion_gendata->gsize) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_get_and_distribute_info_from_file: Incorrect sum of ntasks of files %d <> %d\n", mapping_size, sion_gendata->gsize));
    }
  }

  if(sion_gendata->numfiles<0) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_get_and_distribute_info_from_file: could not get numfiles from sion file\n"));
  }

  if(sion_gendata->numfiles>1) {
    DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "before scatter\n"));
    if(sion_gendata->grank==0) {
      for(t=0;t<mapping_size;t++) {
	DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "  %d -> (%d,%d)\n",t,mapping[t*2],mapping[t*2+1]));
      }
    }

    sion_apidesc->scatterr_cb(mapping, lpos, sion_gendata->comm_data_global, _SION_INT32, 2, 0);
    sion_gendata->filenumber=lpos[0];
    sion_gendata->lrank     =lpos[1];
    DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "after scatter filenum+lrank (%d,%d)\n",sion_gendata->filenumber,sion_gendata->lrank));

    if(sion_gendata->grank==0) {
      tasksinfile = (int *) malloc(sion_gendata->numfiles * sizeof(int));
      if (tasksinfile == NULL) {
	return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"_sion_generic_get_and_distribute_info_from_file: Cannot allocate memory for tasksinfile counter vector"));
      }
      for(t=0;t<sion_gendata->numfiles;t++) tasksinfile[t]=0;                   /* init counter */
      for(t=0;t<mapping_size;t++)  tasksinfile[ mapping[t*2] ]++;	              /* count tasks in file  */
      for(t=0;t<mapping_size;t++)  mapping[t*2+1]=tasksinfile[ mapping[t*2] ];  /* set 2nd value of mapping to lsize  */
    }
    sion_apidesc->scatterr_cb(mapping, lpos, sion_gendata->comm_data_global, _SION_INT32, 2, 0);
    sion_gendata->lsize     =lpos[1];
    DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "after scatter lsize (%d, %d of %d)\n",sion_gendata->filenumber, sion_gendata->lrank, sion_gendata->lsize));

    if(sion_gendata->grank==0) {
      if(tasksinfile) free(tasksinfile);
    }
    /* WARNING: mapping file of sion file is now destroyed and should not be used until close  */

  } else {
    sion_gendata->filenumber=0;
    sion_gendata->lrank     = sion_gendata->grank;
    sion_gendata->lsize     = sion_gendata->gsize;
    DPRINTFP((1, "_sion_generic_get_and_distribute_info_from_file", sion_gendata->grank, "only one file -> filenumber=%d lRank=%d\n",sion_gendata->filenumber,sion_gendata->lrank));
  }

  if(sion_gendata->grank == 0) {
    /* frees also mapping vector */
    if (sid>=0) _sion_close_sid(sid);
  }

  return(rc);
}

int sion_generic_parreinit(int sid, sion_int64 chunksize ) {
  int       grank, gsize, rc;
  _sion_filedesc *sion_filedesc;
  _sion_generic_gendata *sion_gendata;

  if ( (sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR) || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    return(_sion_errorprint(SION_ID_NOT_VALID,_SION_ERROR_RETURN,"sion_generic_parreinit: invalid sion_filedesc %d", sid));
  }
  sion_gendata=sion_filedesc->dataptr;
  
  grank=sion_gendata->grank;
  gsize=sion_gendata->gsize;
  
  DPRINTFP((1, "sion_generic_parreinit", grank, "enter parallel reinit of sid %d\n", sid));

  rc = _sion_parreinit_generic(sid, chunksize, grank, gsize, sion_gendata);

  DPRINTFP((1, "sion_generic_parreinit", grank, "leave parallel reinit of sid %d\n", sid));

  return(rc);
}
