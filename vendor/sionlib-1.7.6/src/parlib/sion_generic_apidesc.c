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

#include "sion.h"
#include "sion_internal.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_generic_apidesc.h"

/*!\brief Allocates memory for internal sion api desc structure
 *
 * @retval      pointer to a new _sion_generic_apidesc structure
 */
_sion_generic_apidesc * _sion_generic_alloc_apidesc(void) {
  _sion_generic_apidesc *sion_apidesc;
  
  sion_apidesc = (_sion_generic_apidesc *) malloc(sizeof(_sion_generic_apidesc));
  if (sion_apidesc == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate apidescriptor structure of size %lu (sion_apidesc), aborting ...\n", (unsigned long) sizeof(_sion_generic_apidesc));
    return(NULL);
  }
  
  return(sion_apidesc);
}

/*!\brief Initialize the sion api description
 *
 * @param  sion_apidesc  sion API description
 *
 * @retval  SION_SUCCESS if OK
 */
int _sion_generic_init_apidesc(_sion_generic_apidesc *sion_apidesc)
{

  int       rc = SION_SUCCESS;

  if (sion_apidesc == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_init_apidesc: cannot initalized, data structure is not allocated, aborting ...\n"));
  }
  sion_apidesc->aid = -1;
  sion_apidesc->name = NULL;
  sion_apidesc->level=SION_GENERIC_API_LEVEL_NONE;
  sion_apidesc->create_lcg_cb = NULL;
  sion_apidesc->free_lcg_cb = NULL;
  sion_apidesc->barrier_cb = NULL;
  sion_apidesc->bcastr_cb = NULL;
  sion_apidesc->gatherr_cb = NULL;
  sion_apidesc->gathervr_cb = NULL;
  sion_apidesc->scatterr_cb = NULL;
  sion_apidesc->scattervr_cb = NULL;
  sion_apidesc->gather_execute_cb = NULL;
  sion_apidesc->execute_scatter_cb = NULL;
  sion_apidesc->get_multi_filename_cb = NULL;
  sion_apidesc->get_capability_cb = NULL;
  return(rc);

}

/*!\brief free apidesc structure
 *
 * @param  sion_apidesc  sion API description
 *
 * @retval  SION_SUCCESS if OK
 */
int _sion_generic_free_apidesc(_sion_generic_apidesc *sion_apidesc)
{

  int       rc = SION_SUCCESS;

  if (sion_apidesc == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_free_apidesc: cannot free, data structure is not allocated, aborting ...\n"));
  }

  if (sion_apidesc->name)  free(sion_apidesc->name);

  free(sion_apidesc);

  return(rc);

}


/*!\brief Allocates memory for internal sion api data structure needed for each SION file
 *
 * @retval      pointer to a new _sion_generic_gendata structure
 */
_sion_generic_gendata * _sion_generic_alloc_gendata(void) {
  _sion_generic_gendata *sion_gendata;
  
  sion_gendata = (_sion_generic_gendata *) malloc(sizeof(_sion_generic_gendata));
  if (sion_gendata == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate gendata structure of size %lu (sion_gendata), aborting ...\n", (unsigned long) sizeof(_sion_generic_gendata));
    return(NULL);
  }
  
  return(sion_gendata);
}

/*!\brief Initialize the sion file description
 *
 * @param *sion_gendata     sion gendata description struct
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_generic_init_gendata(_sion_generic_gendata *sion_gendata)
{

  int       rc = SION_SUCCESS;

  if (sion_gendata == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_init_gendata: cannot initalized, data structure is not allocated, aborting ...\n"));
  }
  sion_gendata->apidesc = NULL;
  sion_gendata->aid = -1;
  sion_gendata->comm_data_global = NULL;
  sion_gendata->comm_data_local = NULL;
 
  return(rc);

}

/*!\brief Free sion gendata description
 *
 * @param *sion_gendata        sion gendata description struct
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_generic_free_gendata(_sion_generic_gendata *sion_gendata)
{

  int       rc = SION_SUCCESS;

  if (sion_gendata == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_free_gendata: cannot free, data structure is not allocated, aborting ...\n"));
  }
  free(sion_gendata);

  return(rc);

}


/*!\brief Update API level 
 *
 * @param[inout]   sion_apidesc   sion gendata description struct
 *
 * @retval      SION_SUCCESS if OK
 */
int _sion_generic_update_api_level(_sion_generic_apidesc *sion_apidesc)
{

  int       rc = SION_SUCCESS;

  if (sion_apidesc == NULL) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_generic_update_api_level: cannot update data structure, it is not allocated, aborting ...\n"));
  }

  if ( 
      (sion_apidesc->create_lcg_cb) && 
       (sion_apidesc->free_lcg_cb) && 
       (sion_apidesc->barrier_cb) &&
       (sion_apidesc->bcastr_cb) &&  
       (sion_apidesc->gatherr_cb) &&  
       (sion_apidesc->gathervr_cb) &&   
       (sion_apidesc->scatterr_cb) &&
       (sion_apidesc->scattervr_cb) 
       ) {
    sion_apidesc->level=SION_GENERIC_API_LEVEL_STD;
    
    if (
	(sion_apidesc->gather_execute_cb) && 
	(sion_apidesc->execute_scatter_cb) && 
	(sion_apidesc->get_capability_cb) 
	) {
      sion_apidesc->level=SION_GENERIC_API_LEVEL_FULL;
    }
  }

  return(rc);

}
