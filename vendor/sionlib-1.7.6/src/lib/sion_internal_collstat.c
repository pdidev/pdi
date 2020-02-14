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

#include "sion_const.h"
#include "sion_internal.h"
#include "sion_error_handler.h"
#include "sion_debug.h"

#include "sion_internal_collstat.h"

#define DFUNCTION "_sion_create_and_init_collstat"
_sion_collstat * _sion_create_and_init_collstat( _sion_filedesc *sion_filedesc ) {
  _sion_collstat *collstat;
  int t;

  collstat = (_sion_collstat *) malloc(sizeof(_sion_collstat));
  if (collstat == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate collstat structure of size %lu (sion_collstat), aborting ...\n",
		     (unsigned long) sizeof(_sion_collstat));
    return(NULL);
  }
  
  collstat->req_collsize=sion_filedesc->collsize;
  collstat->firstsize=0;
  collstat->gsize=0;
  collstat->avg_size_per_sender=0;
  collstat->max_size_per_sender=0;
  collstat->min_size_per_sender=1e10;

  collstat->num_collectors=0;
  collstat->avg_sender_per_collector=0;
  collstat->max_sender_per_collector=0;
  collstat->min_sender_per_collector=10000000;
  collstat->avg_size_per_collector=0;
  collstat->max_size_per_collector=0; 
  collstat->min_size_per_collector=1e10;

 
  /* calculate overall data size */
  collstat->gsize=0;
  for (t = 0; t < sion_filedesc->ntasks; t++) {
    collstat->gsize+=sion_filedesc->all_chunksizes[t];
    if(sion_filedesc->all_chunksizes[t]>collstat->max_size_per_sender) collstat->max_size_per_sender=sion_filedesc->all_chunksizes[t];
    if(sion_filedesc->all_chunksizes[t]<collstat->min_size_per_sender) collstat->min_size_per_sender=sion_filedesc->all_chunksizes[t];
  }
  if(sion_filedesc->ntasks>0) collstat->avg_size_per_sender = (double) collstat->gsize / (double) sion_filedesc->ntasks;
  
  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "gsize=%ld\n", (long) collstat->gsize));

  return(collstat);
}
#undef DFUNCTION


#define DFUNCTION "_sion_update_collstat"
int _sion_update_collstat( _sion_collstat *collstat, _sion_filedesc *sion_filedesc ) {
  int rc = SION_SUCCESS;
  int t, s; 
  sion_int64 currentsize;

  /* calculate overall data size */

  for (t = 0; t < sion_filedesc->ntasks; t++) {

    if(t == sion_filedesc->all_coll_collector[t]) {
      /* its a collector */

      /* accumulate data for collector */
      currentsize=0;
      for (s = t; s < t+sion_filedesc->all_coll_collsize[t]; s++) {
	currentsize  += sion_filedesc->all_chunksizes[s];
      }
      
      /* statistics */
      collstat->num_collectors++;
      collstat->avg_size_per_collector+=currentsize;
      if(currentsize>collstat->max_size_per_collector) collstat->max_size_per_collector=currentsize;
      if(currentsize<collstat->min_size_per_collector) collstat->min_size_per_collector=currentsize;
      if(sion_filedesc->all_coll_collsize[t]<collstat->min_sender_per_collector) collstat->min_sender_per_collector=sion_filedesc->all_coll_collsize[t];
      if(sion_filedesc->all_coll_collsize[t]>collstat->max_sender_per_collector) collstat->max_sender_per_collector=sion_filedesc->all_coll_collsize[t];

    } 
  }
  if(collstat->num_collectors>0) collstat->avg_size_per_collector/=collstat->num_collectors;

  return(rc);
}
#undef DFUNCTION


#define DFUNCTION "_sion_print_collstat"
int _sion_print_collstat( _sion_collstat *collstat, _sion_filedesc *sion_filedesc ) {
  int rc = SION_SUCCESS;
  int t,s;
  
  fprintf(stderr, "collective statistics:             req_collsize=%11d\n",        collstat->req_collsize);
  fprintf(stderr, "collective statistics:       req_num_collectors=%11d\n",        collstat->req_num_collectors);
  fprintf(stderr, "collective statistics:           num_collectors=%11d\n",        collstat->num_collectors);
  fprintf(stderr, "collective statistics: avg_sender_per_collector=%14.2f\n",      collstat->avg_sender_per_collector);
  fprintf(stderr, "collective statistics: min_sender_per_collector=%11d\n",        collstat->min_sender_per_collector);
  fprintf(stderr, "collective statistics: max_sender_per_collector=%11d\n",        collstat->max_sender_per_collector);

  fprintf(stderr, "collective statistics:   avg_size_per_collector=%14.2f bytes\n",    collstat->avg_size_per_collector);
  fprintf(stderr, "collective statistics:   min_size_per_collector=%11lld    bytes\n", collstat->min_size_per_collector);
  fprintf(stderr, "collective statistics:   max_size_per_collector=%11lld    bytes\n", collstat->max_size_per_collector);
    
  fprintf(stderr, "collective statistics:      avg_size_per_sender=%14.2f bytes\n",    collstat->avg_size_per_sender);
  fprintf(stderr, "collective statistics:      min_size_per_sender=%11lld    bytes\n", collstat->min_size_per_sender);
  fprintf(stderr, "collective statistics:      max_size_per_sender=%11lld    bytes\n", collstat->max_size_per_sender);
 
  if(sion_filedesc->colldebug>=3) {
    for (t = 0; t < sion_filedesc->ntasks; t++) {
    
      if(t == sion_filedesc->all_coll_collector[t]) {
	/* its a collector */
	fprintf(stderr,"collective statistics:   startpointers[%2d]=%10lld (%10.4fMB) chunksizes[%2d]=%8lld COLLECTOR collsize=%3d\n",
		t, sion_filedesc->all_startpointers[t], sion_filedesc->all_startpointers[t] / 1024.0 / 1024.0, 
		t, sion_filedesc->all_chunksizes[t],
		sion_filedesc->all_coll_collsize[t]    );
      
	for (s = t+1; s < t+sion_filedesc->all_coll_collsize[t]; s++) {
	  fprintf(stderr,"collective statistics:   startpointers[%2d]=%10lld (%10.4fMB) chunksizes[%2d]=%8lld     SENDER to %3d\n",
		  s, sion_filedesc->all_startpointers[s], sion_filedesc->all_startpointers[s] / 1024.0 / 1024.0, 
		  s, sion_filedesc->all_chunksizes[s], t );
	}
      }
    }
  }
  
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_debugprint_collstat"
int _sion_debugprint_collstat( _sion_collstat *sion_collstat, _sion_filedesc *sion_filedesc ) {
  int rc = SION_SUCCESS;
  int t, s; 


  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "  enter\n"));

  for (t = 0; t < sion_filedesc->ntasks; t++) {
    if(t == sion_filedesc->all_coll_collector[t]) {
      /* its a collector */
      DPRINTFP((2048, DFUNCTION, _SION_DEFAULT_RANK, "  startpointers[%2d]=%10lld (%10.4fMB) chunksizes[%2d]=%8lld COLLECTOR collsize=%3d\n",
		t, sion_filedesc->all_startpointers[t], sion_filedesc->all_startpointers[t] / 1024.0 / 1024.0, 
		t, sion_filedesc->all_chunksizes[t],
		sion_filedesc->all_coll_collsize[t]    ));
      
      for (s = t+1; s < t+sion_filedesc->all_coll_collsize[t]; s++) {
	DPRINTFP((2048, DFUNCTION, _SION_DEFAULT_RANK, "  startpointers[%2d]=%10lld (%10.4fMB) chunksizes[%2d]=%8lld     SENDER to %3d\n",
		  s, sion_filedesc->all_startpointers[s], sion_filedesc->all_startpointers[s] / 1024.0 / 1024.0, 
		  s, sion_filedesc->all_chunksizes[s], t ));
      }
    } 
  }

  DPRINTFP((2, DFUNCTION, _SION_DEFAULT_RANK, "  leave\n",t));
  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_dprint_collstat"
int _sion_destroy_collstat( _sion_collstat *sion_collstat ) {
  int rc = SION_SUCCESS;

  if(sion_collstat) {
    free(sion_collstat);
  }

  return(rc);
}
#undef DFUNCTION
