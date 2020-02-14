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

#ifndef SION_SION_OMP_CB_GEN_H
#define SION_SION_OMP_CB_GEN_H

#ifdef SION_OMP

#include "sion_datatypes.h"
#include "sion_filedesc.h"

typedef struct _omp_api_commdata_struct _omp_api_commdata;
struct _omp_api_commdata_struct {
  int      commset;
  int      thread_num;
  int      num_threads;
};

int _sion_register_callbacks_omp(void);

int _sion_omp_create_lcg_cb(void **local_commgroup, void *global_commgroup, 
			    int grank, int gsize, 
			    int lrank, int lsize,
			    int filenumber, int numfiles
			    );
int _sion_omp_free_lcg_cb(void *local_commgroup);


int _sion_omp_barrier_cb(void *commdata);
int _sion_omp_bcastr_cb(void *data, void *commdata, int dtype, int nelem, int root);
int _sion_omp_gatherr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root);
int _sion_omp_scatterr_cb(void *indata, void *outdata, void *commdata, int dtype, int nelem, int root);
int _sion_omp_gathervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root);
int _sion_omp_scattervr_cb(void *indata, void *outdata, void *commdata, int dtype, int *counts, int nelem, int root);

/* collective gather/scatter & process operation: */
/* this function collects on tasks <collector> data from a number of
   tasks (<range_start> to <range_end>) and calls for each tasks the process function.
   Additionally to the binary data a spec vector has also to be collected.
   the process function has two parameters: indata and spec (from each task) 
   spec[0] -> offset, spec[1] -> len
   Constraints:
    - process must be called in the order of the ranks from data is collected
    - process can be called multiple times if data is split in multiple parts (because of buffer size)
      spec[0] and spec[1] have to be adjusted in that case 
*/

int _sion_omp_gather_process_cb(const void *indata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				void *commdata,  int collector, int range_start, int range_end, int sid, 
				int process_cb(const void *,sion_int64 *, int ) );

int _sion_omp_process_scatter_cb(void *outdata, sion_int64 *spec, int spec_len, sion_int64 fsblksize,
				 void *commdata,  int collector, int range_start, int range_end, int sid, 
				 int process_cb(void *,sion_int64 *, int ) );

int _sion_omp_get_capability_cb(void *commdata );

#endif

#endif
