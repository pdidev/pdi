/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#ifndef OMPI_PARTEST_H_
#define OMPI_PARTEST_H_

/* Enable or disable the checksum */
#define CHECKSUM
#define FNAMELEN 255
#define MB *1024*1024
#define MAXPE 28*64*1024
#define MAXCHARLEN 256

#define WAKEUP   102
#define COLPRINT 103

#include "partest_opts.h"

struct _test_communicators_struct {
  MPI_Comm all;             /* all tasks in mpi program */
  MPI_Comm work;            /* all working tasks, e.g. if bluegene_np is used */
  MPI_Comm workread;        /* all working tasks, shifted by read_task_offset, e.g. if bluegene_np is used */
  MPI_Comm local;           /* tasks which should work on the same file */
  int all_size, all_rank, work_size, work_rank, workread_size, workread_rank, local_size, local_rank;
  int file_number, ionode_number;
};

typedef struct _test_communicators_struct _test_communicators;

#ifdef SION_OMPI
int test_paropen_multi_ompi (char *filename,
			    char *localbuffer,
                            _test_communicators *communicators,
			    _test_options *options
);
#endif /* SION_OMPI */
#ifdef SION_OMP
int test_paropen_omp (char *filename,
			    char *localbuffer,
                            _test_communicators *communicators,
			    _test_options *options
);
#endif /* SION_OMPI */


int barrier_after_start (MPI_Comm comm);
int barrier_after_malloc(MPI_Comm comm);
int barrier_after_open  (MPI_Comm comm);
int barrier_after_write (MPI_Comm comm);
int barrier_after_read  (MPI_Comm comm);
int barrier_after_close (MPI_Comm comm);
int barrier_before_unlink (MPI_Comm comm);
int barrier_after_unlink (MPI_Comm comm);

int collective_print_gather ( char* cbuffer, MPI_Comm comm);


/* Arguments for dtype in reduce_omp*/
#define _PARTEST_SION_INT32     10
#define _PARTEST_SION_INT64     11
#define _PARTEST_DOUBLE    12
void reduce_omp(void *syncdata, void * out, MPI_Op op, int dtype);



#endif /* PARTEST_H_ */
