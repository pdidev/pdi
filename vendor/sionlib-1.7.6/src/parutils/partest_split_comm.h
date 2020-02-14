/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef PARTEST_SPLIT_COMM_H_
#define PARTEST_SPLIT_COMM_H_

#define WAKEUP   102
#define COLPRINT 103
#define MAXCHARLEN 350

struct _test_communicators_struct {
  MPI_Comm all;             /*!< all tasks in mpi program */
  MPI_Comm work;            /*!< all working tasks, e.g. if bluegene_np is used */
  MPI_Comm workread;        /*!< all working tasks, shifted by read_task_offset, e.g. if bluegene_np is used */
  MPI_Comm local;           /*!< tasks which should work on the same file */
  int all_size, all_rank, work_size, work_rank, workread_size, workread_rank, local_size, local_rank;
  int file_number, ionode_number;
};

typedef struct _test_communicators_struct _test_communicators;

int split_communicator(_test_communicators * communicators, int bluegene, int bluegene_np, int bluegene_sort, int numfiles, int read_task_offset, int verbose);
int collective_print(char *cbuffer, MPI_Comm comm);
int collective_print_gather(char *cbuffer, MPI_Comm comm);

#endif
