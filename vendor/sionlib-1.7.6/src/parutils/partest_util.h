/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#ifndef PARTEST_UTIL_H_
#define PARTEST_UTIL_H_

#include "sion.h"
#include "partest_opts.h"
#include "partest_split_comm.h"

#define TIMINGS_MSGS 0
#define TIMINGS_WR_CREATE 1
#define TIMINGS_WR_CREATE_BARR_OPEN 2
#define TIMINGS_WR_CREATE_BARR_CLOSE 3
#define TIMINGS_WR_CREATE_CLOSE 4
#define TIMINGS_WR_OPEN 5
#define TIMINGS_WR_OPEN_BARR_FILE 6
#define TIMINGS_WR_OPEN_BARR_GLOBAL 7
#define TIMINGS_WR_WRITE_SYNC 8
#define TIMINGS_WR_WRITE 9
#define TIMINGS_WR_WRITE_BARR_FILE 10
#define TIMINGS_WR_WRITE_BARR_GLOBAL 11
#define TIMINGS_WR_CLOSE 12
#define TIMINGS_WR_CLOSE_BARR_FILE 13
#define TIMINGS_WR_CLOSE_BARR_GLOBAL 14
#define TIMINGS_WR_TOTAL 15
#define TIMINGS_RD_OPEN 16
#define TIMINGS_RD_OPEN_BARR_FILE 17
#define TIMINGS_RD_OPEN_BARR_GLOBAL 18
#define TIMINGS_RD_READ_SYNC 19
#define TIMINGS_RD_READ 20
#define TIMINGS_RD_READ_BARR_FILE 21
#define TIMINGS_RD_READ_BARR_GLOBAL 22
#define TIMINGS_RD_CLOSE 23
#define TIMINGS_RD_CLOSE_BARR_FILE 24
#define TIMINGS_RD_CLOSE_BARR_GLOBAL 25
#define TIMINGS_RD_TOTAL 26
#define TIMINGS_MAX_NUM 27

#define STATS_BYTES_WR_WROTE 0
#define STATS_BYTES_WR_NUM_CHUNKS 1
#define STATS_BYTES_RD_READ 2
#define STATS_BYTES_RD_NUM_CHUNKS 3
#define STATS_WR_NUM_FILES 4
#define STATS_RD_NUM_FILES 5
#define STATS_MAX_NUM 6

#define TIMINGS_METHOD_WRITE 0
#define TIMINGS_METHOD_READ 1

#define toMiB (1024.0 * 1024.0)
#define toMB  (1000.0 * 1000.0)

int barrier_after_start (MPI_Comm comm);
int barrier_after_malloc(MPI_Comm comm);
int barrier_after_open  (MPI_Comm comm);
int barrier_after_write (MPI_Comm comm);
int barrier_after_read  (MPI_Comm comm);
int barrier_after_close (MPI_Comm comm);
int barrier_before_unlink (MPI_Comm comm);
int barrier_after_unlink (MPI_Comm comm);

int write_timings ( char *set, int method, double *timings, sion_int64 *stats,
		    _test_communicators *communicators, 
		    _test_options *options,
		    int collective);

int collective_print_gather ( char* cbuffer, MPI_Comm comm);
int collective_print ( char* cbuffer, MPI_Comm comm);


#endif /* PARTEST_H_ */
