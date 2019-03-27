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
 */

#ifndef SION_SION_OMPI_INTERNAL_GEN_H
#define SION_SION_OMPI_INTERNAL_GEN_H

#ifdef SION_OMPI

#include "mpi.h"

#include "sion_const.h"

struct __ompi_thread_sync_struct
{
	int grank_master_mpi;
	int grank_master_ompi;
	int lrank_master_mpi;
	int lrank_master_ompi;
	int gsize_mpi;
	int gsize_ompi;
	int lsize_mpi;
	int lsize_ompi;
	int num_threads;
	int numFiles;
	int filenumber;
};

typedef struct __ompi_thread_sync_struct __ompi_thread_sync;


int _sion_gen_info_from_gcomm_ompi(int numFiles, MPI_Comm gComm, int *filenumber, int *lrank, int *lsize);
int _sion_get_info_from_splitted_comm_ompi(MPI_Comm gComm, MPI_Comm lComm, int *numComm, int *CommNumber, int *lrank, int *lsize);

int _sion_get_size_ompi(int ompi_rank, int num_threads);
int _sion_map_rank_mpi_to_ompi(int mpi_rank, int num_threads, int thread_num);
int _sion_map_rank_ompi_to_mpi(int ompi_rank, int num_threads);
int _sion_map_rank_ompi_to_thread_num(int ompi_rank, int num_threads);


int _sion_errorprint_ompi(int rc, int level, const char *format, ...);

#endif

#endif
