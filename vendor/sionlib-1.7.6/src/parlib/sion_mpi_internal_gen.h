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

#ifndef SION_SION_MPI_INTERNAL_GEN_H
#define SION_SION_MPI_INTERNAL_GEN_H

#ifdef SION_MPI

#include "mpi.h"

#include "sion_const.h"

#define USE_PMPIno
#ifdef USE_PMPI
#define MPI_Comm_rank      PMPI_Comm_rank
#define MPI_Comm_size      PMPI_Comm_size	  
#define MPI_Gather	   PMPI_Gather	  
#define MPI_Scatter	   PMPI_Scatter	  
#define MPI_Bcast	   PMPI_Bcast	  
#define MPI_Barrier	   PMPI_Barrier	  
#define MPI_Comm_split     PMPI_Comm_split    
#define MPI_Send           PMPI_Send
#define MPI_Recv           PMPI_Recv
#endif

struct _sion_filedesc_mpiadd_struct {
  MPI_Comm gComm;
  MPI_Comm lComm;
  double ts;
  int blocksize;
  int step;
};
typedef struct _sion_filedesc_mpiadd_struct _sion_filedesc_mpiadd;

int _sion_gen_info_from_gcomm_mpi(int numFiles, MPI_Comm gComm, int *filenumber, int *lrank, int *lsize);
int _sion_get_info_from_splitted_comm_mpi(MPI_Comm gComm, MPI_Comm lComm, int *numComm, int *CommNumber, int *lrank, int *lsize);

int _sion_get_numfiles_from_file_mpi(char *fname);
int _sion_get_filenumber_from_files_mpi(char *fname, MPI_Comm gComm, int *numfiles, int *filenumber, int *lRank);
int _sion_errorprint_mpi(int rc, int level, const char *format, ...);

#endif

#endif
