/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_FORTRAN_WRITEREAD_MPI_F77_H_
#define SION_FORTRAN_WRITEREAD_MPI_F77_H_

#include "sion.h"


/* sion_coll_fwrite_mpi() */
#if defined(_FORTRANCAPS)
#define fsion_coll_fwrite_mpi FSION_COLL_FWRITE_MPI

#elif defined(_FORTRANNOCAPS)
#define fsion_coll_fwrite_mpi fsion_coll_fwrite_mpi

#elif defined(_FORTRANUNDERSCORE)
#define fsion_coll_fwrite_mpi fsion_coll_fwrite_mpi_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_coll_fwrite_mpi fsion_coll_fwrite_mpi__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_coll_fwrite_mpi(const void *data, 
			   sion_int64 *size, 
			   sion_int64 *nitems, 
			   int *sid, 
			   sion_int64 *rc);

/* sion_coll_fread_mpi() */
#if defined(_FORTRANCAPS)
#define fsion_coll_fread_mpi FSION_COLL_FREAD_MPI

#elif defined(_FORTRANNOCAPS)
#define fsion_coll_fread_mpi fsion_coll_fread_mpi

#elif defined(_FORTRANUNDERSCORE)
#define fsion_coll_fread_mpi fsion_coll_fread_mpi_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_coll_fread_mpi fsion_coll_fread_mpi__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_coll_fread_mpi(void *data, 
			  sion_int64 *size, 
			  sion_int64 *nitems, 
			  int *sid, 
			  sion_int64 *rc);

#endif /* SION_FORTRAN_H_ */
