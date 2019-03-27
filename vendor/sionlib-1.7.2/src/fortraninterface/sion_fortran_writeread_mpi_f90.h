/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_FORTRAN_WRITEREAD_F90_H_
#define SION_FORTRAN_WRITEREAD_F90_H_

#include "sion.h"

/* sion_coll_fwrite_mpi() */
#if defined(_FORTRANCAPS)
#define fsion_coll_fwrite_mpi_c FSION_COLL_FWRITE_MPI_C

#elif defined(_FORTRANNOCAPS)
#define fsion_coll_fwrite_mpi_c fsion_coll_fwrite_mpi_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_coll_fwrite_mpi_c fsion_coll_fwrite_mpi_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_coll_fwrite_mpi_c fsion_coll_fwrite_mpi_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_coll_fwrite_mpi_c(const void *data, 
			     sion_int64 *size, 
			     sion_int64 *nitems, 
			     int *sid, 
			     sion_int64 *rc);

/* sion_coll_fread_mpi() */
#if defined(_FORTRANCAPS)
#define fsion_coll_fread_mpi_c FSION_COLL_FREAD_MPI_C

#elif defined(_FORTRANNOCAPS)
#define fsion_coll_fread_mpi_c fsion_coll_fread_mpi_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_coll_fread_mpi_c fsion_coll_fread_mpi_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_coll_fread_mpi_c fsion_coll_fread_mpi_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_coll_fread_mpi_c(void *data, 
			    sion_int64 *size, 
			    sion_int64 *nitems, 
			    int *sid, 
			    sion_int64 *rc);


#endif /* SION_FORTRAN_H_ */
