/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_FORTRAN_MPI_H_
#define SION_FORTRAN_MPI_H_

#include <mpi.h>
#include "sion.h"

/* sion_paropen_mpi() */
#if defined(_FORTRANCAPS)
#define fsion_paropen_mpi_c FSION_PAROPEN_MPI_C

#elif defined(_FORTRANNOCAPS)
#define fsion_paropen_mpi_c fsion_paropen_mpi_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_paropen_mpi_c fsion_paropen_mpi_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_paropen_mpi_c fsion_paropen_mpi_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_paropen_mpi_c(char *fname,
                       char *file_mode,
		       int  *numFiles,
                       MPI_Fint * fgComm,
                       MPI_Fint * flComm,
                       sion_int64 *chunksize, 
		       sion_int32 *fsblksize, 
		       int *globalrank, 
		       char *newfname,
		       int *sid, 
		       int  fname_len, 
		       int  file_mode_len,
		       int  newfname_len);

/* sion_paropen_multi_mpi() */
#if defined(_FORTRANCAPS)
#define fsion_paropen_multi_mpi_c FSION_PAROPEN_MULTI_MPI_C

#elif defined(_FORTRANNOCAPS)
#define fsion_paropen_multi_mpi_c fsion_paropen_multi_mpi_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_paropen_multi_mpi_c fsion_paropen_multi_mpi_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_paropen_multi_mpi_c fsion_paropen_multi_mpi_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_paropen_multi_mpi_c(  char *fname,
                               char *file_mode,
                               int  *numFiles,
                               MPI_Fint *fgComm,
                               MPI_Fint *flComm,
                               sion_int64 *chunksize,
                               sion_int32 *fsblksize,
                               int  *globalrank,
                               int  *sid,
                               char *newfname,
                               int  fname_len,
                               int  file_mode_len,
                               int  newfname_len);

/* sion_parclose_mpi() */
#if defined(_FORTRANCAPS)
#define fsion_parclose_mpi_c FSION_R

#elif defined(_FORTRANNOCAPS)
#define fsion_parclose_mpi_c fsion_parclose_mpi_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_parclose_mpi_c fsion_parclose_mpi_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_parclose_mpi_c fsion_parclose_mpi_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_parclose_mpi_c(int  *sid, int *ierr);

#endif /* SION_FORTRAN_MPI_H_ */
