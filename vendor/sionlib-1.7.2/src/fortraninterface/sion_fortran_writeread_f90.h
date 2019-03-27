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

/*!
 * @file sion_fortran.h
 * @brief Fortran API
 * @author Ventsislav Petkov
 * @date 14.08.2008
 * @date 03.05.2013 modifications to support different Fortran interfaces, Florian Janetzko
 */

/* sion_write() */
#if defined(_FORTRANCAPS)
#define fsion_write_c FSION_WRITE_C

#elif defined(_FORTRANNOCAPS)
#define fsion_write_c fsion_write_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_write_c fsion_write_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_write_c fsion_write_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_write_c(const void *data, 
                   sion_int64 *size, 
                   sion_int64 *nitems, 
                   int *sid, 
                   sion_int64 *rc);

/* sion_fwrite() */
#if defined(_FORTRANCAPS)
#define fsion_fwrite_c FSION_FWRITE_C

#elif defined(_FORTRANNOCAPS)
#define fsion_fwrite_c fsion_fwrite_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_fwrite_c fsion_fwrite_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_fwrite_c fsion_fwrite_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_fwrite_c(const void *data, 
                    sion_int64 *size, 
                    sion_int64 *nitems, 
                    int *sid, 
                    sion_int64 *rc);

/* sion_read() */
#if defined(_FORTRANCAPS)
#define fsion_read_c FSION_READ_C

#elif defined(_FORTRANNOCAPS)
#define fsion_read_C fsion_read_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_read_c fsion_read_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_read_c fsion_read_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_read_c(void *data, 
                  sion_int64 *size, 
                  sion_int64 *nitems, 
                  int *sid, 
                  sion_int64 *rc);

/* sion_fread() */
#if defined(_FORTRANCAPS)
#define fsion_fread_c FSION_FREAD_C

#elif defined(_FORTRANNOCAPS)
#define fsion_fread_c fsion_fread_c

#elif defined(_FORTRANUNDERSCORE)
#define fsion_fread_c fsion_fread_c_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_fread_c fsion_fread_c__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_fread_c(void *data, 
                   sion_int64 *size, 
                   sion_int64 *nitems, 
                   int *sid, 
                   sion_int64 *rc);


#endif /* SION_FORTRAN_H_ */
