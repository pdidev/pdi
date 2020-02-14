/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_FORTRAN_WRITEREAD_F77_H_
#define SION_FORTRAN_WRITEREAD_F77_H_

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
#define fsion_write FSION_WRITE

#elif defined(_FORTRANNOCAPS)
#define fsion_write fsion_write

#elif defined(_FORTRANUNDERSCORE)
#define fsion_write fsion_write_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_write fsion_write__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_write(const void *data, 
                   sion_int64 *size, 
                   sion_int64 *nitems, 
                   int *sid, 
                   sion_int64 *rc);

/* sion_fwrite() */
#if defined(_FORTRANCAPS)
#define fsion_fwrite FSION_FWRITE

#elif defined(_FORTRANNOCAPS)
#define fsion_fwrite fsion_fwrite

#elif defined(_FORTRANUNDERSCORE)
#define fsion_fwrite fsion_fwrite_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_fwrite fsion_fwrite__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_fwrite(const void *data, 
                    sion_int64 *size, 
                    sion_int64 *nitems, 
                    int *sid, 
                    sion_int64 *rc);

/* sion_read() */
#if defined(_FORTRANCAPS)
#define fsion_read FSION_READ

#elif defined(_FORTRANNOCAPS)
#define fsion_read fsion_read

#elif defined(_FORTRANUNDERSCORE)
#define fsion_read fsion_read_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_read fsion_read__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_read(void *data, 
                  sion_int64 *size, 
                  sion_int64 *nitems, 
                  int *sid, 
                  sion_int64 *rc);

/* sion_fread() */
#if defined(_FORTRANCAPS)
#define fsion_fread FSION_FREAD

#elif defined(_FORTRANNOCAPS)
#define fsion_fread fsion_fread

#elif defined(_FORTRANUNDERSCORE)
#define fsion_fread fsion_fread_

#elif defined(_FORTRANDOUBLEUNDERSCORE)
#define fsion_fread fsion_fread__

#elif defined(_FORTRANNOUNDERSCORE)
#else
#error nothing defined for fortran externals
#endif
void fsion_fread(void *data, 
                   sion_int64 *size, 
                   sion_int64 *nitems, 
                   int *sid, 
                   sion_int64 *rc);


#endif /* SION_FORTRAN_H_ */
