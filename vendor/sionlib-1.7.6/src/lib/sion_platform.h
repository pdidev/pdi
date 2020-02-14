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

#ifndef SION_SION_PLATFORM_H
#define SION_SION_PLATFORM_H

/* Blue Gene/P */
#if defined(__bgp__)
#if !defined(_SION_BGP)
#define _SION_BGP
#endif
#if !defined(_BGP)
#define _BGP
#endif

/* Blue Gene/Q */
#elif defined(__bgq__)
#if !defined(_SION_BGQ)
#define _SION_BGQ
#endif
#if !defined(_BGQ)
#define _BGQ
#endif

/* AIX */
#elif defined(_AIX)
#if !defined(_SION_AIX)
#define _SION_AIX
#endif

/* Sun Solaris systems */
#elif defined(__sun) && defined(__SVR4)
#define _SION_SUNSOL

#elif defined(__linux__)
#if !defined(_SION_LINUX)
#define _SION_LINUX
#endif

#endif

#endif
