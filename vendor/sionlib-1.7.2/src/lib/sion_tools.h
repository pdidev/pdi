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

#ifndef SION_SION_TOOLS_H
#define SION_SION_TOOLS_H

#include <stdio.h>

#include "sion_filedesc.h"

#ifdef __cplusplus
extern "C" {
#endif

  FILE              *sion_get_fp(int sid);
  _sion_filedesc    *_sion_get_filedesc(int sid);

 
#ifdef __cplusplus
}
#endif

#endif
