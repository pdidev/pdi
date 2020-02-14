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

#ifndef SION_SION_LOCK_H
#define SION_SION_LOCK_H

#include "sion_const.h"

#ifdef __cplusplus
extern "C" {
#endif

  int _sion_lock_register_lock_callbacks(int lock(void *), int unlock(void *), void *lockdata);
  int _sion_lock_user_callbacks_defined(void);
  int _sion_lock(void);
  int _sion_unlock(void);
 
#ifdef __cplusplus
}
#endif

#endif
