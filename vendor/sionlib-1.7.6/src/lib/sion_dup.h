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

#ifndef SION_SION_DUP_H
#define SION_SION_DUP_H

#include <stdint.h>

#include "sion_const.h"
#include "sion_datatypes.h"
#include "sion_filedesc.h"

#ifdef __cplusplus
extern "C" {
#endif

  int _sion_dup(int sid, int mode, int rank, uint64_t key);
  int _sion_dedup( int sid );

  /* internal auxiliary  */
  int _sion_dup_paropen( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc );
  int _sion_dup_paropenmappedmaster( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc );
  int _sion_dup_blocksizes( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc );
  int _sion_dup_all_ds( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc );
  int _sion_dup_keyvalptr( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc );
  int _sion_dup_all_keyvalptr( _sion_filedesc *sion_filedesc, _sion_filedesc *new_filedesc );

#ifdef __cplusplus
}
#endif

#endif


