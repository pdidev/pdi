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

#ifndef SION_SION_COLLSTAT_H
#define SION_SION_COLLSTAT_H

#include "sion_const.h"
#include "sion_datatypes.h"
#include "sion_filedesc.h"

struct _sion_collstat_struct {
  int        req_num_collectors;
  int        req_collsize;
  int        num_collectors;
  int        min_sender_per_collector;
  int        max_sender_per_collector;
  sion_int64 firstsize;
  sion_int64 gsize;
  sion_int64 min_size_per_collector;
  sion_int64 max_size_per_collector;
  sion_int64 min_size_per_sender;
  sion_int64 max_size_per_sender;
  sion_int64 avg_data_per_collector;
  double avg_sender_per_collector;
  double avg_size_per_collector;
  double avg_size_per_sender;
};
typedef struct _sion_collstat_struct _sion_collstat;

_sion_collstat * _sion_create_and_init_collstat( _sion_filedesc *sion_filedesc );
int _sion_update_collstat( _sion_collstat *collstat, _sion_filedesc *sion_filedesc );
int _sion_print_collstat( _sion_collstat *sion_collstat, _sion_filedesc *sion_filedesc );
int _sion_destroy_collstat( _sion_collstat *sion_collstat );
int _sion_debugprint_collstat( _sion_collstat *collstat, _sion_filedesc *sion_filedesc );

#endif
