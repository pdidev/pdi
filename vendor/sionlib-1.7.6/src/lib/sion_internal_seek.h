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

#ifndef SION_SION_SEEK_H
#define SION_SION_SEEK_H

#include "sion_const.h"
#include "sion_datatypes.h"
#include "sion_filedesc.h"

int        _sion_seek_on_all_ranks_read( _sion_filedesc *sion_filedesc,
					 int  rank,
					 int  blocknr,
					 sion_int64 posinblk );

int        _sion_seek_on_all_ranks_read_master( _sion_filedesc *sion_filedesc,
						int  rank,
						int  blocknr,
						sion_int64 posinblk );

int        _sion_seek_on_current_rank_read( _sion_filedesc *sion_filedesc,
					    int  rank,
					    int  blocknr,
					    sion_int64 posinblk );

int        _sion_seek_on_all_ranks_read_mapped(_sion_filedesc *sion_filedesc,
					       int  rank,
					       int  blocknr,
					       sion_int64 posinblk);

int        _sion_seek_on_all_ranks_write( _sion_filedesc *sion_filedesc,
					  int  rank,
					  int  blocknr,
					  sion_int64 posinblk );

int        _sion_seek_on_current_rank_write( _sion_filedesc *sion_filedesc,
					     int  rank,
					     int  blocknr,
					     sion_int64 posinblk );

int        _sion_seek_on_all_ranks_write_mapped(_sion_filedesc *sion_filedesc,
						int  rank,
						int  blocknr,
						sion_int64 posinblk);

int _sion_seek_search_abs_pos( _sion_filedesc *sion_filedesc,
			       sion_int64      abspos,
			       int            *newblocknr,
			       sion_int64     *newposinblk );

int _sion_seek_search_end_pos(_sion_filedesc* sion_filedesc,
                              sion_int64      posend,
                              int*            newblocknr,
                              sion_int64*     newposinblk);
#endif
