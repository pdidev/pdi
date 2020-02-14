/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#ifndef PARTEST_H_
#define PARTEST_H_

#include "partest_opts.h"
#include "partest_split_comm.h"

/* Enable or disable the checksum */
#define CHECKSUM
#define MB *1024*1024
#define MAXPE 28*64*1024
#define MAXCHARLEN 350


int test_paropen_multi_mpi (char *filename,
			    char *localbuffer,
                            _test_communicators *communicators,
			    _test_options *options
);

int test_mpiio_multi_mpi (char *filename,
			  char *localbuffer,
                          _test_communicators *communicators,
			  _test_options *options
			  );

int test_single_mpi (char *filename,
                     char *localbuffer,
                     _test_communicators *communicators,
		     _test_options *options
                     );

#endif /* PARTEST_H_ */
