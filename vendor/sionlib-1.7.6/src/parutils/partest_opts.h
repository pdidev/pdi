/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#ifndef PARTEST_OPTS_H_
#define PARTEST_OPTS_H_

#define FNAMELEN 255

struct _test_options_struct {
  sion_int64  bufsize;
  sion_int64  totalsize;
  sion_int64  globalsize;
  sion_int64  chunksize;
  sion_int32  fsblksize;
  int         type;
  int         bluegene;
  int         bluegene_np;
  int         bluegene_sort;
  int         verbose;
  int         debug;
  int         Debug;
  int         numfiles;
  int         startoffset;
  int         read_task_offset;
  int         collectiveopenforread;
  int         collectiveread;
  int         collectivewrite;
  int         collmsa;
  int         unlink_files;
  int         suppress_checksum;
  int         serialize_blocknum;
  int         mpiio_lb;
  int         mpiio_bs;
  int         mpiio_sa;           
  double      factor;           
  int         do_read;           
  int         do_write;           
  int         use_posix;           
  char        filename[FNAMELEN];
};

typedef struct _test_options_struct _test_options;


int init_options ( _test_options *options);
int parse_options_std ( int argc, char **argv, _test_options *options);
int parse_options_long ( int argc, char **argv, _test_options *options);
int distribute_options_mpi ( _test_options *options);
void usage(char *name);
void usage_long(char *name);
sion_int64 to_bytes ( char *option);
#endif /* PARTEST_OPTS_H_ */
