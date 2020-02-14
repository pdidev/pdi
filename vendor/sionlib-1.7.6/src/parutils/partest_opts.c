/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <mpi.h>
#include <time.h>
#include <math.h>
#ifdef _SION_AIX
#include "getopt_long.h"
#else
#include <getopt.h>
#endif
#include "sion.h"
#include "partest_opts.h"
#include "partest.h"

int init_options ( _test_options *options) {
  int rc=0;

  options->type                    = 0;
  options->bluegene                = 0;
  options->bluegene_np             = 0;
  options->bluegene_sort           = 0;
  options->bufsize                 = 10 MB;
  options->totalsize               = 20 MB;
  options->chunksize               = options->totalsize;
  options->fsblksize               = 2.0 MB;
  options->verbose                 = 0;
  options->debug                   = 0;
  options->Debug                   = 0;
  options->numfiles                = 1;
  options->startoffset             = 0;
  options->read_task_offset        = 0;
  options->collectiveopenforread   = 1;
  options->collectivewrite         = 0;
  options->collectiveread          = 0;
  options->collmsa                 = 0;
  options->unlink_files            = 0;
  options->suppress_checksum       = 0;
  options->serialize_blocknum      = -1;
  options->mpiio_lb                = -1;
  options->mpiio_bs                = -1;
  options->mpiio_sa                = -1;
  options->factor                  = 0.0;

  options->do_write                = 1;
  options->do_read                 = 1;
  options->use_posix               = 0;

  options->globalsize              = -1;

  strcpy(options->filename, "partest_parfile.sion");

  return(rc);
}

int parse_options_std ( int argc, char **argv, _test_options *options) {
  int rc=0;
  int i;

  /* parse command line */
  i = 1;
  while (i < argc) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
      case 'f':
        strcpy(options->filename, argv[++i]);
        break;
      case 'F':
        options->factor = atof(argv[++i]);
        break;
      case 'n':
        options->numfiles = atoi(argv[++i]);
        break;
      case 'b':
        options->bufsize = (sion_int64) atoi(argv[++i]);
        break;
      case 'B':
        options->bufsize = (sion_int64) atoi(argv[++i]) MB;
        break;
      case 'g':
        options->globalsize = (sion_int64) atoi(argv[++i]);
        break;
      case 'G':
        options->globalsize = (sion_int64) atoi(argv[++i]) * 1024 MB;
        break;
      case 'I':
        options->do_write = 0;
        break;
      case 'O':
        options->do_read = 0;
        break;
      case 'L':
        options->use_posix = 1;
        break;
      case 's':
        options->totalsize = (sion_int64) atoi(argv[++i]);
        break;
      case 'S':
        options->totalsize = (sion_int64) atoi(argv[++i]) MB;
        break;
      case 'r':
        options->chunksize = (sion_int64) atoi(argv[++i]);
        break;
      case 'R':
        options->chunksize = (sion_int64) atoi(argv[++i]) MB;
        break;
      case 'q':
        options->fsblksize = atoi(argv[++i]);
        break;
      case 'Q':
        options->fsblksize = atof(argv[++i]) MB;
        break;
      case 'o':
        options->startoffset = (sion_int64) atoi(argv[++i]);
        break;
      case 'P':
        options->bluegene = 1;
        break;
      case 'p':
        options->bluegene_np = atoi(argv[++i]);
        break;
      case 'T':
        options->type = atoi(argv[++i]);
        break;
      case 'w':
        options->mpiio_lb = atoi(argv[++i]);
        break;
      case 'W':
        options->mpiio_bs = atoi(argv[++i]);
        break;
      case 'x':
        options->mpiio_sa = atoi(argv[++i]);
        break;
      case 'Z':
        options->read_task_offset = atoi(argv[++i]);
        break;
      case 'X':
        options->unlink_files = atoi(argv[++i]);
        break;
      case 'j':
        options->serialize_blocknum = atoi(argv[++i]);
        break;
      case 'C':
        options->suppress_checksum = 1;
        break;
      case 'M':
        options->collectivewrite++;
        break;
      case 'm':
        options->collectiveread++;
        break;
      case 'c':
        options->collmsa = 1;
        break;
      case 'd':
        options->debug++;
        break;
      case 'D':
        options->Debug++;
        break;
      case 'v':
        options->verbose++;
        break;
      default:
        printf("Arg default: %s\n", argv[i]);
        usage(argv[0]);
      }
    }
    else {
      printf("Arg error: %s\n", argv[i]);
      usage(argv[0]);
    }
    i++;
  }

  return(rc);
}


int parse_options_long ( int argc, char **argv, _test_options *options) {
  int rc=1;
  int c;
  int option_index = 0;
  static struct option long_options[] = {
    {"filename",  required_argument, 0, 'f'},
    {"numfiles",  required_argument, 0, 'n'},
    {"chunksize", required_argument, 0, 'r'},
    {"fsblksize", required_argument, 0, 'q'},
    {"testtype",  required_argument, 0, 'T'},
    {"bufsize",   required_argument, 0, 'b'},
    {"totalsize", required_argument, 0, 'g'},
    {"localsize", required_argument, 0, 's'},
    {"factor",    required_argument, 0, 'F'},
    {"write",     required_argument, 0, 'W'},
    {"read",      required_argument, 0, 'R'},

    {"verbose",   required_argument, 0, 'v'},
    {"nochecksum",required_argument, 0, 'C'},
    {"debugtask", required_argument, 0, 'd'},
    {"Debugtask", required_argument, 0, 'D'},

    {"posix",     required_argument, 0, 'L'},
    {"collwrite", required_argument, 0, 'M'},
    {"collread",  required_argument, 0, 'm'},
    {"collmsa",   required_argument, 0, 'c'},

    {"taskoffset",required_argument, 0, 'Z'},
    {"byteoffset",required_argument, 0, 'O'},
    {"serialized",required_argument, 0, 'j'},
    {"unlinkfiles",required_argument, 0, 'X'},
    {"bgionode",         required_argument, 0, 'P'},
    {"bgtaskperionode",  required_argument, 0, 'p'},
    {"bgtasksort",       required_argument, 0, 'Y'},
    {"hintlargeblock",   required_argument, 0, 'w'},
    {"hintiobufsize",    required_argument, 0, 'Q'},
    {"hintsparseacess",  required_argument, 0, 'x'},


    {0, 0, 0, 0}
  };

  while(1) {
    c = getopt_long(argc, argv, "f:n:r:q:T:b:g:s:F:W:R:vCdDLMmcZ:X:O:j:wQ:x",
                    long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 0:
      printf("option %s", long_options[option_index].name);
      if (optarg)
        printf(" with arg %s", optarg);
      printf("\n");
      break;
    case 'f':
      strcpy(options->filename, optarg);
      break;
    case 'n':
      options->numfiles = atoi(optarg);
      break;
    case 'r':
      options->chunksize = to_bytes(optarg);
      break;
    case 'q':
      options->fsblksize = to_bytes(optarg);
      break;

    case 'T':
      options->type = atoi(optarg);
      break;
    case 'b':
      options->bufsize = to_bytes(optarg);
      break;
    case 'g':
      options->globalsize = to_bytes(optarg);
      break;
    case 's':
      options->totalsize = to_bytes(optarg);
      break;
    case 'F':
      options->factor = atoi(optarg);
      break;
    case 'W':
      options->do_write = atoi(optarg);
      break;
    case 'R':
      options->do_read = atoi(optarg);
      break;

    case 'v':
      if(optarg) {
        options->verbose = atoi(optarg);
      } else options->verbose = 1;

      break;

    case 'C':
      if(optarg) {
        options->suppress_checksum = atoi(optarg);
      } else options->suppress_checksum = 1;
      break;

    case 'd':
      if(optarg) {
        options->debug = atoi(optarg);
      } else options->debug = 1;
      break;

    case 'D':
      if(optarg) {
        options->Debug = atoi(optarg);
      } else options->Debug = 1;
      break;

    case 'L':
      if(optarg) {
        options->use_posix = atoi(optarg);
      } else options->use_posix = 1;
      break;

    case 'M':
      if(optarg) {
        options->collectivewrite = atoi(optarg);
      } else options->collectivewrite = 1;
      break;

    case 'm':
      if(optarg) {
        options->collectiveread = atoi(optarg);
      } else options->collectiveread = 1;
      break;

    case 'c':
      options->collmsa = 1;
      break;

    case 'Z':
      options->read_task_offset = atoi(optarg);
      break;

    case 'O':
      options->startoffset = atoi(optarg);
      break;

    case 'j':
      options->serialize_blocknum = atoi(optarg);
      break;

    case 'X':
      if(optarg) {
        options->unlink_files = atoi(optarg);
      } else options->unlink_files = 1;
      break;

    case 'P':
      if(optarg) {
        options->bluegene = atoi(optarg);
      } else options->bluegene = 1;
      break;

    case 'p':
      options->bluegene_np = atoi(optarg);
      break;

    case 'Y':
      options->bluegene_sort = atoi(optarg);
      break;

    case 'w':
      if(optarg) {
        options->mpiio_lb = atoi(optarg);
      } else options->mpiio_lb = 1;
      break;

    case 'Q':
      options->mpiio_bs = atoi(optarg);
      break;

    case 'x':
      if(optarg) {
        options->mpiio_sa = atoi(optarg);
      } else options->mpiio_sa = 1;
      break;

    default:
      printf("?? getopt_long returned character code 0%o ??\n", c);
      rc=0;
      return(rc);
    }
  }

  return(rc);
}

int distribute_options_mpi ( _test_options *options) {
  int rc=0;

  MPI_Bcast(options->filename, FNAMELEN, MPI_CHAR, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->factor, 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);

  MPI_Bcast(&options->bufsize, 1, SION_MPI_INT64, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->totalsize, 1, SION_MPI_INT64, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->globalsize, 1, SION_MPI_INT64, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->chunksize, 1, SION_MPI_INT64, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->fsblksize, 1, SION_MPI_INT32, 0, MPI_COMM_WORLD);

  MPI_Bcast(&options->type, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->verbose, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->bluegene, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->bluegene_np, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->unlink_files, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->serialize_blocknum, 1, MPI_INT, 0, MPI_COMM_WORLD);

  MPI_Bcast(&options->collectivewrite, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->collectiveread, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->collmsa, 1, MPI_INT, 0, MPI_COMM_WORLD);

  MPI_Bcast(&options->numfiles, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->debug, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->Debug, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->read_task_offset, 1, MPI_INT, 0, MPI_COMM_WORLD);

  MPI_Bcast(&options->suppress_checksum, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->do_read, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->do_write, 1, MPI_INT, 0, MPI_COMM_WORLD);
  MPI_Bcast(&options->use_posix, 1, MPI_INT, 0, MPI_COMM_WORLD);

  return(rc);
}


void usage_long(char *name) {

  fprintf(stderr, "Usage: %s options\n\n", name);
  fprintf(stderr, "Options:\n");

  fprintf(stderr, " Sion File Settings:\n");
  fprintf(stderr, "  [-f filename]          (--filename[=])     filename of direct access file\n");
  fprintf(stderr, "  [-n <number of files>] (--numfiles[=])     number of files (def: 1)\n");
  fprintf(stderr, "  [-r <chunksize>]       (--chunksize[=])    sion chunk size (see size format)\n");
  fprintf(stderr, "  [-q <fsblksize>]       (--fsblksize[=])    size of filesystem blocks (see\n");
  fprintf(stderr, "                                             size format)\n");

  fprintf(stderr, " Test Configuration:\n");
  fprintf(stderr, "  [-T <type>]            (--testtype[=])     type of test\n");
  fprintf(stderr, "                                             (0:SION, standard),\n");
  fprintf(stderr, "                                             (1: SION:, independant read),\n");
  fprintf(stderr, "                                             (2:MPI IO), (3: Task-Local-File)\n");
  fprintf(stderr, "  [-b <bufsize>]         (--bufsize[=])      size of blocks written in ONE\n");
  fprintf(stderr, "                                             fwrite (see size format)\n");
  fprintf(stderr, "  [-g <totalsize>]       (--totalsize[=])    global total size of data written\n");
  fprintf(stderr, "                                             (see size format)\n");
  fprintf(stderr, "  [-s <localsize>]       (--localsize[=])    local size of data written by each\n");
  fprintf(stderr, "                                             processor (see size format)\n");
  fprintf(stderr, "  [-F <factor>]          (--factor[=])       factor for random size\n");
  fprintf(stderr, "                                             (0.0 to 1.0, def: 0.0)\n");
  fprintf(stderr, "  [-R (0|1)]             (--read[=])         switch read  off/on\n");
  fprintf(stderr, "  [-W (0|1|2)]           (--write[=])        switch write off, on, or double\n");
  fprintf(stderr, "                                             write\n");

  fprintf(stderr, " Special Test Options:\n");
  fprintf(stderr, "  [-v]                   (--verbose[=](0|1))      verbose print info for each\n");
  fprintf(stderr, "                                                  task\n");
  fprintf(stderr, "  [-C]                   (--nochecksum[=](0|1))   suppress checksum\n");
  fprintf(stderr, "  [-d]                   (--debugtask[=](0|1))    debug task 0\n");
  fprintf(stderr, "  [-D]                   (--Debugtask[=](0|1))    debug task n\n");
  fprintf(stderr, "  [-L]                   (--posix[=](0|1))        use POSIX calls instead of\n");
  fprintf(stderr, "                                                  ANSI calls\n");
  fprintf(stderr, "  [-M]                   (--collwrite[=](0|1))    use collective write if\n");
  fprintf(stderr, "                                                  possible\n");
  fprintf(stderr, "  [-m]                   (--collread[=](0|1))     use collective read if\n");
  fprintf(stderr, "                                                  possible\n");
  fprintf(stderr, "  [-c]                   (--collmsa)              optimize collective I/O for\n");
  fprintf(stderr, "                                                  modular supercomputers\n");
  fprintf(stderr, "  [-Z <offset>]          (--taskoffset[=])        shift tasks numbering for\n");
  fprintf(stderr, "                                                  reading by offset to prevent\n");
  fprintf(stderr, "                                                  data caching of file-system\n");
  fprintf(stderr, "                                                  (default: 0)\n");
  fprintf(stderr, "  [-O <bytes>]           (--byteoffset[=])        start offset, write <bytes>\n");
  fprintf(stderr, "                                                  first before using blksize\n");
  fprintf(stderr, "                                                  (default: 0)\n");
  fprintf(stderr, "  [-j <#tasks>]          (--serialized[=])        serialize I/O, only I/O of\n");
  fprintf(stderr, "                                                  #tasks are running in parallel\n");
  fprintf(stderr, "                                                  (-1 -> all tasks in parallel,\n");
  fprintf(stderr, "                                                  -2 -> use transactions,\n");
  fprintf(stderr, "                                                  -def: -1)\n");
  fprintf(stderr, "  [-X]                   (--unlinkfiles[=](0|1))  remove files after test\n");

  fprintf(stderr, "  Blue Gene/L, Blue Gene/P , Blue Gene/Q:\n");
  fprintf(stderr, "  [-P <mode>]            (--bgionode[=](0|1|2))   order tasks by BG I/O-node\n");
  fprintf(stderr, "                                                  (0 none, 1 ION, 2 IOB)\n");
  fprintf(stderr, "  [-Y <sort>]            (--bgtaskperionode[=])   number of tasks per BG\n");
  fprintf(stderr, "                                                  I/O-node\n");
  fprintf(stderr, "  [-p <numtasks>]        (--bgtasksort[=](0|1))   sort task inside local\n");
  fprintf(stderr, "                                                  communicator (0 distance to\n");
  fprintf(stderr, "                                                  ION, 1 global rank)\n");

  fprintf(stderr, "  MPI-IO, GPFS options:\n");
  fprintf(stderr, "  [-w]                   (--hintlargeblock[=](0|1))  Hint MPI-IO, IBM, Large\n");
  fprintf(stderr, "                                                     Block IO\n");
  fprintf(stderr, "  [-Q <size>]            (--hintiobufsize[=])        Hint MPI-IO, IBM, IO\n");
  fprintf(stderr, "                                                     bufsize in KB\n");
  fprintf(stderr, "  [-x]                   (--hintsparseacess[=](0|1)) Hint MPI-IO, IBM, sparse\n");
  fprintf(stderr, "                                                     access\n");

  fprintf(stderr, " Size Formats: <d>[k, K, kib, kiB, Kib, KiB] for kibi bytes\n");
  fprintf(stderr, "               <d>[kb, kB, Kb, KB] for kilo bytes\n");
  fprintf(stderr, "               similarly for [M, G, T]\n");

}

void usage(char *name) {

  fprintf(stderr, "Usage: %s options\n\n", name);
  fprintf(stderr, "Options:\n\n");

  fprintf(stderr, " Sion File Settings:\n");
  fprintf(stderr, "  [-f filename]          filename of direct access file\n");
  fprintf(stderr, "  [-n <number of files>] number of files\n");
  fprintf(stderr, "  [-r <chunksize>]       sion chunk size in bytes\n");
  fprintf(stderr, "  [-R <chunksize>]       sion chunk size in MBytes\n");
  fprintf(stderr, "  [-q <fsblksize>]       size of filesystem blocks in bytes\n");
  fprintf(stderr, "  [-Q <fsblksize>]       size of filesystem blocks in MBytes\n");

  fprintf(stderr, " Test Configuration:\n");
  fprintf(stderr, "  [-F <factor>]          factor for random size (0.0 to 1.0, def: 0.0)\n");
  fprintf(stderr, "  [-b <bufsize>]         size of blocks written in ONE fwrite in bytes\n");
  fprintf(stderr, "  [-B <bufsize>]         size of blocks written in ONE fwrite in MBytes\n");
  fprintf(stderr, "  [-g <globaltotalsize>] global total size of data written in bytes\n");
  fprintf(stderr, "  [-G <globaltotalsize>] global total size of data written in GBytes\n");
  fprintf(stderr, "  [-s <totalsize>]       total size of data written by each processor in bytes\n");
  fprintf(stderr, "  [-S <totalsize>]       total size of data written by each processor in MBytes\n");
  fprintf(stderr, "  [-T <type>]            type of test (0): w/o collective read; (2): MPI IO\n");
  fprintf(stderr, "  [-v]                   verbose print info for each task\n");
  fprintf(stderr, "  [-C]                   suppress checksum\n");
  fprintf(stderr, "  [-I]                   only read data\n");
  fprintf(stderr, "  [-O]                   only write data\n");
  fprintf(stderr, "  [-d]                   debug task 0\n");
  fprintf(stderr, "  [-D]                   debug task n\n");
  fprintf(stderr, "  [-L]                   use POSIX calls instead of ANSI call\n");
  fprintf(stderr, "  [-M]                   use collective write if possible\n");
  fprintf(stderr, "  [-m]                   use collective read if possible\n");
  fprintf(stderr, "  [-c]                   optimize collective I/O for modular supercomputers\n");
  fprintf(stderr, "  [-Z <offset>]          shift tasks numbering for reading by offset to ommit\n");
  fprintf(stderr, "                         data caching of file-system (default: 0)\n");
  fprintf(stderr, "  [-o <bytes>]           start offset, write <bytes> first before using blksize\n");
  fprintf(stderr, "                         (default: 0)\n");
  fprintf(stderr, "  [-j <#tasks>]          serialize I/O, only I/O of #tasks are running in\n");
  fprintf(stderr, "                         parallel (-1 -> all tasks in parallel, -2 -> use\n");
  fprintf(stderr, "                         transactions, def: -1)\n");

  fprintf(stderr, "  Blue Gene/L, Blue Gene/P , Blue Gene/Q:\n");
  fprintf(stderr, "  [-P <mode>]            order tasks by BG I/O-node (0 none, 1 ION, 2 IOB)\n");
  fprintf(stderr, "  [-Y <sort>]            number of tasks per BG I/O-node\n");
  fprintf(stderr, "  [-p <numtasks>]        number of tasks per BG I/O-node\n");

  fprintf(stderr, "  MPI-IO, GPFS options:\n");
  fprintf(stderr, "  [-w <1|0>]             Hint MPI-IO, IBM, Large Block IO\n");
  fprintf(stderr, "  [-W <size>]            Hint MPI-IO, IBM, IO bufsize in KB\n");
  fprintf(stderr, "  [-x <1|0>]             Hint MPI-IO, IBM, sparse access\n");

  exit(1);

}


/* convert following formats to bytes:
   t,T,Ti,Tib,TiB -> 1024*1024*1024*1024 bytes
   g,G,Gi,Gib,GiB ->      1024*1024*1024 bytes
   m,M,Mi,Mib,MiB ->           1024*1024 bytes
   k,K,Ki,Kib,KiB ->                1024 bytes

   Tb,TB   -> 1000*1000*1000*1000 bytes
   Gb,GB   ->      1000*1000*1000 bytes
   Mb,MB   ->           1000*1000 bytes
   Kb,KB   ->                1000 bytes
*/
sion_int64 to_bytes ( char *option) {
      int rc;
      sion_int64 size = -1;
      sion_int64 factor = 1024;
      char char1='\0',char2='\0',char3='\0';
      rc = sscanf(option, "%lld%c%c%c", &size, &char1,&char2,&char3);
      if (rc == 3 && ( (char2=='b') || (char2=='B') )) {
        factor = 1000;
      }

      if ( (char1=='T') || (char1=='t') ) size*=factor*factor*factor*factor;
      if ( (char1=='G') || (char1=='g') ) size*=factor*factor*factor;
      if ( (char1=='M') || (char1=='m') ) size*=factor*factor;
      if ( (char1=='K') || (char1=='k') ) size*=factor;

      return size;
}
