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

#include "partest_util.h"

int barrier_after_start(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int barrier_after_malloc(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int barrier_after_open(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int barrier_after_write(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int barrier_after_read(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int barrier_after_close(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int barrier_before_unlink(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int barrier_after_unlink(MPI_Comm comm)
{
  MPI_Barrier(comm);
  return (1);
}

int write_timings ( char *set, int method, double *timings, sion_int64 *stats,
		   _test_communicators *communicators, 
		    _test_options *options, int collective ) {
  char      cbuffer1[2*MAXCHARLEN];
  char      cbuffer2[2*MAXCHARLEN];
  char      cbuffer3[2*MAXCHARLEN];
  char      cbuffer4[2*MAXCHARLEN];

  if(method == TIMINGS_METHOD_WRITE) {
    sprintf(cbuffer1,
	    "timings[%06d] T_STAT           %s %s  b=%10.2f MiB t=%10.6fs #chunks=%ld bw=%10.4f MB/s (%10.4f MiB/s) ion=%d nf=%d\n",
	    communicators->all_rank,set,"WRITE",
	    1.0 * stats[STATS_BYTES_WR_WROTE] / toMiB,
	    timings[TIMINGS_WR_TOTAL],
	    (long) stats[STATS_BYTES_WR_NUM_CHUNKS], 
	    1.0 * stats[STATS_BYTES_WR_WROTE] / toMB / timings[TIMINGS_WR_TOTAL], 
	    1.0 * stats[STATS_BYTES_WR_WROTE] / toMiB / timings[TIMINGS_WR_TOTAL], 
	    communicators->ionode_number,
	    options->numfiles);

    sprintf(cbuffer2,
	    "timings[%06d] T_PHASE          %s %s  create=%.5fs, create_cls=%.5fs, open=%.5fs, write=%.5fs, close=%.5fs, tlog=%.4fs\n",
	    communicators->all_rank,set,"WRITE",
	    timings[TIMINGS_WR_CREATE],timings[TIMINGS_WR_CREATE_CLOSE],timings[TIMINGS_WR_OPEN],timings[TIMINGS_WR_WRITE],timings[TIMINGS_WR_CLOSE],timings[TIMINGS_MSGS]);

    sprintf(cbuffer3,
	    "timings[%06d] T_FILE_BARRIER   %s %s  open=%.4fs, write=%.4fs, close=%.4fs\n",
	    communicators->all_rank,set,"WRITE",
	    timings[TIMINGS_WR_OPEN_BARR_FILE],timings[TIMINGS_WR_WRITE_BARR_FILE],timings[TIMINGS_WR_CLOSE_BARR_FILE]); 

    sprintf(cbuffer4,
	    "timings[%06d] T_GLOBAL_BARRIER %s %s  create=%.4fs, create_cls=%.4fs, open=%.4fs, write=%.4fs, close=%.4fs\n",
	    communicators->all_rank,set,"WRITE",
	    timings[TIMINGS_WR_CREATE_BARR_OPEN],timings[TIMINGS_WR_CREATE_BARR_CLOSE],timings[TIMINGS_WR_OPEN_BARR_FILE],timings[TIMINGS_WR_WRITE_BARR_FILE],timings[TIMINGS_WR_CLOSE_BARR_FILE]); 
  } else {
    sprintf(cbuffer1,
	    "timings[%06d] T_STAT           %s %s  b=%10.2f MiB t=%10.6fs #chunks=%ld bw=%10.4f MB/s (%10.4f MiB/s) ion=%d nf=%d\n",
	    communicators->all_rank,set,"READ",
	    1.0 * stats[STATS_BYTES_RD_READ] / toMiB,
	    timings[TIMINGS_RD_TOTAL],
	    (long) stats[STATS_BYTES_RD_NUM_CHUNKS], 
	    1.0 * stats[STATS_BYTES_RD_READ] / toMB / timings[TIMINGS_RD_TOTAL], 
	    1.0 * stats[STATS_BYTES_RD_READ] / toMiB / timings[TIMINGS_RD_TOTAL], 
	    communicators->ionode_number,
	    options->numfiles);

    sprintf(cbuffer2,
	    "timings[%06d] T_PHASE          %s %s  open=%.5fs, read=%.5fs, close=%.5fs, tlog=%.4fs\n",
	    communicators->all_rank,set,"READ",
	    timings[TIMINGS_RD_OPEN],timings[TIMINGS_RD_READ],timings[TIMINGS_RD_CLOSE],timings[TIMINGS_MSGS]);

    sprintf(cbuffer3,
	    "timings[%06d] T_FILE_BARRIER   %s %s  open=%.4fs, read=%.4fs, close=%.4fs\n",
	    communicators->all_rank,set,"READ",
	    timings[TIMINGS_RD_OPEN_BARR_FILE],timings[TIMINGS_RD_READ_BARR_FILE],timings[TIMINGS_RD_CLOSE_BARR_FILE]); 

    sprintf(cbuffer4,
	    "timings[%06d] T_GLOBAL_BARRIER %s %s  open=%.4fs, read=%.4fs, close=%.4fs\n",
	    communicators->all_rank,set,"READ",
	    timings[TIMINGS_RD_OPEN_BARR_FILE],timings[TIMINGS_RD_READ_BARR_FILE],timings[TIMINGS_RD_CLOSE_BARR_FILE]); 
  }

  if(collective) {
    collective_print_gather(cbuffer1, communicators->work);
    collective_print_gather(cbuffer2, communicators->work);
    collective_print_gather(cbuffer3, communicators->work);
    collective_print_gather(cbuffer4, communicators->work);
  } else {
    fprintf(stderr,"%s", cbuffer1);
    fprintf(stderr,"%s", cbuffer2);
    fprintf(stderr,"%s", cbuffer3);
    fprintf(stderr,"%s", cbuffer4);
  }

  return(1);
}

int collective_print_gather(char *cbuffer, MPI_Comm comm)
{
  int       rank, size, p;
  char     *lbuffer;


  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  if(size*MAXCHARLEN > 2*1024*1024) {
    return(collective_print(cbuffer,comm));
  }

  if(rank==0) {
    lbuffer = (char *) malloc(MAXCHARLEN * size);
    if(!lbuffer) {
      fprintf(stderr,"could allocate buffer of size %d\n",MAXCHARLEN * size);
      MPI_Abort(comm,1);
    }
  }
  else  lbuffer = NULL;
   
  

  MPI_Gather(cbuffer, MAXCHARLEN, MPI_CHAR, lbuffer, MAXCHARLEN, MPI_CHAR, 0, comm);

  if (rank == 0) {

    for (p = 0; p < size; p++) {
      fprintf(stderr, "%s", lbuffer + p * MAXCHARLEN);
    }
  }

  if(rank==0) free(lbuffer);

  return (1);
}

int collective_print(char *cbuffer, MPI_Comm comm)
{
  int       rank, size, p;
  int       dummy = 0;
  char      lbuffer[MAXCHARLEN];
  MPI_Status status;

  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);
  if (rank == 0) {
    fprintf(stderr, "%s", cbuffer);

    for (p = 1; p < size; p++) {
      if (p > 0) {
        MPI_Send(&dummy, 1, MPI_INT, p, WAKEUP, comm);
        MPI_Recv(lbuffer, MAXCHARLEN, MPI_CHAR, p, COLPRINT, comm, &status);
        if (strlen(lbuffer) > 0)
          fprintf(stderr, "%s", lbuffer);
      }
    }

  }
  else {

    MPI_Recv(&dummy, 1, MPI_INT, 0, WAKEUP, comm, &status);
    MPI_Send(cbuffer, MAXCHARLEN, MPI_CHAR, 0, COLPRINT, comm);

  }
  return (1);
}

