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
#include <mpi.h>
#include <time.h>
#include <math.h>

#include "sion.h"

int main(int argc, char **argv)
{
  int  rank, size;

  /* -------------------------- */
  /* PROLOG */
  /* -------------------------- */

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* -------------------------- */
  /* TEST A: write a empty file and get info */
  /* -------------------------- */
  {
    sion_int64  chunksize = 100;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    FILE       *fp;
    MPI_Comm    lcomm;
    

    sid = sion_paropen_mpi("testA.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    sion_parclose_mpi(sid);

  }


  /* -------------------------- */
  /* TEST A: write a small file and get info */
  /* -------------------------- */
  {
    sion_int64  chunksize = 1000;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    FILE       *fp;
    MPI_Comm    lcomm;
    int         i;
    char        buffer[1000];

    sion_int64  bytes_written=-1;

    for (i = 0; i < 1000; i++)
      buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("testB.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);

    /* write small first chunk */
    sion_fwrite(buffer,1,40+rank*14,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    /* write full second chunk */
    sion_fwrite(buffer,1,1000,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    /* write half full third chunk */
    sion_fwrite(buffer,1,512,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    /* write small 4th chunk */
    sion_fwrite(buffer,1,40+rank*14,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    sion_parclose_mpi(sid);

  }

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
