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

  /* ------------------------------------------------------ */
  /* TEST A: write a empty file, use split communicator     */
  /* ------------------------------------------------------ */
  {
    sion_int64  chunksize = 100;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = -1;
    char       *newfname=NULL;
    int         sid, filenum, lsize,lrank;
    MPI_Comm    lcomm;
    FILE       *fp;

    filenum=rank%4;
    MPI_Comm_split(MPI_COMM_WORLD, filenum, rank, &lcomm);

    MPI_Comm_rank(MPI_COMM_WORLD, &lrank);
    MPI_Comm_size(MPI_COMM_WORLD, &lsize);
    printf("on rank %d: after split of communicator: filenum=%d lrank=%d lsize=%d\n",rank,filenum,lrank,lsize);
    
    sid = sion_paropen_mpi("testA.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) sion_parclose_mpi(sid);

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST A 2 files\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
