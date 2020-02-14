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

#ifndef nott
  /* -------------------------- */
  /* TEST A: test with wrong parameters */
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

    /* lcomm=NULL */
    sid = sion_paropen_mpi("testA.out", "bw", &numfiles, MPI_COMM_WORLD, NULL, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) sion_parclose_mpi(sid);

    /* numfiles=NULL */
    sid = sion_paropen_mpi("testA.out", "bw", NULL, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0)sion_parclose_mpi(sid);

  }
  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST A\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* TEST B: check correct return data when reading file */
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

    sion_int64  Rchunksize = -1;
    sion_int32  Rfsblksize = -1;
    int         Rglobalrank= -1;
    int         Rnumfiles  = -1;
    char       *Rnewfname=NULL;
    MPI_Comm    Rlcomm;

    /* lcomm=NULL */
    sid = sion_paropen_mpi("testB.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) sion_parclose_mpi(sid);

    /* numfiles=NULL */
    sid = sion_paropen_mpi("testB.out", "br", &numfiles, MPI_COMM_WORLD, &Rlcomm, &Rchunksize, &Rfsblksize, &Rglobalrank, &fp, &Rnewfname);
    if(sid>=0) sion_parclose_mpi(sid);

    printf("on rank %d: numfiles       =      %d , %d\n",rank,numfiles,Rnumfiles);
    printf("on rank %d: chunksize      =      %d , %d\n",rank,(int) chunksize, (int) Rchunksize);
    printf("on rank %d: globalrank     =      %d , %d\n",rank,(int) globalrank, (int) Rglobalrank);
    printf("on rank %d: newfname       =      %s , %s\n",rank,newfname, Rnewfname);
    

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST B\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#endif
  /* -------------------------- */
  /* TEST C: check correct return data when reading file, 4 files */
  /* -------------------------- */
  {
    sion_int64  chunksize = 100;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 4;
    char       *newfname=NULL;
    int         sid;
    FILE       *fp;
    MPI_Comm    lcomm;

    sion_int64  Rchunksize = -1;
    sion_int32  Rfsblksize = -1;
    int         Rglobalrank= -1;
    int         Rnumfiles  = -1;
    char       *Rnewfname=NULL;
    MPI_Comm    Rlcomm;

    /* lcomm=NULL */
    sid = sion_paropen_mpi("testC.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) sion_parclose_mpi(sid);

    /* numfiles=NULL */
    sid = sion_paropen_mpi("testC.out", "br", &Rnumfiles, MPI_COMM_WORLD, &Rlcomm, &Rchunksize, &Rfsblksize, &Rglobalrank, &fp, &Rnewfname);
    if(sid>=0) sion_parclose_mpi(sid);

    printf("on rank %d: numfiles       =      %d , %d\n",rank,numfiles,Rnumfiles);
    printf("on rank %d: chunksize      =      %d , %d\n",rank,(int) chunksize, (int) Rchunksize);
    printf("on rank %d: globalrank     =      %d , %d\n",rank,(int) globalrank, (int) Rglobalrank);
    printf("on rank %d: newfname       =      %s , %s\n",rank,newfname, Rnewfname);
    

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST C\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
