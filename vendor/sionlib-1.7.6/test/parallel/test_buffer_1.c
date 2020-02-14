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

  /* ------------------------------------------------------- */
  /* TEST B: write a  file with some chunks < fs block size  */
  /* ------------------------------------------------------- */
  {
#define BUFSIZE 100 
#define CHUNKSIZE 1000
    sion_int64  chunksize = CHUNKSIZE;
    sion_int32  fsblksize = 1000;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    MPI_Comm    lcomm;
    FILE       *fp;
    char        buffer[BUFSIZE];
    sion_int64  bytes_written=0;
    long        sum; 
    size_t      bwrote;
    int         i;

    for (i = 0; i < BUFSIZE; i++) buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("testB.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);
    if(sid>=0) {
      bytes_written=0;
      while(bytes_written<chunksize) { 
	bwrote=sion_fwrite(buffer,1,BUFSIZE,sid);
	bytes_written+=bwrote;
	printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);
	for (i = 0,sum=0; i < bwrote; i++) sum=sum+buffer[i];
	printf("on rank %d: bwrote=%3d blocksum=%8d \n",rank, (int) bwrote,(int) sum);
      }
      sion_parclose_mpi(sid);
    } else {
      fprintf(stderr, "on rank %d: error sid = %d\n",rank,sid);

    }
  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST B write\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);



  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST B read collective\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();

  return(0);
}
