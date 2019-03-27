/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

#include "sion.h"

#define FNAMELEN 255
#define BUFSIZE (1024 * 1024)

int main(int argc, char** argv)
{
  /* for SIONlib open call */
  char       fname[FNAMELEN];
  int        numFiles = 0;
  MPI_Comm   gComm, lComm;
  sion_int64 chunksize  = 0;
  sion_int32 fsblksize  = 0;
  int        globalrank = 0;
  FILE*      fileptr    = NULL;
  char*      newfname   = NULL;

  /* other variables */
  int        rank        = 0;
  int        size        = 0;
  int        sid         = 0;
  sion_int64 left        = 0;
  size_t     btoread     = 0;
  size_t     bread       = 0;
  size_t     bwrote      = 0;
  char*      localbuffer = NULL;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* allocate and initalize a buffer  */
  localbuffer = (char*)malloc(BUFSIZE);

  memset(localbuffer, 'A', BUFSIZE);

  /* inital parameters */
  strcpy(fname, "parfile.sion");
  numFiles   = 1;
  gComm      = lComm = MPI_COMM_WORLD;
  chunksize  = 10 * 1024 * 1024;
  fsblksize  = -1;
  globalrank = rank;
  fileptr    = NULL;

  /* write */
  sid = sion_paropen_mpi(fname, "bw", &numFiles, gComm, &lComm, &chunksize,
                         &fsblksize, &globalrank, &fileptr, &newfname);
  left   = BUFSIZE;
  bwrote = sion_fwrite(localbuffer, 1, left, sid);
  printf("Task %02d: wrote bytes: %zd\n", rank, bwrote);
  sion_parclose_mpi(sid);
  printf("Task %02d: wrote sionfile -> %s\n", rank, newfname);


  /* read */
  sid = sion_paropen_mpi(fname, "br", &numFiles, gComm, &lComm, &chunksize,
                         &fsblksize, &globalrank, &fileptr, &newfname);
  while (!sion_feof(sid)) {
    btoread = sion_bytes_avail_in_chunk(sid);
    bread   = sion_fread(localbuffer, 1, btoread, sid);
    printf("Task %02d: read bytes: %zd\n", rank, bread);
  }
  sion_parclose_mpi(sid);
  printf("Task %02d: read sionfile -> %s\n", rank, newfname);

  MPI_Finalize();

  return 0;
}
