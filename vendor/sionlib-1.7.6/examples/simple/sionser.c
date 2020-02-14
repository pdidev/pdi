/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sion.h"

#define FNAMELEN 255

int main(int argc, char** argv)
{
  /* for SIONlib open call */
  sion_int32  fsblksize   = -1;
  sion_int64* chunksizes  = NULL;
  int*        globalranks = NULL;
  int         ntasks      = -1;
  int         nfiles      = 1;

  /* other variables */
  int    sid         = 0;
  int    task        = 0;
  size_t buffer_size = 1024;
  char*  buffer      = NULL;
  size_t bavail      = 0;
  size_t bread       = 0;
  size_t bwrote      = 0;

  /* read */
  ntasks      = 4;
  chunksizes  = (sion_int64*)malloc(ntasks * sizeof (sion_int64));
  globalranks = (int*)malloc(ntasks * sizeof (int));
  buffer      = (char*)malloc(buffer_size);

  for (task = 0; task < ntasks; ++task) {
    chunksizes[task]  = 100;
    globalranks[task] = task;
  }

  memset(buffer, 'A', buffer_size);

  sid = sion_open("serfile.sion", "wb", &ntasks, &nfiles, &chunksizes,
                  &fsblksize, &globalranks, NULL);

  bwrote = sion_fwrite(buffer, 1, buffer_size, sid);
  printf("Wrote %zd bytes\n", bwrote);

  sion_close(sid);

  free(chunksizes);
  chunksizes = NULL;
  free(globalranks);
  globalranks = NULL;
  free(buffer);
  buffer = NULL;

  /* write */
  sid = sion_open("serfile.sion", "rb", &ntasks, &nfiles, &chunksizes,
                  &fsblksize, &globalranks, NULL);

  bavail = sion_bytes_avail_in_chunk(sid);
  buffer = (char*)malloc(bavail);

  bread = sion_fread(buffer, 1, bavail, sid);
  printf("Read %zd bytes\n", bread);

  sion_close(sid);

  free(chunksizes);
  chunksizes = NULL;
  free(globalranks);
  globalranks = NULL;
  free(buffer);
  buffer = NULL;

  return 0;
}
