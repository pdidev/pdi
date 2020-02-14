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

#include "sion.h"

#define FILE_NAME "testA.out"

int main(int argc, char** argv)
{
  {
    int ntasks = 1;
    int nfiles = 1;
    sion_int64 chunksize = 10;
    sion_int64 *chunksizes = &chunksize;
    sion_int32 fsblksize = 10;
    int globalrank =  0;
    int *globalranks = &globalrank;
    FILE *fp = NULL;
    int sid = sion_open(FILE_NAME, "w", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &fp);
    if (sid == -1) {
      fprintf(stderr, "TEST A: sion_open returned -1\n");
      return EXIT_FAILURE;
    }
    sion_fwrite("A", 1, 1, sid);
    int currentchunk, maxchunks;
    sion_int64 currentpos;
    sion_get_current_location(sid, &currentchunk, &currentpos, &maxchunks, &chunksizes);
    printf("currentchunk=%d, currentpos=%ld\n", currentchunk, (long)currentpos);
    sion_fwrite("B", 1, 1, sid);
    sion_get_current_location(sid, &currentchunk, &currentpos, &maxchunks, &chunksizes);
    printf("currentchunk=%d, currentpos=%ld\n", currentchunk, (long)currentpos);
    sion_close(sid);
  }

  {
    int ntasks, nfiles;
    sion_int64 *chunksizes = NULL;
    sion_int32 fsblksize = 10;
    int* globalranks = NULL;
    FILE *fp = NULL;
    int sid = sion_open(FILE_NAME, "r", &ntasks, &nfiles, &chunksizes, &fsblksize, &globalranks, &fp);
    if (sid == -1) {
      fprintf(stderr, "TEST B: sion_open returned -1\n");
      return EXIT_FAILURE;
    }
    char buf[2];
    buf[1] = '\0';
    sion_fread(&buf, 1, 1, sid);
    printf("read value %s\n", buf);
    int currentchunk, maxchunks;
    sion_int64 currentpos;
    sion_get_current_location(sid, &currentchunk, &currentpos, &maxchunks, &chunksizes);
    printf("currentchunk=%d, currentpos=%ld\n", currentchunk, (long)currentpos);
    sion_fread(&buf, 1, 1, sid);
    printf("read value %s\n", buf);
    sion_get_current_location(sid, &currentchunk, &currentpos, &maxchunks, &chunksizes);
    printf("currentchunk=%d, currentpos=%ld\n", currentchunk, (long)currentpos);
    sion_close(sid);
  }

  return EXIT_SUCCESS;
}
