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
#include <time.h>
#include <math.h>

#include "sion.h"


int main(int argc, char** argv)
{
  /* ------------------------------------ */
  /* TEST A: write file with empty blocks */
  /* ------------------------------------ */
  {
#define BUFSIZE 10000
    sion_int32  fsblksize   = 10;
    sion_int64* chunksizes  = NULL;
    int*        globalranks = NULL;
    int         ntasks      = 4;
    int         nfiles      = 1;
    int         sid;
    FILE*       outfp = NULL;
    int         t, j, c;
    char        buffer[BUFSIZE];
    size_t      bwrote;
    long        sum;

    chunksizes = (sion_int64*)malloc(ntasks * sizeof (sion_int64));
    if (chunksizes == NULL) {
      fprintf(stderr,
              "TEST A: cannot allocate chunksizes of size %lu, aborting ...\n",
              (unsigned long)ntasks * sizeof (sion_int64));
      return 1;
    }
    globalranks = (int*)malloc(ntasks * sizeof (int));
    if (globalranks == NULL) {
      fprintf(stderr,
              "TEST A: cannot allocate globalranks size %lu, aborting ...\n",
              (unsigned long)ntasks * sizeof (int));
      return 1;
    }
    for (t = 0; t < ntasks; t++) {
      chunksizes[t]  = 1000 + t * 200;
      globalranks[t] = t;
    }
    sid = sion_open("testA.out", "wb", &ntasks, &nfiles, &chunksizes,
                    &fsblksize, &globalranks, &outfp);
    if (sid >= 0) {
      for (j = 0; j < 2; j++) {
        /* only write on first two tasks */
        for (t = 0; t < 2; t++) {
          memset(buffer, 'A' + t, chunksizes[t]);
          sion_seek(sid, t, SION_CURRENT_BLK, SION_CURRENT_POS);
          /* don't fill the chunk */
          bwrote = sion_fwrite(buffer, 1, chunksizes[t] - 9, sid);
          for (c = 0, sum = 0; c < bwrote; c++) {
            sum = sum + buffer[c];
          }
          printf("on rank %d: bytes_written=%d blocksum=%ld\n", t, (int)bwrote,
                 sum);
        }
      }

      sion_close(sid);
    }
    else {
      fprintf(stderr, "TEST A: sion_open returned %d\n", sid);
    }
    free(chunksizes);
    free(globalranks);
  }

  /* --------------------- */
  /* TEST B: read all data */
  /* --------------------- */
  {
#define BUFSIZE 10000
    sion_int32  fsblksize   = 10;
    sion_int64* chunksizes  = NULL;
    int*        globalranks = NULL;
    int         ntasks      = 4;
    int         nfiles      = 1;
    int         sid;
    FILE*       outfp = NULL;
    int         t, j, c;
    char        buffer[BUFSIZE];
    size_t      bread, btoread;
    long        sum;

    sid = sion_open("testA.out", "rb", &ntasks, &nfiles, &chunksizes,
                    &fsblksize, &globalranks, &outfp);
    if (sid >= 0) {
      for (j = 0; j < 2; j++) {
        for (t = 0; t < ntasks; t++) {
          memset(buffer, '-', chunksizes[t]);
          sion_seek(sid, t, SION_CURRENT_BLK, SION_CURRENT_POS);
          btoread = sion_bytes_avail_in_chunk(sid);
          sum     = 0;
          bread   = 0;
          if (btoread) {
            bread = sion_fread(buffer, 1, btoread, sid);
            for (c = 0; c < bread; c++) {
              sum = sum + buffer[c];
            }
          }
          printf("TEST B read: rank=%d chunksizes=%ld  bread=%d blocksum=%ld\n",
                 t, (long)chunksizes[t], (int)bread, sum);
        }
      }

      sion_close(sid);
    }
    else {
      fprintf(stderr, "TEST B: sion_open returned %d\n", sid);
    }
    free(chunksizes);
    free(globalranks);
  }

  /* ------------------------------------ */
  /* TEST C: seek relative to end of file */
  /* ------------------------------------ */
  {
  #define BUFSIZE 10000
    sion_int32      fsblksize   = 10;
    sion_int64*     chunksizes  = NULL;
    int*            globalranks = NULL;
    int             ntasks      = 4;
    int             nfiles      = 1;
    int             sid         = -1;
    FILE*           outfp       = NULL;
    int             rank        = 0;
    int             iteration   = 0;
    size_t          btoread     = 0;

    int             maxchunks     = 0;
    int             newblocknr    = 0;
    sion_int64      newposinblk   = 0;
    int             poslen        = 9;
    int             posidx        = 0;
    sion_int64      pos           = 0;
    /* Offsets for rank = 1 */
    sion_int64      offsets1[] = { 1, 0, -1, -1190, -1191, -1192, -2381, -2382, -2383 };
    sion_int64*     dummy       = NULL;

    sid = sion_open("testA.out", "rb", &ntasks, &nfiles, &chunksizes,
                    &fsblksize, &globalranks, &outfp);
    if (sid >= 0) {
      for (iteration = 0; iteration < 2; iteration++) {
        rank = 1;
        sion_seek(sid, rank, SION_CURRENT_BLK, SION_CURRENT_POS);

        for (posidx = 0; posidx < poslen; posidx++) {
          pos = offsets1[posidx];
          sion_seek(sid, rank, SION_END_POS, pos);
          sion_get_current_location(sid, &newblocknr, &newposinblk, &maxchunks, &dummy);
          btoread = sion_bytes_avail_in_chunk(sid);
          printf("TEST C: rank = %d, iteration = %d, pos = %lld, newblocknr = %d, newposinblk = %lld, btoread = %zd\n",
                 rank, iteration, pos, newblocknr, newposinblk, btoread);
        }
      }

      sion_close(sid);
    }
    else {
      fprintf(stderr, "TEST C: sion_open returned %d\n", sid);
    }
    free(chunksizes);
    free(globalranks);
  }

  return 0;
}
