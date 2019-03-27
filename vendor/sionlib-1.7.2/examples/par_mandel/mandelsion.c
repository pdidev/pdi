#include <mpi.h>
#include <stdlib.h>

#include "mandelsion.h"
#include "infostruct.h"
#include "sion.h"

/* write block to SION file */
void write_to_sion_file(int *sid, const _infostruct *infostruct, const int *iterations, int width, int height,
                        int xpos, int ypos)
{
  _pos_struct pos_struct;

  if (width * height > 0) {
    pos_struct.width = width;
    pos_struct.height = height;
    pos_struct.xpos = xpos;
    pos_struct.ypos = ypos;

    sion_fwrite(&pos_struct, sizeof(pos_struct), 1, *sid);
    sion_fwrite(iterations, sizeof(int), width * height, *sid);
  }
}

/* open SION file */
void open_sion(int *sid, const _infostruct *infostruct, const int *blocksize, const int *start, int rank)
{
  int numFiles = 1;
  int fsblksize = -1;
  sion_int64 chunksize;
  MPI_Comm lComm;

  chunksize = blocksize[0] * blocksize[1] * sizeof(int) + sizeof(_pos_struct);
  if (rank == 0) {
    if (infostruct->type == 2) {
      chunksize = 0;
    }
    if (infostruct->type == 3) {
      chunksize = infostruct->width * infostruct->height * sizeof(int) + sizeof(_pos_struct);
    }
    chunksize += sizeof(*infostruct);
  } else {
    if (infostruct->type == 3) {
      chunksize = 0;
    }
  }

  *sid = sion_paropen_mpi("simple.sion", "w", &numFiles, MPI_COMM_WORLD, &lComm, &chunksize, &fsblksize,
                          &rank, NULL, NULL);

  /* write global file header */
  if (rank == 0) {
    sion_fwrite(infostruct, sizeof(*infostruct), 1, *sid);
  }
}

/* close SION file */
void close_sion(int *sid, const _infostruct *infostruct, int rank)
{
    sion_parclose_mpi(*sid);
}

/* read SION file */
void collect_sion(int **iterations, int **proc_distribution, _infostruct *infostruct)
{
  sion_int32  fsblksize   = -1;
  sion_int64* chunksizes  = NULL;
  int*        globalranks = NULL;
  int         ntasks      = 1;
  int         nfiles      = 1;
  int         outsid      = -1;

  int*        buffer = NULL;
  _pos_struct info;
  int         task = 0;
  int         i    = 0;
  int         size = -1;
  int         xpos,ypos;

  /* open SION file */
  outsid = sion_open("simple.sion", "r", &ntasks, &nfiles, &chunksizes,
                     &fsblksize, &globalranks, NULL);

  /* read global header*/
  sion_seek(outsid, 0, SION_CURRENT_BLK, SION_CURRENT_POS);
  sion_fread(infostruct, sizeof(_infostruct), 1, outsid);

  size               = infostruct->width * infostruct->height;
  *iterations        = malloc(size * sizeof(int));
  *proc_distribution = malloc(size * sizeof(int));

  /* ignore Master-task when using master-worker scheme */
  if (infostruct->type == 2) {
    task = 1;
  }
  else {
    task = 0;
  }

  for (; task < ntasks; ++task) {
    /* move to task position in SION file */
    sion_seek(outsid, task, SION_CURRENT_BLK, SION_CURRENT_POS);

    while (! sion_feof(outsid)) {
      /* read info header */
      sion_fread(&info, sizeof (_pos_struct), 1, outsid);

      /* read data */
      size   = info.width * info.height;
      buffer = malloc(size * sizeof (int));
      sion_fread(buffer, sizeof (int), size, outsid);

      /* store data inside global data file */
      i = 0;
      for (ypos=info.ypos; ypos < info.ypos + info.height; ++ypos) {
        for (xpos=info.xpos; xpos < info.xpos + info.width; ++xpos) {
          (*iterations)[ypos*infostruct->width + xpos] = buffer[i++];
          (*proc_distribution)[ypos*infostruct->width + xpos] = task;
        }
      }
      free(buffer);
    }
  }

  free(chunksizes);
  free(globalranks);

  /* close SION file */
  sion_close(outsid);
}
