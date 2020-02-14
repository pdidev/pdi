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

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST A empty file\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* TEST A: write a small file with different chunks  */
  /* -------------------------- */
  {
    sion_int64  chunksize = 1024;
    sion_int32  fsblksize = 10;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    FILE       *fp;
    MPI_Comm    lcomm;
    int         i;
    char        buffer[1000];
    char        cbuffer[1000];

    sion_int64  bytes_written=-1;


    sid = sion_paropen_mpi("testA.out", "bw", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);


    /* write full chunk */
    for (i = 0; i < 1000; i++) buffer[i] = 'A';
    sion_fwrite(buffer,1,1000,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    /* write small chunk */
    for (i = 0; i < 1000; i++) buffer[i] = 'B';
    strcpy(cbuffer,"This is a small test for seek!");
    sion_fwrite(buffer,1,40,sid);
    sion_fwrite(cbuffer,1,36,sid);
    sion_fwrite(buffer,1,256,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    /* write half full chunk */
    for (i = 0; i < 1000; i++) buffer[i] = 'C';
    sion_fwrite(buffer,1,512,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    /* write small variable size chunk */
    for (i = 0; i < 1000; i++) buffer[i] = 'D';
    sion_fwrite(buffer,1,40+rank*14,sid);
    bytes_written=sion_get_bytes_written(sid);
    printf("on rank %d: bytes_written=%lld\n",rank,bytes_written);

    sion_parclose_mpi(sid);

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST A small file with different chunks\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);


#ifndef DOALL
  /* -------------------------- */
  /* TEST B: read part of file, with sion_seek, position in chunk */
  /* -------------------------- */
  {
    sion_int64  chunksize = -1;
    sion_int32  fsblksize = -1;
    int         globalrank= rank;
    int         numfiles  = 1;
    char       *newfname=NULL;
    int         sid;
    FILE       *fp;
    MPI_Comm    lcomm;
    int         i;
    char        buffer[1000];
    char        cbuffer[1000];
    int         currentchunknr;
    sion_int64  currentpos;
    int         maxchunks;
    sion_int64 *chunksizes;

    sion_int64  bytes_read=-1;

    for (i = 0; i < 1000; i++)
      buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("testA.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);

    sion_get_current_location(sid,&currentchunknr,&currentpos,&maxchunks,&chunksizes);
    printf("on rank %d: currentchunknr=%d\n",rank,currentchunknr);
    printf("on rank %d: currentpos    =%lld\n",rank,currentpos);
    printf("on rank %d: maxchunks     =%d\n",rank,maxchunks);
    for (i = 0; i < maxchunks; i++)     printf("on rank %d: chunksize[%d] = %lld\n",rank,i,chunksizes[i]);

    
    /* read part of first chunk */
    sion_fread(buffer,1,200,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld\n",rank,bytes_read);

    /* goto chunk 1, read part of this chunk */
    for (i = 0; i < 1000; i++) cbuffer[i] = '\0';
    sion_seek(sid,SION_CURRENT_RANK,1,40);
    sion_fread(cbuffer,1,36,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld read=%s\n",rank,bytes_read,cbuffer);

    /* read a further part of this chunk */
    sion_fread(buffer,1,200,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld\n",rank,bytes_read);


    /* goto chunk 1, read again part of this chunk */
    for (i = 0; i < 1000; i++) cbuffer[i] = '\0';
    sion_seek(sid,SION_CURRENT_RANK,1,40);
    sion_fread(cbuffer,1,36,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld read=%s\n",rank,bytes_read,cbuffer);


    /* goto chunk 1, read again part of this chunk */
    for (i = 0; i < 1000; i++) cbuffer[i] = '\0';
    sion_seek(sid,SION_CURRENT_RANK,1,50);
    sion_fread(cbuffer,1,10,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld read=%s\n",rank,bytes_read,cbuffer);

    sion_parclose_mpi(sid);

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST B\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

  /* -------------------------- */
  /* TEST C: read part of file, with sion_seek, absolute position in written bytes */
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
    char        cbuffer[1000];

    sion_int64  bytes_read=-1;

    for (i = 0; i < 1000; i++)
      buffer[i] = 'A' + rank;

    sid = sion_paropen_mpi("testA.out", "br", &numfiles, MPI_COMM_WORLD, &lcomm, &chunksize, &fsblksize, &globalrank, &fp, &newfname);

    /* read part of first chunk */
    sion_fread(buffer,1,200,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld\n",rank,bytes_read);

    /* goto chunk 1, read part of this chunk */
    for (i = 0; i < 1000; i++) cbuffer[i] = '\0';
    sion_seek(sid,SION_CURRENT_RANK,SION_ABSOLUTE_POS,1040);
    sion_fread(cbuffer,1,36,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld read=%s (absolute)\n",rank,bytes_read,cbuffer);

    /* read a further part of this chunk */
    sion_fread(buffer,1,200,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld\n",rank,bytes_read);


    /* goto chunk 1, read again part of this chunk */
    for (i = 0; i < 1000; i++) cbuffer[i] = '\0';
    sion_seek(sid,SION_CURRENT_RANK,1,40);
    sion_fread(cbuffer,1,36,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld read=%s (absolute)\n",rank,bytes_read,cbuffer);


    /* goto chunk 1, read again part of this chunk */
    for (i = 0; i < 1000; i++) cbuffer[i] = '\0';
    sion_seek(sid,SION_CURRENT_RANK,SION_ABSOLUTE_POS,1050);
    sion_fread(cbuffer,1,10,sid);
    bytes_read=sion_get_bytes_read(sid);
    printf("on rank %d: bytes_read=%lld read=%s (absolute)\n",rank,bytes_read,cbuffer);

    sion_parclose_mpi(sid);

  }

  fflush(stderr);
  fflush(stdout);
  MPI_Barrier(MPI_COMM_WORLD);
  printf("on rank %d: END of TEST C\n",rank);
  MPI_Barrier(MPI_COMM_WORLD);

#endif

  /* -------------------------- */
  /* EPILOG */
  /* -------------------------- */
  MPI_Finalize();
  
  return(0);
  
}
