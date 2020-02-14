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

#include "sion.h"

int main(int argc, char **argv)
{
  char ppmtype[2];
  int xmaxpoints;
  int ymaxpoints;
  int myymaxpoints;
  int colorlimit;
  const int single_img_leng = 6;
  char *image = NULL;
  char *output_fname = "test_mandel_1.ppm";
  char *input_fname = "test_mandel_1.sion";

  sion_int32  fsblksize = 10;
  sion_int64 *chunksizes = NULL;
  int        *globalranks = NULL;
  int         ntasks = 4;
  int         nfiles = 1;
  int         sid;
  FILE       *infp;
  FILE       *outfp;
  sion_int64 posinblk;

  int ipoint, currentblknr, itask;

  int current_endianness, file_endianness, doswap;

  printf("  ===============================\n");
  printf("  Calculations of mandelbrot set.\n");
  printf("  ===============================\n");
  printf("  Converting %s to %s\n\n", input_fname, output_fname);

  fsblksize = -1;
  nfiles    =  1;
  sid       = -1;

  sid = sion_open(input_fname, "rb", &ntasks, &nfiles, &chunksizes,
  		  &fsblksize, &globalranks, &infp);
  outfp = fopen(output_fname, "wb");

  /* Readi-in data */
  currentblknr = 0;
  posinblk     = 0;

  current_endianness = sion_get_endianness();
  file_endianness = sion_get_file_endianness(sid);
  if (current_endianness == file_endianness) {
    doswap = 0;
  }
  else {
    doswap=1;
  }


  /* Read image */
  for (itask = 0; itask < ntasks; itask++) {
  /* do itask=0,ntasks-1 */
    sion_seek(sid, itask, currentblknr, posinblk);
    if (itask == 0) {
      /* Read ppm header in and write it to ppm file */
      printf("  Converting points from task %6i\n", itask);
      sion_fread(&ppmtype, 2, 1, sid);
      sion_fread(&xmaxpoints, 4, 1, sid);
      sion_swap(&xmaxpoints, &xmaxpoints, 4, 1, doswap);
      sion_fread(&ymaxpoints, 4, 1, sid);
      sion_swap(&ymaxpoints, &ymaxpoints, 4, 1, doswap);
      sion_fread(&colorlimit, 4, 1, sid);
      sion_swap(&colorlimit, &colorlimit, 4, 1, doswap);
      printf("  File created with %6i tasks\n", ntasks);
      printf("  ppmtype: %c%c\n", ppmtype[0],ppmtype[1]);
      printf("  xmaxpoints: %6i\n", xmaxpoints);
      printf("  ymaxpoints: %6i\n", ymaxpoints);
      printf("  colorlimit: %6i\n", colorlimit);
      fprintf(outfp, "%s %i %i %i\n", ppmtype, xmaxpoints, ymaxpoints,
  	      colorlimit);
    }
    printf("  Reading data from task %6i\n", itask);
    myymaxpoints = ymaxpoints / ntasks;
    if (ymaxpoints % ntasks != 0) {
      if (itask == ntasks-1) {
        myymaxpoints = myymaxpoints + (ymaxpoints % ntasks);
      }
    }

    image = (char*) malloc(single_img_leng * myymaxpoints * xmaxpoints);
    
    sion_fread(image, 1, single_img_leng * myymaxpoints * xmaxpoints, sid);
    for (ipoint = 0; ipoint < single_img_leng * myymaxpoints * xmaxpoints;
	 ipoint++) {
      fprintf(outfp, "%c", image[ipoint]);
    }
    if (image) free(image);
  }

  fclose(outfp);
  sion_close(sid);

  if(chunksizes) free(chunksizes);
  if(globalranks) free(globalranks);

  return 0;
}
