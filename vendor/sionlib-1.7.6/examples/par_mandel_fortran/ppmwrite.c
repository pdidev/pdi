#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include "ppmwrite.h"

#define SHADES 256
#define MAXCOL2 (SHADES * SHADES * SHADES)
#define MAXCOL (7 * SHADES)

#define BUFLEN 2048

static int color_r[MAXCOL];
static int color_g[MAXCOL];
static int color_b[MAXCOL];

void ppminit_(int *d)
{
  int ix, iy, iz;
  int count = *d == 1 ? 0 : MAXCOL - 1;
  for (ix = 0; ix < SHADES; ++ix) {
    for (iy = 0; iy < SHADES; ++iy) {
      for (iz = 0; iz < SHADES; ++iz) {
        color_r[count] = ix;
        color_g[count] = iy;
        color_b[count] = iz;
        count         += *d;
      }
    }
  }
}

/*
 * SmoothColorTable
 */

void ppminitsmooth_(int *d)
{
  int S = SHADES - 1;
  int i;

  int count = *d == 1 ? 0 : MAXCOL - 1;

  color_r[count] = 0;
  color_g[count] = 0;
  color_b[count] = 0;
  count         += *d;

  for (i = 1; i <= S; ++i) {
    color_r[count] = 0;
    color_g[count] = 0;
    color_b[count] = i;
    count         += *d;
  }

  for (i = 1; i <= S; ++i) {
    color_r[count] = 0;
    color_g[count] = i;
    color_b[count] = S;
    count         += *d;
  }

  for (i = 1; i <= S; ++i) {
    color_r[count] = 0;
    color_g[count] = S;
    color_b[count] = S - i;
    count         += *d;
  }

  for (i = 1; i <= S; ++i) {
    color_r[count] = i;
    color_g[count] = S;
    color_b[count] = 0;
    count         += *d;
  }

  for (i = 1; i <= S; ++i) {
    color_r[count] = S;
    color_g[count] = S - i;
    color_b[count] = 0;
    count         += *d;
  }

  for (i = 1; i <= S; ++i) {
    color_r[count] = S;
    color_g[count] = 0;
    color_b[count] = i;
    count         += *d;
  }

  for (i = 1; i <= S; ++i) {
    color_r[count] = S;
    color_g[count] = i;
    color_b[count] = S;
    count         += *d;
  }

  /*
     for (i=0; i<MAXCOL; ++i) {
     printf("COL[%5d]: %3d,%3d,%3d\n",i,color_r[i],color_g[i],color_b[i]);
     }
   */
}

/*
 * ppmwrite
 */
void ppmwriter_(int* a, int *nx, int *ny, int *minval, int *maxval, char* filename)
{
  FILE*  outfile;
  double distance = *maxval - *minval;
  double factor   = (double)distance / MAXCOL;
  char   buf[BUFLEN];
  int    b, ix, iy, iz;

  if ((outfile = fopen(filename, "wt")) == NULL) {
    printf("\tUnable to open output file %s.\n", filename);
    return;
  }

  b = sprintf(buf, "P6 %5d %5d %d\n", *nx, *ny, SHADES - 1);

  for (iy = 0; iy < *ny; ++iy) {
    for (ix = 0; ix < *nx; ++ix) {
      /* put the origin to the bottom instead of the top */
      int idx = a[ix + (*ny - iy - 1) * (*nx)];
      if (idx == *maxval) {
        for (iz = 0; iz < 3; ++iz) {
          buf[b++] = 0;
          if (b >= BUFLEN) {
            fwrite(buf, (size_t)BUFLEN, 1, outfile);
            b = 0;
          }
        }
      }
      else {
        idx = (int)(idx / factor);
        for (iz = 0; iz < 3; ++iz) {
          if (iz == 0) {
            buf[b++] = (char) color_r[idx];
          }
          if (iz == 1) {
            buf[b++] = (char) color_g[idx];
          }
          if (iz == 2) {
            buf[b++] = (char) color_b[idx];
          }
/*        printf("WF: %3d %3d %1d -> %4d %d\n",ix,iy,iz,idx,buf[b-1]); */
          if (b >= BUFLEN) {
            fwrite(buf, (size_t)BUFLEN, 1, outfile);
            b = 0;
          }
        }
      }
    }
  }
  fwrite(buf, (size_t)b, 1, outfile);
  fclose(outfile);
}
