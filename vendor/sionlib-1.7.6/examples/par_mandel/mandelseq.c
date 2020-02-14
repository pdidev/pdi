#include <stdlib.h>
#include <stdio.h>

#include "mandelseq.h"
#include "ppmwrite.h"
#include "infostruct.h"
#include "mandelsion.h"

int main(int argc, char* argv[])
{
  int         *iterations = NULL;
  int         *proc_distribution = NULL;
  _infostruct info_glob;

  /* init ppm */
  ppminitsmooth(1);

  /* read SION file */
  collect_sion(&iterations, &proc_distribution, &info_glob);

  /* print information */
  print_infostruct(&info_glob);

  /* create ppm files */
  ppmwrite(iterations, info_glob.width, info_glob.height, 0,
           info_glob.maxiter, "mandelcol.ppm");


  ppmwrite(proc_distribution, info_glob.width, info_glob.height, 0,
             info_glob.numprocs, "mandelcol_procs.ppm");

  free(iterations);
  if (proc_distribution != NULL) {
    free(proc_distribution);
  }

  return EXIT_SUCCESS;
}

/*
 * Print infostruct data
 */
void print_infostruct(const _infostruct* info)
{
  printf("type      = %d\n", info->type);
  printf("width     = %d\n", info->width);
  printf("height    = %d\n", info->height);
  printf("numprocs  = %d\n", info->numprocs);
  printf("xmin      = %g\n", info->xmin);
  printf("xmax      = %g\n", info->xmax);
  printf("ymin      = %g\n", info->ymin);
  printf("ymax      = %g\n", info->ymax);
  printf("maxiter   = %d\n", info->maxiter);
}
