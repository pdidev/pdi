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
  printf("SIONlib Version %d.%d.%d (git_rev %s), fileformat version %d (%s)\n",
         SION_MAIN_VERSION,SION_SUB_VERSION, SION_VERSION_PATCHLEVEL,
         SION_GIT_VERSION, SION_FILEFORMAT_VERSION, argv[0]);
  return 0;
}
