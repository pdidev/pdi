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

#include <sys/stat.h>
#include <sys/types.h>

#include "sion_file.h"

#define FILE_NAME "testA.out"

int main(int argc, char** argv)
{
  int m = mkdir(FILE_NAME, 0777);
  if (m) {
    fprintf(stderr, "could not create directory.\n");
    return EXIT_FAILURE;
  }

  _sion_fileptr *f = _sion_file_open(FILE_NAME, SION_FILE_FLAG_POSIX | SION_FILE_FLAG_READ, 0);
  if (!f) {
    fprintf(stderr, "could not open file.\n");
    return EXIT_FAILURE;
  }

  int t = 0;
  // Reading a directory should fail.
  sion_int64 c = _sion_file_read(&t, sizeof(t), f);
  if (c != sizeof(t)) {
    fprintf(stderr, "could not read file.\n");
    return EXIT_FAILURE;
  }

  if (SION_SUCCESS != _sion_file_close(f)) {
    fprintf(stderr, "could not close file.\n");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
