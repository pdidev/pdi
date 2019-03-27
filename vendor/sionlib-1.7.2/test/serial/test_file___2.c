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

#include "sion_file.h"

#define FILE_NAME "testA.out"

int main(int argc, char** argv)
{
  _sion_fileptr *f = _sion_file_open(FILE_NAME, SION_FILE_FLAG_POSIX | SION_FILE_FLAG_CREATE | SION_FILE_FLAG_WRITE, 0);
  if (!f) {
    fprintf(stderr, "could not open file.\n");
    return EXIT_FAILURE;
  }

  if (SION_SUCCESS != _sion_file_close(f)) {
    fprintf(stderr, "could not close file.\n");
    return EXIT_FAILURE;
  }

  f = _sion_file_open(FILE_NAME, SION_FILE_FLAG_POSIX | SION_FILE_FLAG_READ, 0);
  if (!f) {
    fprintf(stderr, "could not open file.\n");
    return EXIT_FAILURE;
  }

  int s = 0x11111111;
  // Writing a file opened for reading should fail.
  sion_int64 c = _sion_file_write(&s, sizeof(s), f);
  if (c != sizeof(s)) {
    fprintf(stderr, "could not write file.\n");
    return EXIT_FAILURE;
  }

  if (SION_SUCCESS != _sion_file_close(f)) {
    fprintf(stderr, "could not close file.\n");
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
