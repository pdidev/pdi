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

#include "sion_file.h"

#define FILE_NAME "testA.out"

int main(int argc, char** argv)
{
  _sion_fileptr *f = _sion_file_open(FILE_NAME, SION_FILE_FLAG_ANSI | SION_FILE_FLAG_CREATE | SION_FILE_FLAG_WRITE, 0);
  if (!f) {
    fprintf(stderr, "could not open file.\n");
    return EXIT_FAILURE;
  }

  int s = 0x11111111;
  sion_int64 c = _sion_file_write(&s, sizeof(s), f);
  if (c != sizeof(s)) {
    fprintf(stderr, "could not write file.\n");
    return EXIT_FAILURE;
  }

  sion_int64 p = _sion_file_set_position(f, 0);
  if (p) {
    fprintf(stderr, "could not set file position.\n");
  }

  int t = 0;
  // Reading a file opened for writing should fail.
  c = _sion_file_read(&t, sizeof(t), f);
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
