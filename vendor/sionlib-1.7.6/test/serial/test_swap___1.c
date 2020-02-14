/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "sion.h"

int main(int argc, char* argv[]) {
  uint8_t a1 = 0x01, b1;
  sion_swap(&a1, &a1, sizeof(a1), 1, 0);
  assert(0x01 == a1);
  sion_swap(&a1, &a1, sizeof(a1), 1, 1);
  assert(0x01 == a1);

  sion_swap(&b1, &a1, sizeof(a1), 1, 0);
  assert(0x01 == b1);
  sion_swap(&b1, &a1, sizeof(a1), 1, 1);
  assert(0x01 == b1);

  uint16_t a2 = 0x0123, b2;
  sion_swap(&a2, &a2, sizeof(a2), 1, 0);
  assert(0x0123 == a2);
  sion_swap(&a2, &a2, sizeof(a2), 1, 1);
  assert(0x2301 == a2);
  sion_swap(&a2, &a2, sizeof(a2), 1, 1);
  assert(0x0123 == a2);

  sion_swap(&b2, &a2, sizeof(a2), 1, 0);
  assert(0x0123 == b2);
  sion_swap(&b2, &a2, sizeof(a2), 1, 1);
  assert(0x2301 == b2);

  uint32_t a4 = 0x01234567, b4;
  sion_swap(&a4, &a4, sizeof(a4), 1, 0);
  assert(0x01234567 == a4);
  sion_swap(&a4, &a4, sizeof(a4), 1, 1);
  assert(0x67452301 == a4);
  sion_swap(&a4, &a4, sizeof(a4), 1, 1);
  assert(0x01234567 == a4);

  sion_swap(&b4, &a4, sizeof(a4), 1, 0);
  assert(0x01234567 == b4);
  sion_swap(&b4, &a4, sizeof(a4), 1, 1);
  assert(0x67452301 == b4);

  uint64_t a8 = 0x0123456789abcdef, b8;
  sion_swap(&a8, &a8, sizeof(a8), 1, 0);
  assert(0x0123456789abcdef == a8);
  sion_swap(&a8, &a8, sizeof(a8), 1, 1);
  assert(0xefcdab8967452301 == a8);
  sion_swap(&a8, &a8, sizeof(a8), 1, 1);
  assert(0x0123456789abcdef == a8);

  sion_swap(&b8, &a8, sizeof(a8), 1, 0);
  assert(0x0123456789abcdef == b8);
  sion_swap(&b8, &a8, sizeof(a8), 1, 1);
  assert(0xefcdab8967452301 == b8);

  return EXIT_SUCCESS;
}
