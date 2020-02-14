/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

/*!
 * @file sion_convert.c
 *
 * @brief originally written for VISIT: perform byte-order swapping for arrays
 *
 * @author Th.Eickermann & W.Frings (December 2000)
 *
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include "sion.h"
#include "sion_debug.h"


/**
 * @ brief Perform byte-order swapping for arrays
 *
 * `n` elements of `size` bytes each are swapped if `do_swap` is `true`.
 * In-place swapping (`target == source`) is allowed.
 * If `target != source`, the buffers must not overlap.
 *
 * @param[out] target the byte-swapped data is written starting at this address
 * @param[in] source the data to be byte-swapped is read starting at this address
 * @param[in] size the size (in bytes) of a single element
 * @param[in] n the number of elements to be byte-swapped
 * @param[in] do_swap byte-swapping is only performed if this argument is `true`
 */
void sion_swap(void *target, void *source, int size, int n, int do_swap)
{
  int       i, j;
  char      *iptr, *optr;

  optr = target;
  iptr = source;

  if (iptr == optr) {
    if (!do_swap)
      return;
    /*
     * in-place swapping
     * unroll loop for the common cases 2,4,8 Byte
     *
     */
    DPRINTF((4, "sion_swap: swapping %d x %d Bytes in place =%ld\n", n, size, (sion_int32) *iptr));

    char tmp;
    switch (size) {
    case 1:
      /* nothing to do */
      break;
    case 2:
      for (i = 0; i < n; i++) {
        tmp = iptr[0];
        iptr[0] = iptr[1];
        iptr[1] = tmp;
        iptr += 2;
      }
      break;
    case 4:
      for (i = 0; i < n; i++) {
        tmp = iptr[0];
        iptr[0] = iptr[3];
        iptr[3] = tmp;

        tmp = iptr[1];
        iptr[1] = iptr[2];
        iptr[2] = tmp;

        iptr += 4;
      }
      break;
    case 8:
      for (i = 0; i < n; i++) {
        tmp = iptr[0];
        iptr[0] = iptr[7];
        iptr[7] = tmp;

        tmp = iptr[1];
        iptr[1] = iptr[6];
        iptr[6] = tmp;

        tmp = iptr[2];
        iptr[2] = iptr[5];
        iptr[5] = tmp;

        tmp = iptr[3];
        iptr[3] = iptr[4];
        iptr[4] = tmp;

        iptr += 8;
      }
      break;
    default:
      for (i = 0; i < n; i++) {
        for (j = 0; j < size / 2; j++) {
          tmp = iptr[j];
          iptr[j] = iptr[size - j - 1];
          iptr[size - j - 1] = tmp;
        }
        iptr += size;
      }
    }
    DPRINTF((4, "sion_swap: swapped   %d x %d Bytes in place =%ld\n", n, size, *optr));

  }
  else {
    if (!do_swap) {
      memcpy(optr, iptr, size * n);
      return;
    }
    /*
     * swap into a different non-overlapping buffer
     * unroll common cases again
     */
    DPRINTF((4, "sion_swap: swapping %d x %d Bytes\n", n, size));

    switch (size) {
    case 1:
      memcpy(optr, iptr, size * n);
      break;
    case 2:
      for (i = 0; i < n; i++) {
        optr[0] = iptr[1];
        optr[1] = iptr[0];

        iptr += 2;
        optr += 2;
      }
      break;
    case 4:
      for (i = 0; i < n; i++) {
        optr[0] = iptr[3];
        optr[1] = iptr[2];
        optr[2] = iptr[1];
        optr[3] = iptr[0];

        iptr += 4;
        optr += 4;
      }
      break;
    case 8:
      for (i = 0; i < n; i++) {
        optr[0] = iptr[7];
        optr[1] = iptr[6];
        optr[2] = iptr[5];
        optr[3] = iptr[4];
        optr[4] = iptr[3];
        optr[5] = iptr[2];
        optr[6] = iptr[1];
        optr[7] = iptr[0];

        iptr += 8;
        optr += 8;
      }
      break;
    default:
      for (i = 0; i < n; i++) {
        for (j = 0; j < size; j++) {
          optr[j] = iptr[size - j - 1];
        }
        iptr += size;
        optr += size;
      }
    }
  }
}
