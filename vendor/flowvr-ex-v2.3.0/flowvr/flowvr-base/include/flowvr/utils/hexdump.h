/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Base Libraries                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490). ALL RIGHTS RESERVED.                                *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/flowvr/utils/hexdump.h                            *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_HEXDUMP_H
#define FLOWVR_UTILS_HEXDUMP_H

#include <stdio.h>

namespace flowvr
{

namespace utils
{

inline char todisp(char c)
{
  if ((unsigned)c<32) return '.';
  else return c;
}

inline void hexdump(const unsigned char* p, size_t size, size_t pos=0, int linesize=16)
{
  int offsetsize = 1;
  int line,offset;
  while ((pos+size)>(1<<(offsetsize*4))) ++offsetsize;
  p+=pos;
  for (line=0;linesize<size;line++,p+=linesize,size-=linesize)
  {
    printf("%0*lX:",offsetsize,(long unsigned int)(pos+line*linesize));
    for (offset=0;offset<linesize;offset++)
      printf(" %02X",(int)p[offset]);
    printf("  ");
    for (offset=0;offset<linesize;offset++)
      printf("%c",todisp((unsigned char)p[offset]));
    printf("\n");
  }
  {
    printf("%0*lX:",offsetsize,(long unsigned int)(pos+line*linesize));
    for (offset=0;offset<size;offset++)
      printf(" %02X",(int)p[offset]);
    printf("  ");
    for (offset=0;offset<size;offset++)
      printf("%c",todisp(p[offset]));
    printf("\n");
  }
}

} // namespace utils

} // namespace flowvr

#endif
