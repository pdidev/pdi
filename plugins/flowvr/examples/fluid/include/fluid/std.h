/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Application Library                         *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *	
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./std.h                                                   *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef STD_H
#define STD_H

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <iostream>
#include <iomanip>

 inline int f2int_down(float f)
 {
   //if (f>=0) return int(f);
   //else      return -int(-f);
   return int(f+1000000)-1000000;
 }
/*
inline int f2int_down(double r)
{
  static const double FLOATTOINTCONST=(1.5*(1LL<<(52-16)));
  r+=FLOATTOINTCONST;
  return ((((int*)&r)[0])>>16);
}
*/

template <class T>
inline T lerp(const T& a,const T& b,float f)
{
  return a+(b-a)*f;
}

#endif
