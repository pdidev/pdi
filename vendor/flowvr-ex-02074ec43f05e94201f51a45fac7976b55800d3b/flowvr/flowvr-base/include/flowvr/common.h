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
* File: include/flowvr/common.h                                   *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_COMMON_H
#define FLOWVR_COMMON_H

#include <stdlib.h> // for NULL
#include <string>

namespace flowvr
{

  typedef unsigned char ubyte;
  typedef   signed char sbyte;
  typedef unsigned short uword;
  typedef   signed short sword;
  typedef unsigned int udword;
  typedef   signed int sdword;
#if defined __GNUC__
  typedef unsigned long long uqword;
  typedef   signed long long sqword;
#elif defined WIN32
  typedef unsigned __int64 uqword;
  typedef   signed __int64 sqword;
#endif

  static unsigned int DefaultMemId = 51337;
};



#endif
