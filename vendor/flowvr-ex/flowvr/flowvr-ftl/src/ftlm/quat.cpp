/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Clement Menier.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/ftlm/quat.cpp                                         *
*                                                                 *
* Contacts: 20/09/2005 Clement Menier <clement.menier.fr>         *
*                                                                 *
******************************************************************/
#include "ftl/quat.h"
#include <math.h>

namespace ftl
{

namespace Type
{


template <>
bool assign(Quat& dest, int type, const void* data)
{
  Vec<4,float> tmp(1,0,0,0);
  if (!assign(tmp,type,data)) return false;
  dest.w = tmp[0];
  dest.x = tmp[1];
  dest.y = tmp[2];
  dest.z = tmp[3];
  dest.normalize();
  return true;
}

template <>
bool assign(Quatd& dest, int type, const void* data)
{
  Vec<4,double> tmp(1,0,0,0);
  if (!assign(tmp,type,data)) return false;
  dest.w = tmp[0];
  dest.x = tmp[1];
  dest.y = tmp[2];
  dest.z = tmp[3];
  dest.normalize();
  return true;
}


} // namespace Type

} // namespace ftl
