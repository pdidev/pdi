/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
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
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/flowvr/gltrace/colors.h                           *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*  30/11/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_GLTRACE_COLORS_H
#define FLOWVR_UTILS_GLTRACE_COLORS_H

#include <map>
#include <string>

namespace flowvr
{

namespace utils
{

namespace gltrace
{

enum colors
{
  BLACK = 0x000000,
  GRAY = 0x808080,
  WHITE = 0xFFFFFF,

  GREEN = 0x00FF00,
  LIGHT_GREEN = 0x70FF70,
  DARK_GREEN = 0x008000,

  RED = 0x0000FF,
  LIGHT_RED = 0x7070FF,
  DARK_RED = 0x000080,

  BLUE = 0xFF6020,
  LIGHT_BLUE = 0xFFCB65,
  DARK_BLUE = 0x9D3A00,

  YELLOW = 0x00FFEA,
  LIGHT_YELLOW = 0x70FFFF,
  DARK_YELLOW = 0x008E9D,

  ORANGE = 0x007EFF,
  LIGHT_ORANGE = 0x7EBDFF,
  DARK_ORANGE = 0x0053AB,

  PINK = 0x9000FF,
  LIGHT_PINK = 0xD6A2FF,
  DARK_PINK = 0x5400A1
};

unsigned int getColor(const std::string& name);

} // namespace gltrace

} // namespace utils

} // namespace flowvr

#endif
