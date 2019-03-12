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
* File: src/gltrace/colors.cpp                                    *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*  30/11/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "colors.h"

#include <map>
#include <string>

namespace flowvr
{

namespace utils
{

namespace gltrace
{

class ColorList : public std::map<std::string,unsigned int>
{
public:

  void add(const std::string& name, unsigned int c)
  {
    (*this)[name]=c;
  }

  ColorList()
  {
    add("black", BLACK);
    add("gray", GRAY);
    add("white", WHITE);
    add("green", GREEN);
    add("lightgreen", LIGHT_GREEN);
    add("darkgreen", DARK_GREEN);
    add("red", RED);
    add("lightred", LIGHT_RED);
    add("darkred", DARK_RED);
    add("blue", BLUE);
    add("lightblue", LIGHT_BLUE);
    add("darkblue", DARK_BLUE);
    add("yellow", YELLOW);
    add("lightyellow", LIGHT_YELLOW);
    add("darkyellow", DARK_YELLOW);
    add("orange", ORANGE);
    add("lightorange", LIGHT_ORANGE);
    add("darkorange", DARK_ORANGE);
    add("pink", PINK);
    add("lightpink", LIGHT_PINK);
    add("darkpink", DARK_PINK);

    // Additional colors from http://www.graphviz.org/cvs/doc/info/colors.html
    add("cyan",0xFFFF00);
    add("purple",0xF020A0);
    add("maroon",0x6030B0);
  }

  unsigned int get(const std::string& name)
  {
    // filter name
    std::string s;
    for (unsigned int i=0;i<name.size();i++)
    {
      char c=name[i];
      if (c>='A' && c<='Z') c='a'+(c-'A');
      if (c=='_') continue; // ignore '_'
      s.push_back(c);
    }
    const_iterator it = find(s);
    if (it==end()) return BLACK;
    else return it->second;
  }

} colorlist;

unsigned int getColor(const std::string& name)
{
  return colorlist.get(name);
}

} // namespace gltrace

} // namespace utils

} // namespace flowvr
