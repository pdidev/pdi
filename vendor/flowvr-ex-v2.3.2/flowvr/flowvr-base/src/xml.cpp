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
* File: src/xml.cpp                                               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/xml.h"

#include "tinyxml/tinyxml.cpp"

#include "tinyxml/tinyxmlerror.cpp"

#include "tinyxml/tinyxmlparser.cpp"

#include <iostream>
#include <sstream>

namespace flowvr
{

namespace xml
{

int DOMParser::parse(const char* filename)
{
  document.LoadFile(filename);
  if (document.Error())
  {
    std::cerr << "Error in "<<document.Value()<<":"<<document.ErrorDesc() << std::endl;
  }
  return document.Error();
}

int DOMParser::parseString(const char* text)
{
  document.Parse(text);
  if (document.Error())
  {
    std::cerr << "Error in "<<text<<":"<<document.ErrorDesc() << std::endl;
  }
  return document.Error();
}

std::string DOMWriter::toString(const DOMNode* xml)
{
  if (xml==NULL) return std::string();
  std::ostringstream out;
  out << *xml;
  return out.str();
}

} // namespace xml

} // namespace flowvr

