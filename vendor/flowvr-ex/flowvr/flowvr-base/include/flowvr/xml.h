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
* File: include/flowvr/xml.h                                      *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_XML_H
#define FLOWVR_XML_H

#include "flowvr/common.h"

#include <ctype.h>

#define TIXML_USE_STL
#define TIXML_NEED_STREAM
#define TIXML_DOMAPI
#define TIXML_NAMESPACE_BEGIN namespace flowvr { namespace xml {
#define TIXML_NAMESPACE_END                 }               }
#define TIXML_NAMESPACE flowvr::xml

#include "tinyxml.h"

namespace flowvr
{

namespace xml
{

class DOMParser
{
 public:

  DOMParser() {}

  int parse(const char* filename);

  int parseString(const char* text);

  DOMDocument* getDocument() { return &document; }

 protected:
  DOMDocument document;

};

class DOMWriter
{
 public:

  static std::string toString(const DOMNode* xml);
};

typedef DOMParser XercesDOMParser;

} // namespace xml

} // namespace flowvr

#endif
