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
* File: src/stamp.cpp                                             *
*                                                                 *
* Contacts:                                                       *
*  05/25/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/trace.h"
#include <iostream>

namespace flowvr
{

Trace::Trace(const std::string &myname)
  : name(myname), id(0)
{
}

Trace::~Trace()
{
}

xml::DOMElement* Trace::xmlDesc() const
{
  xml::DOMElement* root = new xml::DOMElement("trace");
  root->SetAttribute("name",name);
  if (id!=0)
    root->SetAttribute("id",id);
  return root;
}

/// Activate logging of this trace
bool Trace::start(int newId, BufferWrite newBuf, xml::DOMElement* parameters)
{
  id = newId;
  log = newBuf;
  return true;
}

/// Stop logging this trace
bool Trace::stop(xml::DOMElement* parameters)
{
  log.clear();
  return true;
}

} // namespace flowvr
