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
* File: get.cpp                                                   *
*                                                                 *
* Contacts:                                                       *
*  26/02/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/module.h"
#include <iostream>
#include <unistd.h>

int sleep_time=1;

int main(int argc, const char** argv)
{
  flowvr::InputPort pIn("text");
  std::vector<flowvr::Port*> ports;
  ports.push_back(&pIn);

  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  if (flowvr == NULL)
  {
    return 1;
  }

  int it=0;
  while (flowvr->wait())
  {
    // Get Message
    flowvr::Message m;
    flowvr->get(&pIn,m);

    // Read data
    std::string text;
    text.append((const char*)m.data.readAccess(),((const char*)m.data.readAccess())+m.data.getSize());

    // Log info
    std::string source;
    int mit;
    m.stamps.read(pIn.stamps->it,mit);
    m.stamps.read(pIn.stamps->source,source);
    std::cout<<"Received "<<text<<"(it="<<mit<<") from "<<source<<std::endl;

    sleep(sleep_time);
    ++it;
  }

  flowvr->close();
  return 0;
}
