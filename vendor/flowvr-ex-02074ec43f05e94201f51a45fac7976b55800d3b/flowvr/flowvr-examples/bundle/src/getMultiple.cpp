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
  flowvr::InputPort pIn2("text2");
  std::vector<flowvr::Port*> ports;
  ports.push_back(&pIn);
  ports.push_back(&pIn2);

  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  if (flowvr == NULL)
  {
    return 1;
  }

  int it=0;
  while (flowvr->wait())
  {
    // Get Messages
    flowvr::Message m;
    flowvr->get(&pIn,m);
    
    flowvr::Message m2;
    flowvr->get(&pIn2,m2);

    // Read data
    std::string text;
    text.append((const char*)m.data.readAccess(),((const char*)m.data.readAccess())+m.data.getSize());
    
    std::string text2;
    text2.append((const char*)m2.data.readAccess(),((const char*)m2.data.readAccess())+m2.data.getSize());

    // Log info
    std::string source;
    int mit;
    m.stamps.read(pIn.stamps->it,mit);
    m.stamps.read(pIn.stamps->source,source);
    std::cout<<"(getMultiple) Received "<<text<<"(it="<<mit<<") from "<<source<<std::endl;
    
    std::string source2;
    int mit2;
    m2.stamps.read(pIn2.stamps->it,mit2);
    m2.stamps.read(pIn2.stamps->source,source2);
    std::cout<<"(getMultiple) Received "<<text2<<"(it="<<mit2<<") from "<<source2<<std::endl;

    sleep(sleep_time);
    ++it;
  }

  flowvr->close();
  return 0;
}
