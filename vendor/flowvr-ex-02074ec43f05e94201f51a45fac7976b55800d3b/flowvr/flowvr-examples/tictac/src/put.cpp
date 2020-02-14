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
* File: put.cpp                                                   *
*                                                                 *
* Contacts:                                                       *
*  26/02/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/module.h>
#include <iostream>
#include <unistd.h>

int sleep_time=2;

int main(int argc, const char** argv)
{
  flowvr::OutputPort pOut("text");
  std::vector<flowvr::Port*> ports;
  ports.push_back(&pOut);
  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  if (flowvr == NULL)
  {
    return 1;
  }

  int it=0;
  while (flowvr->wait())
  {
    flowvr::MessageWrite m;
    std::string text=(it&1)?"tac":"tic";

    // Build data
    m.data = flowvr->alloc(text.length());
    memcpy(m.data.writeAccess(),text.c_str(),text.length());

    // Send message
    flowvr->put(&pOut,m);

    // Log info
    int mit;
    m.stamps.read(pOut.stamps->it,mit);
    std::cout<<"Sent "<<text<<" (it="<<mit<<")"<<std::endl;

    sleep(sleep_time);
    ++it;
  }

  flowvr->close();

  return 0;
}
