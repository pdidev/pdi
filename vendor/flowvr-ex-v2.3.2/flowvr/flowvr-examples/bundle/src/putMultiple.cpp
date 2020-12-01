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

int sleep_time=1;

int main(int argc, const char** argv)
{
  flowvr::OutputPort pOut("text");
  std::vector<flowvr::Port*> ports;
  ports.push_back(&pOut);
  flowvr::ModuleAPI* module1 = flowvr::initModule(ports, "Module1");
  
  flowvr::OutputPort pOut2("text2");
  std::vector<flowvr::Port*> ports2;
  ports2.push_back(&pOut2);
  flowvr::ModuleAPI* module2 = flowvr::initModule(ports2, "Module2");
  
  
  if (module1 == NULL || module2 == NULL) {
      std::cout << "Modules couldn't be initialized" << std::endl;
	  return 1;	
  }

  int it=0;
  while (module1->wait())
  {
    flowvr::MessageWrite m;
    std::string text=(it&1)?"tac":"tic";

    // Build data
    m.data = module1->alloc(text.length());
    memcpy(m.data.writeAccess(),text.c_str(),text.length());

    // Send message
    module1->put(&pOut,m);

    // Log info
    int mit;
    m.stamps.read(pOut.stamps->it,mit);
    std::cout<<"(putMultiple/Module1) : Sent "<<text<<" (it="<<mit<<")"<<std::endl;
    
    
    // MODULE 2
    
	module2->wait();
	flowvr::MessageWrite m2;
	std::string text2=(it&1)?"TAC":"TIC";

	// Build data
	m2.data = module2->alloc(text2.length());
	memcpy(m2.data.writeAccess(),text2.c_str(),text2.length());

	// Send message
	module2->put(&pOut2,m2);

	// Log info
	int mit2;
	m2.stamps.read(pOut2.stamps->it,mit2);
	std::cout<<"(putMultiple/Module2) : Sent "<<text2<<" (it="<<mit2<<")"<<std::endl;

	sleep(sleep_time);
	++it;
  }

  module1->close();
  module2->close();

  return 0;
}
