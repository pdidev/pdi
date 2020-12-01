/******* COPYRIGHT ************************************************
*                                                                 *
*                         FlowVR VRPN                             *
*                    FlowVR VRPN Coupling Modules                 *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (EA 4022) ALL RIGHTS RESERVED.                                  *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Sebastien Limet,                                             *
*    Sophie Robert.                                               *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: ./src/Input/flowvr_button.cpp                             *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
//FlowVR IMPORTS
#include <flowvr/moduleapi.h>
#include <ftl/chunkevents.h>
#include <ftl/chunkwriter.h>
#include <ftl/quat.h>

#include <vrpn_Button.h>

#include "flowvr_vrpn_stamps.h"

#include <iostream>
#include <string>
#include <vector>


using namespace std;
using namespace flowvr;


int main(int argc, const char** argv)
{
  
  flowvr::InputPort pIn("vrpn_button");
  flowvr::OutputPort pOut("ftl_button");
  
  std::vector<flowvr::Port*> ports;
  
  
  StampInfo TypeMsgStamp("TypeMsg",TypeInt::create());
  (&pIn)->stamps->add(&TypeMsgStamp);
  
  ports.push_back(&pIn);
  ports.push_back(&pOut);
  
  int TypeMsg;
  int Iteration=0;
  
  
  ftl::ChunkEventWriter* buttMsg = NULL;
  

  
  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  
  if (flowvr == NULL)
    return 1;
  
  flowvr::Message m;    
  vrpn_BUTTONCB b={0,0,0};

  // A first message to unlock the mergeIt filter
  buttMsg = new ftl::ChunkEventWriter();
  buttMsg->addEventButton(static_cast<unsigned char>(0), 0);
  buttMsg->put(&pOut);
  delete buttMsg;
  // End of first message
  

  while (flowvr->wait()) {          
    // Get Message
    flowvr->get(&pIn,m);


    m.stamps.read(pIn.stamps->it,Iteration);
    m.stamps.read(TypeMsgStamp,TypeMsg);
    
    //    cout << "Type du message reçu : " << TypeMsg << " à l'itération : " << Iteration << endl;
    
    // if (m.data.getSize() > 0) {

    //   buttMsg = new ftl::ChunkEventWriter();

    //   if (TypeMsg==flowvr_vrpn_BUTTONCB)
    //   memcpy((void*)(&b),m.data.readAccess(),m.data.getSize());      
    //   cout << "bouton : " << b.button << " : " <<  b.state << " à l'itération : " << Iteration << endl;
    //   buttMsg->addEventButton(static_cast<unsigned char>(b.button), b.state);
    //   buttMsg->put(&pOut);
    //   delete buttMsg;
    // }

    if (TypeMsg==flowvr_vrpn_BUTTONCB) {
      if (m.data.getSize() > 0) {
	buttMsg = new ftl::ChunkEventWriter();
	memcpy((void*)(&b),m.data.readAccess(),m.data.getSize());      
	//	cout << "bouton : " << b.button << " : " <<  b.state << " à l'itération : " << Iteration << endl;
	buttMsg->addEventButton(static_cast<unsigned char>(b.button), b.state);
	buttMsg->put(&pOut);
	delete buttMsg;
      }
    }
  }
  
  flowvr->close();
  return 0;
}
