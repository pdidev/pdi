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
* File: ./src/Input/flowvr_analog.cpp                             *
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

#include <vrpn_Analog.h>

#include "flowvr/module.h"
#include "flowvr_vrpn_stamps.h"

#include <iostream>
#include <string>
#include <vector>


using namespace std;
using namespace flowvr;


int main(int argc, const char** argv)
{
  InputPort pIn("vrpn_analog");
  OutputPort pOut("ftl_analog");
  std::vector<flowvr::Port*> ports;
  
  StampInfo TypeMsgStamp("TypeMsg",TypeInt::create());
  (&pIn)->stamps->add(&TypeMsgStamp);
  ports.push_back(&pIn);
  ports.push_back(&pOut);
    
  int TypeMsg;
  int Iteration;
  
  ftl::ChunkEventWriter* algMsg = NULL;


  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  if (flowvr == NULL)
    return 1;

  vrpn_ANALOGCB a;      	
  
  vrpn_float64 channel_old[vrpn_CHANNEL_MAX];


  bool premier = 1;
  algMsg = new ftl::ChunkEventWriter();  
  algMsg->addEventSlider(static_cast<unsigned char>(0), 0);
  algMsg->put(&pOut);
  delete algMsg;
  
  while (flowvr->wait()) {
    
    algMsg = new ftl::ChunkEventWriter();  
    // Get Message
    flowvr::Message m;
    flowvr->get(&pIn,m);
    m.stamps.read(pIn.stamps->it,Iteration);
    m.stamps.read(TypeMsgStamp,TypeMsg);    
    
    //    cout << "Type du message reçu : " << TypeMsg << endl;
    

    if (TypeMsg==flowvr_vrpn_ANALOGCB) {
      memcpy((void*)(&a),m.data.readAccess(),m.data.getSize());	  	
      if (premier) {
	for (int i=0; i< a.num_channel; ++i) {
	  channel_old[i] = a.channel[i];
          //	  cout << "channel " << i << ":" << a.channel[i] << endl;
	  algMsg->addEventSlider(static_cast<unsigned char>(i), a.channel[i]);
	}
	premier = 0;
	algMsg->put(&pOut);          
      }
      else {	
	bool chgt = 0;
	for (int i=0; i< a.num_channel; ++i) {
	  if ( channel_old[i] != a.channel[i]) {
	    chgt = 1;
            //	    cout << "channel " << i << ":" << a.channel[i] << endl;
	    algMsg->addEventSlider(static_cast<unsigned char>(i), a.channel[i]);
	    channel_old[i]=a.channel[i];
	  }
	}
	if (chgt)
	  algMsg->put(&pOut);          
      }
    }
    else {
      algMsg->put(&pOut);          
    }
    
    //    algMsg->put(&pOut);          
    
    delete algMsg;
  }
  flowvr->close();
  return 0;
}
