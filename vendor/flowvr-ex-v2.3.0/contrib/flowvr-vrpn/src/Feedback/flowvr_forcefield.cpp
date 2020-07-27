#include <flowvr/moduleapi.h>
#include <ftl/chunkevents.h>
#include <ftl/chunkwriter.h>
#include <ftl/quat.h>
#include <ftl/vec.h>

#include <iostream>
#include <string>
#include <vector>
#include <time.h>
#include "flowvr-vrpn/core/forces.h"

using namespace std;
using namespace flowvr;


int main(int argc, const char** argv)
{  
  bool passage = 0;
  
  flowvr::InputPort pIn("force");
  flowvr::OutputPort pOut("vrpn_force");


  
  std::vector<flowvr::Port*> ports;
  
  ports.push_back(&pIn);
  ports.push_back(&pOut);
  
  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  
  if (flowvr == NULL)
    return 1;


  forces2vrpn msg2vrpn;  
  flowvr::Message m;    
  fieldforces* ff = new fieldforces();  
  
  
  while (flowvr->wait()) {          
    
    flowvr->get(&pIn,m);
    
    if (m.data.getSize() != 0) {
      
      ff->ForceMsg(m);        
      //      cout << *ff;
      
      if (!(ff->isEmpty())) {
	msg2vrpn.AddMessage(ORIGIN,3,ff->getFFOrigin());
	msg2vrpn.AddMessage(FORCE,3,ff->getFFForce());
	msg2vrpn.AddMessage(JACOBIAN,9,ff->getFFJacobian());
	float radius = ff->getFFRadius();
	msg2vrpn.AddMessage(RADIUS,1,&radius);   
	msg2vrpn.AddMessage(SENDFORCE,0,NULL);
	msg2vrpn.put(&pOut);    
	msg2vrpn.ClearMessage();
      }
      else {
	msg2vrpn.AddMessage(STOPFORCE,0,NULL);
	msg2vrpn.put(&pOut);
	msg2vrpn.ClearMessage();
      }
    }
    else {
      msg2vrpn.AddMessage(ORIGIN,3,ff->getFFOrigin());
      msg2vrpn.AddMessage(FORCE,3,ff->getFFForce());
      msg2vrpn.AddMessage(JACOBIAN,9,ff->getFFJacobian());
      float radius = ff->getFFRadius();
      msg2vrpn.AddMessage(RADIUS,1,&radius);
      msg2vrpn.AddMessage(SENDFORCE,0,NULL);
      msg2vrpn.put(&pOut);
      msg2vrpn.ClearMessage();
    }
  }
  flowvr->close();
  return 0;
}
