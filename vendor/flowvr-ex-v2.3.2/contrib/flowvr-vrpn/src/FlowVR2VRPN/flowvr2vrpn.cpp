#include <iostream>
#include <unistd.h>
#include <vector>

//VRPN IMPORTS
#include <vrpn_ForceDevice.h>
#include <vrpn_Analog_Output.h>

//FlowVR IMPORTS
#include <flowvr/moduleapi.h>
#include "flowvr-vrpn/core/forces.h"


using namespace std;
using namespace flowvr;

ModuleAPI* flowvrmodule = 0;

InputPort pIn("vrpnmsg");

std::vector<flowvr::Port*> ports;

int main(int argc, char **argv) {       


  char* server;
  
  if (argc < 2) {
    cout << argv[0] << ": Invalid parameters " << endl;
    cout << "Usage : " << endl;
    cout << argv[0] << "peripheric@host " << endl;
    return -1;
  }
  
  server = argv[1];
  
  ports.push_back(&pIn);
  
  flowvrmodule = initModule(ports);
  if (flowvrmodule == NULL)
    return 1;

  vrpn_ForceDevice_Remote *forceDevice;
  forceDevice = new vrpn_ForceDevice_Remote(server);
  
  float* _Origin = (float*) malloc(3*sizeof(float));  
  
  while (flowvrmodule->wait()) {    
    forceDevice->mainloop();    	
    flowvr::Message msg;
    flowvrmodule->get(&pIn,msg);
    forces2vrpn msg2vrpn;
    msg2vrpn.VRPNMsg(msg);
    
    for (vector<_VRPNMsg>::iterator it = (msg2vrpn.MessageVRPN).begin(); it != (msg2vrpn.MessageVRPN).end(); it++) {
      (*it).ApplyVRPNFunc(forceDevice);
    }    
  }  
  flowvrmodule->close();
}
