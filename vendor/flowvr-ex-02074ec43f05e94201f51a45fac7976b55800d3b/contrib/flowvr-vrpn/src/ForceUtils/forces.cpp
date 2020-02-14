#include "flowvr-vrpn/core/forces.h"
#include "flowvr-vrpn/core/vrpnwrapper.h"

void fieldforces::put(OutputPort* port) {
  MessageWrite msg;
  msg.data = port->getModule()->alloc(16*sizeof(float));
  
  float* AEcrire;
  AEcrire = (float*) malloc(16*sizeof(float));
  
  for (int i=0; i<3; i++) 
    AEcrire[i] = (this->getFFOrigin())[i];
  for (int i=0; i<3; i++)
    AEcrire[3+i] = (this->getFFForce())[i];
  for (int i=0; i<3; i++)
    for (int j=0; j<3; j++) 
      AEcrire[6+3*i+j] = (this->getFFJacobian())[3*i+j];
  AEcrire[15] = (this->getFFRadius());
  
  memcpy((void*)msg.data.writeAccess(), (void*)AEcrire,16*sizeof(float));
  
  port->getModule()->put(port,msg);  
  delete[] AEcrire;
}

void fieldforces::ForceRecv(InputPort* port) {
  Message msg;
  port->getModule()->get(port,msg);
  float* ALire = new float(16);
  ALire = (float*)msg.data.getRead<float>();
  this->setFFOrigin(ALire);
  this->setFFForce(ALire+3);
  this->setFFJacobian(ALire+6);
  this->setFFRadius(ALire[15]);  
  delete[] ALire;  
}

void fieldforces::ForceMsg(Message msg) {
  float* ALire;
  ALire = (float*)msg.data.getRead<float>();
  this->setFFOrigin(ALire);
  this->setFFForce(ALire+3);
  this->setFFJacobian(ALire+6);
  this->setFFRadius(ALire[15]);
}


void forces2vrpn::put(OutputPort* port) {
  MessageWrite msg;
  int len = getLength();  

  msg.data = port->getModule()->alloc(getSize()+sizeof(int));
  memcpy((void*)msg.data.writeAccess(), (void*)&len,sizeof(int));
  
  int size = sizeof(int);
  for (vector<_VRPNMsg>::iterator it = (this->MessageVRPN).begin(); it != (this->MessageVRPN).end(); it++) {
    memcpy((void*)(msg.data.writeAccess()+size), (void*)&((*it).type), sizeof(int));
    size += sizeof(int);
    memcpy((void*)(msg.data.writeAccess()+size), (void*)&((*it).n), sizeof(int));
    size += sizeof(int);
    memcpy((void*)(msg.data.writeAccess()+size), (void*)(*it).data, (*it).n*sizeof(float));
    size += (*it).n*sizeof(float);    
  }
  port->getModule()->put(port,msg);    
}

void forces2vrpn::VRPNMsg(Message msg) {
  int size = msg.data.getSize();
  int t = 0;
  const void* ALire = (void*)malloc(size);
  int len;
  ALire = msg.data.getRead<void>(0);
  memcpy((void*)&len,ALire,sizeof(int));
  t+=sizeof(int);
  
  for (int i=0; i<len; i++) {
    FeedbackForceMessage type;
    ALire = msg.data.getRead<void>(t);
    t+=sizeof(int);
    memcpy((void*)&type,ALire,sizeof(int));
    
    int n;
    ALire = msg.data.getRead<void>(t);
    t+=sizeof(int);
    memcpy((void*)&n,ALire,sizeof(int));
    
    float* tab = (float*) malloc(n*sizeof(float));
    ALire = msg.data.getRead<void>(t);
    memcpy((void*)tab, ALire, n*sizeof(float));
    t+=n*sizeof(float);
    
    this->AddMessage(type,n,tab);
    delete[] tab;
  }
}

typedef void (* vrpnwrapper)(float* , vrpn_ForceDevice_Remote*);
typedef map<int, vrpnwrapper> function_map;


void _VRPNMsg::ApplyVRPNFunc(vrpn_ForceDevice_Remote* fdr) {
  int type = this->type;
  float* data = this->data;
  
  //  static std::map<int::type, vrpnwrapper> funcmap; 
  //  static std::map<int::type, string> funcmap; 
  function_map funcmap;
  
  if(funcmap.empty()) {
    funcmap[ORIGIN] = _setFF_Origin;
    funcmap[FORCE] = _setFF_Force;
    funcmap[JACOBIAN] = _setFF_Jacobian;
    funcmap[RADIUS] = _setFF_Radius;
    funcmap[SENDFORCE] = _sendForceField;
    funcmap[STOPFORCE] = _stopForceField;
  }
  
  function_map::const_iterator i = funcmap.find(type);
  if (i == funcmap.end() ) 
    std::cout<<"unknown FeedbackForceMessage : " << endl;
  else 
    (i->second)(data, fdr);
}
