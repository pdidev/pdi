#include <flowvr/app/components/module.comp.h>

#ifndef _MODULEFLOWVR2VRPN_H_
#define _MODULEFLOWVR2VRPN_H_

using namespace flowvr::app;

namespace flowvrvrpn{
  
  class Moduleflowvr2vrpn : public Module {
    public :
      Moduleflowvr2vrpn(const std::string& id_) : Module(id_) {

      setInfo("VRPN Client that receives vrpn messages and send them to VRPN server");
      
      // Interface declaration
      addPort("vrpnmsg", INPUT);
      
      // Class Module has created beginIt and endIt ports 

    };
    
    // Mandatory create method
    virtual Component* create() const { return new Moduleflowvr2vrpn(getId());};
  };
  
};
#endif 
