#include <flowvr/app/components/module.comp.h>

#ifndef _MODULEFLOWVRFORCEFIELD_H_
#define _MODULEFLOWVRFORCEFIELD_H_

using namespace flowvr::app;

namespace flowvrvrpn {
  
  class ModuleFlowvr_ForceField : public Module {
    public :
      ModuleFlowvr_ForceField(const std::string& id_) : Module(id_) {
      
      setInfo("Receive ftl force feedback messages and translates them to VRPN ones");
      
      // Interface declaration
      addPort("force", INPUT);
      addPort("vrpn_force", OUTPUT);
      // Class Module has created beginIt and endIt ports 
    };
    
    // Mandatory create method
    virtual Component* create() const { return new ModuleFlowvr_ForceField(getId());};
  };
  
};
#endif 
