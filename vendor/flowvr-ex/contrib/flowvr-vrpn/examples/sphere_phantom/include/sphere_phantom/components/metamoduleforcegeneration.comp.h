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
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING-LIB file for further information.                       *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Sebastien Limet,                                             *
*    Sophie Robert.                                               *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: ./examples/sphere_phantom/include/sphere_phantom/components/metamoduleforcegeneration.comp.h*
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>

#include "moduleforcegeneration.comp.h"

#ifndef _METAMODULEFORCEGENERATION_H_
#define _METAMODULEFORCEGENERATION_H_

using namespace flowvr::app;

namespace sphere_phantom {

  class MetaModuleForcegeneration : public MetaModuleFlowvrRunSSHSingleton<ModuleForcegeneration>
    {
      public :

	MetaModuleForcegeneration(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModuleForcegeneration>(id_,CmdLine("force_generation")) 
	{
	  setInfo("Metamodule Forcegeneration for the Sphere Example Forcegeneration");
	};
      
      
      // virtual destructor
      virtual ~MetaModuleForcegeneration(){};
      
      // Mandatory create  method
      virtual Component* create() const { return new MetaModuleForcegeneration(getId());};
    };  
};

#endif 
