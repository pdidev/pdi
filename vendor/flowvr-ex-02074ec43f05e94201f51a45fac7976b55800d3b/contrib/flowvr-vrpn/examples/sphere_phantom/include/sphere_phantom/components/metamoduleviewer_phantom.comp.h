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
* File: ./examples/sphere_joypad/include/sphere_joypad/components/metamoduleviewer_joypad.comp.h*
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>

#include "moduleviewer_phantom.comp.h"

#ifndef _METAMODULEVIEWERPHANTOM_H_
#define _METAMODULEVIEWERPHANTOM_H_

using namespace flowvr::app;

namespace sphere_phantom {
    
    class MetaModuleViewer_phantom : public MetaModuleFlowvrRunSSHSingleton<ModuleViewer_phantom>
    {
        public :
        
	MetaModuleViewer_phantom(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModuleViewer_phantom>(id_,CmdLine("viewer_phantom")) 
            {
                setInfo("Metamodule that illustrates an OpenGL application controlled by a joypad");
                getRun()->openDisplay(":0");
            };
        
        
        // virtual destructor
        virtual ~MetaModuleViewer_phantom(){};
        
        // Mandatory create  method
        virtual Component* create() const { return new MetaModuleViewer_phantom(getId());};
    };  
};

#endif 
