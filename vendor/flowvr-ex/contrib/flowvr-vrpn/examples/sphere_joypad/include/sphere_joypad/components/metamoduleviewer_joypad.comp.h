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

#include "moduleviewer_joypad.comp.h"

#ifndef _METAMODULEVIEWERJOYPAD_H_
#define _METAMODULEVIEWERJOYPAD_H_

using namespace flowvr::app;

namespace sphere_joypad {

  class MetaModuleViewer_joypad : public MetaModuleFlowvrRunSSHSingleton<ModuleViewer_joypad>
    {
      public :

	MetaModuleViewer_joypad(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModuleViewer_joypad>(id_,CmdLine("viewer_joypad")) 
	{
	  setInfo("Metamodule that illustrates an OpenGL application controlled by a joypad");
	  getRun()->openDisplay(":0");
	};
      
      
      // virtual destructor
      virtual ~MetaModuleViewer_joypad(){};
      
      // Mandatory create  method
      virtual Component* create() const { return new MetaModuleViewer_joypad(getId());};
    };  
};

#endif 
