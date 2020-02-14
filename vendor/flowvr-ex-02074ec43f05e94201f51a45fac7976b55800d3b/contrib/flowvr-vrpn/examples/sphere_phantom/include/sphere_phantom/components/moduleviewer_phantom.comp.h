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
* File: ./examples/sphere_joypad/include/sphere_joypad/components/moduleviewer_joypad.comp.h*
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include <flowvr/app/components/module.comp.h>

#ifndef _MODULEVIEWERPHANTOM_H_
#define _MODULEVIEWERPHANTOM_H_

using namespace flowvr::app;

namespace sphere_phantom 
{
    class ModuleViewer_phantom : public Module 
    {
        public :
	ModuleViewer_phantom(const std::string& id_) : Module(id_)
	{
            
            setInfo("The Sphere Example Viewer");
            
            addPort("Analog", INPUT);
            addPort("Button", INPUT);
            
	};
        
        virtual Component* create() const { return new ModuleViewer_phantom(getId());
      };
    };
};
#endif 
