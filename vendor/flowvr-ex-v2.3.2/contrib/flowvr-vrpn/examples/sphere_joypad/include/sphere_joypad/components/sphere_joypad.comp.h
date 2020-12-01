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
* File: ./examples/sphere_joypad/include/sphere_joypad/components/sphere_joypad.comp.h*
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
/** cube application*/

// basic components includes
#include <flowvr/app/components/connection.comp.h>

#ifndef _SPHEREJOYPAD_H_
#define _SPHEREJOYPAD_H_

using namespace flowvr::app;

namespace sphere_joypad {
    
    class sphere_joypad: public Composite {
        public :
        
        //  Constructor with id (required so this component can be dynamically loaded)
        sphere_joypad(const std::string id_): Composite(id_)
        {
            setInfo("OpenGL application that displays spheres controlled by ftl input messages");
        };
        
        
        // virtual destructor
        virtual ~sphere_joypad(){};
        
        
        // Composite components need an execute method. The network of cube application is defined in this method.
        virtual void execute();
        
        // Mandatory create method
        virtual Component* create() const { return new sphere_joypad(this->getId()); };
        
    };    
};

#endif 
