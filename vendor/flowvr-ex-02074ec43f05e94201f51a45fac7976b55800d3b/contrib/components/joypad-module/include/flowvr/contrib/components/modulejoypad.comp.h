/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                     Application Library                         *
 *                                                                 *
 *-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
 * INRIA                                                           *
 * ALL RIGHTS RESERVED.	                                          *
 *                                                                 *
 * This source is covered by the GNU LGPL, please refer to the     *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Jeremie Allard,                                              *
 *    Thomas Arcila,                                               *
 *    Jean-Denis Lesage.                                           *	
 *    Clement Menier,                                              *
 *    Bruno Raffin                                                 *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: ./include/flowvr/app/components/flowvr-render/modulejoypad.comp.h*
 *                                                                 *
 * Contacts: benjamin.petit@inrialpes.fr                           *
 *                                                                 *
 ******************************************************************/
#ifndef MODULEJOYPAD_COMP_H_
#define MODULEJOYPAD_COMP_H_

#include <flowvr/app/components/module.comp.h>

#define NB_BUTTON       11
#define NB_AXE          7

namespace flowvr { namespace render {

    class ModuleJoypad : public Module
    {
        public:
            ModuleJoypad(const std::string& id_) :
                Module(id_)
        {		    
            setInfo("Module to output a joypad commands");

            for(unsigned int i=0;i<NB_BUTTON;i++)
            {
                addPort("bt"+toString<unsigned int>(i), OUTPUT);
                addPort("s_bt"+toString<unsigned int>(i), OUTPUT);
            }
            for(unsigned int i=0;i<NB_AXE;i++)
            {
                addPort("axe"+toString<unsigned int>(i), OUTPUT);
                addPort("s_axe"+toString<unsigned int>(i), OUTPUT);
            }

        }

            virtual Component* create() const
            {
                return new ModuleJoypad(getId());
            }
    };

} }

#endif /*MODULEJOYPAD_COMP_H_*/
