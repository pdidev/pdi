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
 * File: ./include/flowvr/app/components/flowvr-render/metamodulejoypad.comp.h*
 *                                                                 *
 * Contacts: benjamin.petit@inrialpes.fr                           *
 *                                                                 *
 ******************************************************************/

#ifndef METAMODULEJOYPAD_COMP_H_
#define METAMODULEJOYPAD_COMP_H_

#include "flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h"
#include "flowvr/contrib/components/modulejoypad.comp.h"

namespace flowvr {	namespace render { 

    class MetaModuleJoypad : public MetaModuleFlowvrRunSSHSingleton<ModuleJoypad>
    {
        public:
            MetaModuleJoypad(const std::string& id_) :
                MetaModuleFlowvrRunSSHSingleton<ModuleJoypad>(id_, CmdLine("joypad") )
        {
        }

            virtual Component* create() const
            {
                return new MetaModuleJoypad(getId());
            }

            virtual void configure(){

            }

    };

} }

#endif /*METAMODULEJOYPAD_COMP_H_*/
