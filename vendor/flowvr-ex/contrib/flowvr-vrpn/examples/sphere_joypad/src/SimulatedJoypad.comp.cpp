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
 * This source is covered by the GNU GPL, please refer to the      *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Sebastien Limet,                                             *
 *    Sophie Robert.                                               *
 *                                                                 * 
 *******************************************************************
 *                                                                 *
 * File: ./examples/sphere_joypad/src/MetaModuleJoypad.comp.cpp    *
 *                                                                 *
 * Contacts:                                                       *
 *  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
 *                                                                 *
 ******************************************************************/
#include <flowvr/app/components/connection.comp.h>
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>
#include <flowvr-vrpn/components/modulevrpn2flowvr.comp.h>
#include <flowvr-vrpn/components/moduleflowvr_button.comp.h>
#include <flowvr-vrpn/components/moduleflowvr_analog.comp.h>


#include "sphere_joypad/components/SimulatedJoypad.comp.h"

using namespace flowvr;
using namespace flowvr::app;
using namespace flowvrvrpn;

  
void SimulatedJoypad::execute() {
    
    Component* vrpn2flowvr = addObject(MetaModuleFlowvrRunSSHSingleton<Modulevrpn2flowvr>("vrpn2flowvr", CmdLine("simulateddevice joypad.xml")));
    Component* button      = addObject(MetaModuleFlowvrRunSSHSingleton<Moduleflowvr_button>("flowvr_button", CmdLine("flowvr_button")));
    Component* analog      = addObject(MetaModuleFlowvrRunSSHSingleton<Moduleflowvr_analog>("flowvr_analog", CmdLine("flowvr_analog")));
    
    
    addObjectandLink<Connection>("cbutton",vrpn2flowvr->getPort("vrpnmsg"),button->getPort("vrpn_button"));
    addObjectandLink<Connection>("canalog",vrpn2flowvr->getPort("vrpnmsg"),analog->getPort("vrpn_analog"));
    
    link((button->getPort("ftl_button")), (getPort("buttons")));          
    link((analog->getPort("ftl_analog")), (getPort("analog")));          
}  
