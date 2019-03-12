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
 * File: ./examples/sphere_phantom/src/MetaModulePhantom.comp.cpp    *
 *                                                                 *
 * Contacts:                                                       *
 *  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
 *                                                                 *
 ******************************************************************/
#include <flowvr/app/components/connection.comp.h>
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>
#include <flowvr-vrpn/components/modulevrpn2flowvr.comp.h>
#include <flowvr-vrpn/components/moduleflowvr_button.comp.h>
#include <flowvr-vrpn/components/moduleflowvr_tracker.comp.h>

#include <flowvr-vrpn/components/moduleflowvr2vrpn.comp.h>
#include <flowvr-vrpn/components/moduleflowvr_forcefield.comp.h>


#include "sphere_phantom/components/Phantom.comp.h"

using namespace flowvr;
using namespace flowvr::app;
using namespace flowvrvrpn;

void Phantom::execute() {
    
    Component* vrpn2flowvr = addObject(MetaModuleFlowvrRunSSHSingleton<Modulevrpn2flowvr>("vrpn2flowvr", CmdLine("vrpn2flowvr Phantom@tcp://vrpn_server")));
    Component* button      = addObject(MetaModuleFlowvrRunSSHSingleton<Moduleflowvr_button>("flowvr_button", CmdLine("flowvr_button")));
    Component* tracker     = addObject(MetaModuleFlowvrRunSSHSingleton<Moduleflowvr_tracker>("flowvr_tracker", CmdLine("flowvr_tracker")));
    
    
    addObjectandLink<Connection>("cbutton",vrpn2flowvr->getPort("vrpnmsg"),button->getPort("vrpn_button"));
    addObjectandLink<Connection>("ctracker",vrpn2flowvr->getPort("vrpnmsg"),tracker->getPort("vrpn_tracker"));
    
    link((button->getPort("ftl_button")), (getPort("buttons")));          
    link((tracker->getPort("ftl_tracker")), (getPort("tracker")));                  
    

    Component* flowvr2vrpn = addObject(MetaModuleFlowvrRunSSHSingleton<Moduleflowvr2vrpn>("flowvr2vrpn", CmdLine("flowvr2vrpn Phantom@tcp://vrpn_server")));
    Component* forcefield = addObject(MetaModuleFlowvrRunSSHSingleton<ModuleFlowvr_ForceField>("flowvr_forcefield", CmdLine("flowvr_forcefield")));
    
    addObjectandLink<Connection>("cforce",forcefield->getPort("vrpn_force"),flowvr2vrpn->getPort("vrpnmsg"));
    
    link((getPort("force")),(forcefield->getPort("force")));
        
}  
