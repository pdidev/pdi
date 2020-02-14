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
 * File: ./examples/sphere_joypad/src/sphere_joypad.comp.cpp       *
 *                                                                 *
 * Contacts:                                                       *
 *  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
 *                                                                 *
 ******************************************************************/
#include "flowvr/app/core/genclass.h"
#include "sphere_phantom/components/metamoduleviewer_phantom.comp.h"
#include "sphere_phantom/components/Phantom.comp.h"  
#include "sphere_phantom/components/sphere_phantom.comp.h"    
#include "sphere_phantom/components/metamoduleforcegeneration.comp.h"    
#include <flowvr/app/components/flowvr-app.comp.h> 

using namespace flowvr::app;

namespace sphere_phantom {
  
  GENCLASS(sphere_phantom)
      
      void sphere_phantom::execute() {
      
      Component* device = addObject<Phantom>("device");
      
      MetaModuleViewer_phantom* viewer = addObject<MetaModuleViewer_phantom>("viewer");
      
      typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterIt,SyncGreedy> Greedy; 
      typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterMergeIt,SyncGreedy> Merge; 

      
      Merge* ganalog1  = addObject<Merge>("ganalog1");       
      link(device->getPort("tracker"),ganalog1->getPort("in"));
      link(ganalog1->getPort("out"),viewer->getPort("Analog"));
      link(viewer->getPort("endIt"),ganalog1->getPort("sync"));
      
      Merge* gbutton  = addObject<Merge>("gbutton"); 
      link(device->getPort("buttons"),gbutton->getPort("in"));
      link(gbutton->getPort("out"),viewer->getPort("Button"));
      link(viewer->getPort("endIt"),gbutton->getPort("sync"));

      MetaModuleForcegeneration* forcegeneration = addObject<MetaModuleForcegeneration>("forcegeneration");      
      
      Merge* ganalog2  = addObject<Merge>("ganalog2");       
      link(device->getPort("tracker"),ganalog2->getPort("in"));
      link(ganalog2->getPort("out"),forcegeneration->getPort("Tracker"));
      link(forcegeneration->getPort("endIt"),ganalog2->getPort("sync"));
      
      Merge* ganalog3  = addObject<Merge>("ganalog3"); 
      link(forcegeneration->getPort("Force"),ganalog3->getPort("in"));
      link(ganalog3->getPort("out"),device->getPort("force"));
      link(viewer->getPort("endIt"),ganalog3->getPort("sync"));

  };    
};
