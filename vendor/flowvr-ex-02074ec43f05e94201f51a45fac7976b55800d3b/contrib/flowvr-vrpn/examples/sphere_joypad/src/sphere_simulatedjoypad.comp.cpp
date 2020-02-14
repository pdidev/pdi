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
#include "sphere_joypad/components/metamoduleviewer_joypad.comp.h"
#include "sphere_joypad/components/Joypad.comp.h"  
#include "sphere_joypad/components/SimulatedJoypad.comp.h"  
#include "sphere_joypad/components/sphere_joypad.comp.h"    
#include <flowvr/app/components/flowvr-app.comp.h> 

using namespace flowvr::app;

namespace sphere_joypad {
  
  GENCLASS(sphere_joypad)
      
      void sphere_joypad::execute() {
      

      addParameter("device");
      setParameter("device","Joypad");
      std::string param = getParameter<std::string>("device");

      Component* device;

      if (param=="Joypad") {
          std::cout << "joypad" << std::endl;
          device = addObject<Joypad>("device");
      }
      else {
          std::cout << "simulated joypad" << std::endl;
          device = addObject<SimulatedJoypad>("device");
      }
      
      MetaModuleViewer_joypad* viewer = addObject<MetaModuleViewer_joypad>("viewer");
      
      typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterIt,SyncGreedy> Greedy; 
      typedef ComSync<Connection,ConnectionStamps,Connection,ConnectionStamps,FilterMergeIt,SyncGreedy> Merge; 
      
      Greedy* ganalog  = addObject<Greedy>("ganalog"); 
      //      Merge* ganalog  = addObject<Merge>("ganalog"); 
      link(device->getPort("analog"),ganalog->getPort("in"));
      link(ganalog->getPort("out"),viewer->getPort("Analog"));
      link(viewer->getPort("endIt"),ganalog->getPort("sync"));
      
      Merge* gbutton  = addObject<Merge>("gbutton"); 
      link(device->getPort("buttons"),gbutton->getPort("in"));
      link(gbutton->getPort("out"),viewer->getPort("Button"));
      link(viewer->getPort("endIt"),gbutton->getPort("sync"));
  };    
};
