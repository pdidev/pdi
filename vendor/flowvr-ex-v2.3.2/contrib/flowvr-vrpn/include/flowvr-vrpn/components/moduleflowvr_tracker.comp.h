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
* File: ./include/flowvr-vrpn/components/moduleflowvr_tracker.comp.h*
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include <flowvr/app/components/module.comp.h>

#ifndef _MODULEFLOWVRTRACKER_H_
#define _MODULEFLOWVRTRACKER_H_

using namespace flowvr::app;

namespace flowvrvrpn {

class Moduleflowvr_tracker : public Module 
{
public :
  Moduleflowvr_tracker(const std::string& id_) : Module(id_)
  {

    setInfo("OpenGL et envoi d'un événement");

    // Interface declaration
    addPort("vrpn_tracker", INPUT);
    addPort("ftl_tracker", OUTPUT);
    // Class Module has created beginIt and endIt ports 
  };

  // Mandatory create method
  virtual Component* create() const { return new Moduleflowvr_tracker(getId());};
};

};
#endif 
