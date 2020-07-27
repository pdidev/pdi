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
* File: ./include/flowvr-vrpn/components/modulevrpn2flowvr.comp.h *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include <flowvr/app/components/module.comp.h>

#ifndef _MODULEVRPN2FLOWVR_H_
#define _MODULEVRPN2FLOWVR_H_

using namespace flowvr::app;

namespace flowvrvrpn{

class Modulevrpn2flowvr : public Module 
{
public :
  Modulevrpn2flowvr(const std::string& id_) : Module(id_)
  {

    setInfo("VRPN Client that receives all VRPN input messages from the server and sends them to FlowVR");

    // Interface declaration
    addPort("vrpnmsg", OUTPUT);

    // Class Module has created beginIt and endIt ports 
  };

  // Mandatory create method
  virtual Component* create() const { return new Modulevrpn2flowvr(getId());};
};

};
#endif 
