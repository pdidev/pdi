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
 *  Contact : Jean-Denis.Lesage@imag.fr                            *
 *                                                                 *
 ******************************************************************/

// basic components includes
#include <flowvr/app/components/module.comp.h>

#ifndef _MODULEPUTFCA_H_
#define _MODULEPUTFCA_H_

using namespace flowvr::app;

namespace tictacfca {

class ModulePutFca : public Module 
{
public :
  ModulePutFca(const std::string& id_) : Module(id_)
  {

    setInfo(" Alternatively produce a tic, tac message sent on its output port");

    // Interface declaration
    addPort("text", OUTPUT);
    // Class Module has created beginIt and endIt ports 
    myTraceList.push_back(std::string("beginTrace"));
    myTraceList.push_back(std::string("endTrace"));
  };

  // Mandatory create method
  virtual Component* create() const { return new ModulePutFca(getId());};
};

};
#endif //_MODULEPUTFCA_H_
