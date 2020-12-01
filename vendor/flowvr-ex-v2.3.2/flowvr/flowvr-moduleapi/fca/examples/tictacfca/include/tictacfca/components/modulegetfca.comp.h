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

#ifndef _MODULEGETFCA_H_
#define _MODULEGETFCA_H_

using namespace flowvr::app;

namespace tictacfca {

class ModuleGetFca : public Module 
{
public :
  ModuleGetFca(const std::string& id_) : Module(id_)
  {

    setInfo("Receive and print  messages received on its  input  port");

    // Interface declaration
    addPort("text", INPUT);
    // Class Module has created beginIt and endIt ports 
  };

  // Mandatory create method
  virtual Component* create() const { return new ModuleGetFca(getId());};
};

};
#endif //__MODULEGETFCA_H_
