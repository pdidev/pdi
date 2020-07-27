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


#ifndef _MODULESENDER_H_
#define _MODULESENDER_H_

#include <flowvr/app/components/module.comp.h>


namespace FiniteFilterQueueTest {

class ModuleSender : public flowvr::app::Module
{
public :
	ModuleSender(const std::string& id_) : flowvr::app::Module(id_)
  {

    setInfo("send a static message");

    // Interface declaration
    addPort("out", flowvr::app::OUTPUT);
    // Class Module has created beginIt and endIt ports
  }

  // Mandatory create method
  virtual Component* create() const { return new ModuleSender(getId()); }
};

}
#endif //_MODULESENDER_H_
