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

#ifndef _MODULECAPTURE_H_
#define _MODULECAPTURE_H_

using namespace flowvr::app;

namespace primes{

class ModuleCapture : public Module 
{
public :
  ModuleCapture(const std::string& id_) : Module(id_)
  {

    setInfo("Module capture sent key pressed and mouse events on output port");

	  // Interface declaration
    addPort("keysOut", OUTPUT);
	 // Class Module has created beginIt and endIt ports 
  };

  //Mandatory virtual destructor
  virtual ~ModuleCapture(){};

  // Mandatory create method
  virtual Component* create() const { return new ModuleCapture(getId());};
};

};
#endif //_MODULECAPTURE_H_
