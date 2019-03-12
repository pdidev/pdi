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

#ifndef _MODULECOMPUTE_H_
#define _MODULECOMPUTE_H_

using namespace flowvr::app;

namespace primes{

class ModuleCompute : public Module 
{
public :
  ModuleCompute(const std::string& id_) : Module(id_)
  {
    setInfo("Module cmputing PI decimals and sending them on output port");

    // Interface declaration
    addPort("primesOut", OUTPUT);
    // Class Module has created beginIt and endIt ports 
  };


  //Mandatory virtual destructor
  virtual ~ModuleCompute(){};


  // Mandatory create method
  virtual Component* create() const { return new ModuleCompute(getId());};
};

};
#endif //_MODULECOMPUTE_H_
