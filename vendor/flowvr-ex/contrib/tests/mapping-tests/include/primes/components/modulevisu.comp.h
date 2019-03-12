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

#ifndef _MODULEVISU_H_
#define _MODULEVISU_H_

using namespace flowvr::app;

namespace primes{

class ModuleVisu : public Module 
{
public :
  ModuleVisu(const std::string& id_) : Module(id_)
  {
    setInfo("Module visu render received data on a disk");

    // Interface declaration
    addPort("keysIn", INPUT);
    addPort("primesIn", INPUT);
    // beginIt and endIt ports created by the Module class
    
    //declare the user traces to be added to this module:
    myTraceList.push_back(std::string("myTraceKeyPressedNotification"));
  };


  //Mandatory virtual destructor
  virtual ~ModuleVisu(){};

  // Mandatory create method
  virtual Component* create() const { return new ModuleVisu(getId());};
};

};
#endif //_MODULEVISU_H_
