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

// flowvr-app core includes
#include <flowvr/app/core/genclass.h>

// basic components includes
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>

// primes specific components includes
#include "primes/components/modulecapture.comp.h"

#ifndef _METAMODULECAPTURE_H_
#define _METAMODULECAPTURE_H_

using namespace flowvr::app;

namespace primes {

class MetaModuleCapture : public MetaModuleFlowvrRunSSHSingleton<ModuleCapture>
{
public :

  //Interface construction : MetaModuleSingleDisplay clones Capture interface
 MetaModuleCapture(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModuleCapture>(id_, CmdLine("capture")) 
  {
    setInfo("Metamodule launching capture  modules");

    getRun()->openDisplay(":0");
  };

  //Mandatory virtual destructor
  virtual ~MetaModuleCapture(){};
	
  // Mandatory create method
  virtual Component* create() const { return new MetaModuleCapture(getId());};
};

};

#endif //_METAMODULECAPTURE_H_
