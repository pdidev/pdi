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
#include <flowvr/app/components/metamoduleflowvr-run-ssh-parallel.comp.h>

// primes specific components includes
#include "primes/components/modulecompute.comp.h"

#ifndef _METAMODULECOMPUTE_H_
#define _METAMODULECOMPUTE_H_

using namespace flowvr::app;

namespace primes {

class MetaModuleCompute : public MetaModuleFlowvrRunSSHParallel<ModuleCompute>
{
private :
public :
  //Interface construction : MetaModuleFlowvrRunSSHSingleton clones Compute interface
  MetaModuleCompute(const std::string& id_ ) : MetaModuleFlowvrRunSSHParallel<ModuleCompute>(id_, CmdLine("compute")) 
  {
    setInfo("Metamodule launching compute modules");
  };
	
  //Mandatory virtual destructor
  virtual ~MetaModuleCompute(){};

  // Mandatory create  method
  virtual Component* create() const { return new MetaModuleCompute(getId());};

};

};

#endif //_METAMODULECOMPUTE_H_
