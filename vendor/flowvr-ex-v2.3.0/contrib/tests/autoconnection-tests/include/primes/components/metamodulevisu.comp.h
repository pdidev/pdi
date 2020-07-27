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
#include "primes/components/modulevisu.comp.h"

#ifndef _METAMODULEVISU_H_
#define _METAMODULEVISU_H_

using namespace flowvr::app;

namespace primes {

class MetaModuleVisu : public MetaModuleFlowvrRunSSHSingleton<ModuleVisu>
{
public :
	//Interface construction : MetaModuleSingleDisplay clones Visu interface
  //  MetaModuleVisu(const std::string& id_ ) : MetaModuleSingleDisplay<ModuleVisu>(id_, "visu") {};
 MetaModuleVisu(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModuleVisu>(id_, CmdLine("visu") ) 
  {
    setInfo("Metamodule launching visu  modules");

    getRun()->openDisplay(":0"); 
  };

  //Mandatory virtual destructor
  virtual ~MetaModuleVisu(){};
	
  // Mandatory create  method
  virtual Component* create() const { return new MetaModuleVisu(getId());};
};

};

#endif //_METAMODULEVISU_H_
