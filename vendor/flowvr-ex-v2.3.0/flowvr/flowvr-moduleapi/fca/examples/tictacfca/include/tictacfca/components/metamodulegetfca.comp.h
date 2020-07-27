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
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>

// tictac specific components includes
#include "tictacfca/components/modulegetfca.comp.h"

#ifndef _METAMODULEGETFCA_H_
#define _METAMODULEGETFCA_H_

using namespace flowvr::app;

namespace tictacfca {



class MetaModuleGetFca : public MetaModuleFlowvrRunSSHSingleton<ModuleGetFca>
{
public :
	//Interface construction : MetaModuleSingle clones Get interface
  MetaModuleGetFca(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModuleGetFca>(id_,CmdLine("getfca")) 
  {
    setInfo("Metamodule GetFca launches the getfca module");

  };
	

  // virtual destructor
  virtual ~MetaModuleGetFca(){};


  // Mandatory create  method
  virtual Component* create() const { return new MetaModuleGetFca(getId());};
};


};

#endif //_METAMODULEGETFCA_H_
