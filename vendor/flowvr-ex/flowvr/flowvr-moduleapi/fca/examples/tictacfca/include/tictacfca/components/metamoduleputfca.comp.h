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
#include "tictacfca/components/moduleputfca.comp.h"

#ifndef _METAMODULEPUTFCA_H_
#define _METAMODULEPUTFCA_H_

using namespace flowvr::app;

namespace tictacfca {


class MetaModulePutFca : public MetaModuleFlowvrRunSSHSingleton<ModulePutFca>
{
public :

 MetaModulePutFca(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModulePutFca>(id_,CmdLine("putfca")) 
  {
    setInfo("Metamodule PutFca launches the putfca module");
  };



  //Mandatory virtual destructor
  virtual ~MetaModulePutFca(){};

  // Mandatory create  method
  virtual Component* create() const { return new MetaModulePutFca(getId());};
};

};

#endif //_METAMODULEPUTFCA_H_
