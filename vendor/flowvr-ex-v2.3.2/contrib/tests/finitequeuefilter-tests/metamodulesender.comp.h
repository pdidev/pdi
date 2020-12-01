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

#ifndef _METAMODULESENDER_H_
#define _METAMODULESENDER_H_

#include "modulesender.comp.h"
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>



namespace FiniteFilterQueueTest {


class MetaModuleSender : public flowvr::app::MetaModuleFlowvrRunSSHSingleton<ModuleSender>
{
public :

  MetaModuleSender(const std::string& id_ )
  : flowvr::app::MetaModuleFlowvrRunSSHSingleton<ModuleSender>(id_,flowvr::app::CmdLine("ModuleSender"))
  {
    setInfo("Metamodule Sender launches the ModuleSender module");
  }

  // Mandatory create  method
  virtual Component* create() const { return new MetaModuleSender(getId()); }
};

};

#endif //_METAMODULESENDER_H_
