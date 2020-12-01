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

#ifndef _METAMODULERECEIVER_H_
#define _METAMODULERECEIVER_H_

#include "modulereceiver.comp.h"
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>



namespace FiniteFilterQueueTest
{
	class MetaModuleReceiver : public flowvr::app::MetaModuleFlowvrRunSSHSingleton<ModuleReceiver>
	{
	public :

		MetaModuleReceiver(const std::string& id_ )
		: flowvr::app::MetaModuleFlowvrRunSSHSingleton<ModuleReceiver>(id_,flowvr::app::CmdLine("ModuleReceiver"))
	  {
		setInfo("Metamodule Receiver launches the ModuleReceiver module");

		// control the blocking state of the receiver's in port
		// (true: blocking, false: non-blocking)
		addParameter<bool>("blocking-in", true);
		addParameter<int>("request", 1);
	  }

	  virtual void configure()
	  {
		  bool bBlockingIn = getParameter<bool>("blocking-in");
		  if(bBlockingIn)
			  getRun()->addArg("--blocking-in");

		  int nRequest = getParameter<int>("request");
		  if(nRequest)
			  getRun()->addArg("--request=" + getParameterT("request"));
	  }

	  // Mandatory create  method
	  virtual Component* create() const { return new MetaModuleReceiver(getId()); }
	};

}

#endif //_METAMODULERECEIVER_H_
