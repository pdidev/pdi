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


#ifndef _MODULERECEIVER_H_
#define _MODULERECEIVER_H_

#include <flowvr/app/components/module.comp.h>


namespace FiniteFilterQueueTest
{

	class ModuleReceiver : public flowvr::app::Module
	{
	public :
	  ModuleReceiver(const std::string& id_) : flowvr::app::Module(id_)
	  {
		setInfo("receive a message, and utter a trigger.");

		// this is the 'request' for new messages
		addPort("trigger", flowvr::app::OUTPUT, flowvr::app::STAMPS);
		addPort("in", flowvr::app::INPUT, flowvr::app::FULL);
	  }

	  // Mandatory create method
	  virtual Component* create() const { return new ModuleReceiver(getId()); }
	};

}
#endif //_MODULERECEIVER_H_
