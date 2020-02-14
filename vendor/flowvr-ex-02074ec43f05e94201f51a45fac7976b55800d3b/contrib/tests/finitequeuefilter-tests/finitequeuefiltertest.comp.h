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
 *  Contact :                                                      *
 *                                                                 *
 ******************************************************************/


#ifndef _FINITEFILTERQUEUETESTCOMP_H_
#define _FINITEFILTERQUEUETESTCOMP_H_

#include <flowvr/app/core/component.h>

namespace FiniteFilterQueueTest
{
	class FiniteFilterQueueTest : public flowvr::app::Composite
	{
	public :
		/**
		 * the constructor will pass the id to its base class
		 * and setup the description of this composite.
		 */
	  FiniteFilterQueueTest(const std::string &id_);

	  /// the execute method is called during construction
	  /// of this example and will setup the network
	  virtual void execute();

	  // Mandatory create method
	  virtual flowvr::app::Component* create() const;

	};
} // end of namespace FiniteFilterQueueTest

#endif //_FINITEFILTERQUEUETESTCOMP_H_
