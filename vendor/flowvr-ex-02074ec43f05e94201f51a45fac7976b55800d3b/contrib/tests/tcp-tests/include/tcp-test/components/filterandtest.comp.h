/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                                                                 *
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

#ifndef FILTERANDTEST_COMP_H
#define FILTERANDTEST_COMP_H

#include <flowvr/app/core/component.h>

namespace tcptest
{
	class FilterAndTest : public flowvr::app::Composite
	{
	public:
		FilterAndTest( const std::string &id );

		virtual void execute();
		virtual Component *create() const;
	private:
	};
}




#endif // FILTERANDTEST_COMP_H
