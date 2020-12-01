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

#ifndef MENUSOURCE_COMP_H_
#define MENUSOURCE_COMP_H_

#include <flowvr/app/core/component.h>

namespace menusource
{
	class MenuSource : public flowvr::app::Composite
	{
	public:
		MenuSource( const std::string &id_ );
		~MenuSource();

		virtual void execute();

		Component *create() const;
	};
}


#endif // MENUSOURCE_COMP_H_
