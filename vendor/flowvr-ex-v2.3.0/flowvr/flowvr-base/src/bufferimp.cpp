/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: bufferimp.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include <flowvr/bufferimp.h>

namespace flowvr
{

	BufferImp::~BufferImp()
	{

	}

	bool BufferImp::uniqueOwner() const
	{
		return (getCount() == 1);
	}

}
