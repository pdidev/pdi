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
* File: allocator.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include "flowvr/allocator.h"
#include <flowvr/ipc/atomic.h>

namespace
{
	flowvr::Allocator* DefaultAllocator = 0L;
	flowvr::Allocator* UserSet = 0L;

	flowvr::ipc::MTAtomicInt 	refCnt(0);
}


namespace flowvr
{

	Allocator *Allocator::getDefaultAllocator()
	{
		return DefaultAllocator;
	}

	Allocator *Allocator::getAllocator()
	{
		return UserSet;
	}

	Allocator *Allocator::the()
	{
		return getAllocator();
	}

	void Allocator::setAllocator( Allocator *_userSet, bool bOverwriteDefault )
	{
		if( DefaultAllocator == 0L or bOverwriteDefault == true)
			DefaultAllocator = _userSet;

		if( _userSet == 0L and bOverwriteDefault == true)
			DefaultAllocator = 0L;

		UserSet = _userSet;
	}

	int Allocator::attach()
	{
		return 1 + refCnt.exchange_and_add( 1 );
	}

	bool Allocator::detach()
	{
		if( refCnt.dec_and_test_null() )
		{
			if( this == the() )
				setAllocator( 0L, true );

			delete this;
			return true;
		}
		return false;
	}
}
