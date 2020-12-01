/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                         Base Libraries                          *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * INRIA and                                                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (FRE 2490). ALL RIGHTS RESERVED.                                *
 *                                                                 *
 * This source is covered by the GNU LGPL, please refer to the     *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Jeremie Allard,                                              *
 *    Ronan Gaugne,                                                *
 *    Valerie Gouranton,                                           *
 *    Loick Lecointre,                                             *
 *    Sebastien Limet,                                             *
 *    Bruno Raffin,                                                *
 *    Sophie Robert,                                               *
 *    Emmanuel Melin.                                              *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: src/bufferpool.cpp                                        *
 *                                                                 *
 * Contacts:                                                       *
 *  01/21/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/

#include "flowvr/bufferpool.h"
#include "flowvr/moduleapi.h"

namespace flowvr
{

/// Constructor
BufferPool::BufferPool(int maxbuffers)
: nbrequests(0)
, nballocs(0)
, globalLock("bufferpool")
, currentBufferSize(0)
, maxBuffers(maxbuffers)
{
}

/// Allocate a new buffer or reuse a free one.
BufferWrite BufferPool::alloc(Allocator* allocator, size_t size, bool strict)
{
	if (size == 0)
		return allocator->alloc(0);

	ipc::ScopedMTLock locker(globalLock, "alloc");

	++nbrequests;

	if (size != currentBufferSize)
	{ // remove all current references
		buffers.clear();
		freeBuffers.clear();
		currentBufferSize = size;
	}

	if (freeBuffers.empty() && !buffers.empty())
	{ // search for any newly free buffers
		BufferList::iterator it = buffers.begin();
		while (it != buffers.end())
		{
			if (it->unique())
			{ // this buffer is free
				BufferList::iterator old = it;
				freeBuffers.push_back(*it);
				++it;
				buffers.erase(old);
			}
			else
				++it;
		}
	}
	BufferWrite buf;
	if(freeBuffers.empty())
	{
		if( buffers.size() >= maxBuffers)
		{
#ifdef DEBUG
			std::cerr << "Warning: buffer pool overflow ("<<buffers.size()<<")."<<std::endl;
#endif
			if (strict)
				return BufferWrite();
			else
				buffers.pop_front();
		}
		// allocate a new buffer
		++nballocs;
		buf = allocator->alloc(currentBufferSize);
		if (buf.getSize() != size)
			std::cerr << "BufferPool: alloc failure for requested size "
					<< size << std::endl;
	}
	else
	{ // use the first free buffer
		buf = freeBuffers.front();
		freeBuffers.pop_front(); // this is not a free buffer anymore
		if (buf.getSize() != size)
			std::cerr << "BufferPool: reuse failure for requested size "
					<< size << std::endl;
	}
	if (buf.getSize() == size)
		buffers.push_back(buf);
	return buf;
}

// ############################################################################
// BufferPoolAllocator
// ############################################################################

	// Constructor
	BufferPoolAllocator::BufferPoolAllocator(flowvr::Allocator *a, int maxbuffers) :
		bufferPool(maxbuffers), allocator(a)
	{
	}


	flowvr::BufferWrite BufferPoolAllocator::alloc(size_t size)
	{
		return bufferPool.alloc(allocator, size);
	}

	bool BufferPoolAllocator::realloc(BufferWrite &b, size_t size, bool amortized)
	{
	  return b.resize(size, amortized);
	}
} // namespace flowvr
