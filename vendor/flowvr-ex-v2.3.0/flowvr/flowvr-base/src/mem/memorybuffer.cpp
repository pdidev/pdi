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
* File: src/mem/memorybuffer.cpp                                  *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/mem/memorybuffer.h"
#include "flowvr/mem/memorymanager.h"

#include <stdlib.h>

namespace flowvr
{

	namespace mem
	{

		MemoryBuffer::MemoryBuffer(size_t _size, int nref) :
			size(_size), data(NULL), nbRef(nref)
		{
			if (size > 0)
				data = malloc(size);
		}

		MemoryBuffer::~MemoryBuffer()
		{
			if (data != NULL)
				free(data);
		}

		size_t MemoryBuffer::open()
		{
			++nbRef;
			return size;
		}

		void MemoryBuffer::claim()
		{
			++nbRef;
		}

		void MemoryBuffer::release()
		{
			close();
		}

		void MemoryBuffer::close()
		{
			if (nbRef.dec_and_test_null())
				delete this;
		}

		int MemoryBuffer::getCount() const
		{
			return (int) nbRef;
		}

		const ubyte* MemoryBuffer::readAccess() const
		{
			return (const ubyte*) data;
		}

		ubyte* MemoryBuffer::writeAccess() const
		{
			return (ubyte*) data;
		}

		bool MemoryBuffer::resize(size_t minSize, size_t preferredSize, bool expandOnly)
		{
			if (!uniqueOwner()) // resizing works only on uniqueOwner buffers
				return false; // indicate failure when buffer is shared

			if (size >= minSize)
				return true; // no need for a change here, we consider this success,
			// regardless of the preferred size

			if (expandOnly and ((minSize < size) or (preferredSize < size)))
				return false; // do not shrink


			// step 1: try the system re-alloc on the preferred size
			void *newData = realloc(data, preferredSize);
			if (!newData)
			{
				// did not work, so we try again with the minSize given
				newData = realloc(data, minSize);
				if (!newData)
					return false; // did not work as well... indicate failure
				size = minSize; // worked, so assign size to the new minSize
			}
			else
				size = preferredSize; // worked, so assign the size to the preferred size

			data = newData; // assign new pointer
			return true; // indicate success
		}

		size_t MemoryBuffer::getSize() const
		{
			return size;
		}

		Allocator* MemoryBuffer::getAllocator() const
		{
			return MemoryManager::instance();
		}


		/**
		 * static member: emptyBuffer()
		 */
		 MemoryBuffer MemoryBuffer::emptyBuffer(0, 1);
	} // namespace mem

} // namespace flowvr
