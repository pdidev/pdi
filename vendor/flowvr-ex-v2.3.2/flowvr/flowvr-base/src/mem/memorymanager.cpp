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
* File: src/mem/memorymanager.cpp                                 *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/mem/memorymanager.h"
#include "flowvr/mem/memorybuffer.h"

#include <stdlib.h>

namespace flowvr
{

namespace mem
{

/// Allocate a memory buffer.
BufferWrite MemoryManager::alloc(size_t size)
{
  if (size == 0)
    return BufferWrite(&MemoryBuffer::emptyBuffer, 0, 0, true);
  else
    return BufferWrite(new MemoryBuffer(size), 0, size, true);
}

MemoryManager* MemoryManager::instance()
{
	return dynamic_cast<MemoryManager*>( Allocator::getAllocator() );
}

} // namespace mem

} // namespace flowvr
