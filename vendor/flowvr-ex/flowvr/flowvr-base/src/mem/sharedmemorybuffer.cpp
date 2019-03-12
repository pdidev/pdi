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
* File: src/sharedmem/sharedmemorybuffer.cpp                      *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/mem/sharedmemorybuffer.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/mem/sharedmemorymanager.h"

#include <sys/shm.h>
#include <sys/types.h>
#include <iostream>
#include <string.h>

#include "flowvr/utils/streambuf.h"
namespace flowvr
{

namespace mem
{

SharedMemoryBuffer* SharedMemoryBuffer::create(SharedMemoryArea* area, size_t offset, size_t size)
{
  return new SharedMemoryBuffer(area,offset,size);
}

SharedMemoryBuffer::SharedMemoryBuffer(SharedMemoryArea* area, size_t offset, size_t size)
  : area(area), offset(offset), size(size), nbRefLocal(0)
{
  if (area==NULL)
  {
    if (size!=0)
    {
      std::cerr<<"SharedMemoryBuffer: Invalid reference to NULL area"<<std::endl;
      size = 0;
    }
  }
  else if (!area->isValidRef(offset))
  {
    std::cerr<<"SharedMemoryBuffer: Invalid reference to offset 0x"<<std::hex<<offset<<std::dec<<std::endl;
    area = NULL;
    size = 0;
  }
  else
    area->addRef(offset);
}

SharedMemoryBuffer::~SharedMemoryBuffer()
{
  if (area!=NULL)
    area->freeRef(offset);
}

size_t SharedMemoryBuffer::open()
{
  ++nbRefLocal;
  return size;
}

int SharedMemoryBuffer::getCount() const
{
	return (int)nbRefLocal;
}

void SharedMemoryBuffer::claim()
{
  if (nbRefLocal<=0)
  {
    std::cerr << "ERROR: claiming an already closed buffer 0x"
    		  << std::hex << offset << std::dec << " of size " << size << std::endl;
    for ( int i = 0  ;  i < this->size  ;  i++ ) {
        std::cerr << this->readAccess()[ i ];
    }
    std::cerr << std::endl;
    return;
  }
  ++nbRefLocal;
}

void SharedMemoryBuffer::release()
{
	close();
}

void SharedMemoryBuffer::close()
{
  if (nbRefLocal<=0)
  {
    std::cerr << "ERROR: close already closed buffer 0x"
              << std::hex << offset << std::dec << " of size " << size << std::endl;
    for ( int i = 0  ;  i < this->size  ;  i++ ) {
        std::cerr << this->readAccess()[ i ];
    }
    std::cerr << std::endl;
    return;
  }
  
  if (nbRefLocal.dec_and_test_null())
    delete this;
}

bool SharedMemoryBuffer::uniqueOwner() const
{
  return (getCount()==1) && (area==NULL || area->uniqueRef(offset));
}

const ubyte* SharedMemoryBuffer::readAccess() const
{
  if (area==NULL)
    return NULL;

  return area->getMappingRead()+(offset);
}

ubyte* SharedMemoryBuffer::writeAccess() const
{
  if (area==NULL)
    return NULL;

  return area->getMappingWrite()+(offset);
}

bool SharedMemoryBuffer::resize(size_t minSize, size_t preferredSize, bool expandOnly)
{
	if (area == NULL)
	  return false;

	if (size>=minSize)
	  return true;

  size = area->getBufferSize(offset);
  return ( size >= minSize );
}

size_t SharedMemoryBuffer::getSize() const
{
  return size;
}

Allocator* SharedMemoryBuffer::getAllocator() const
{
  return SharedMemoryManager::instance();
}

SharedMemoryArea *SharedMemoryBuffer::getArea() const
{
	return area;
}

size_t SharedMemoryBuffer::getOffset() const
{
	return offset;
}


SharedMemoryBuffer SharedMemoryBuffer::emptyBuffer;

SharedMemoryBuffer::SharedMemoryBuffer()
  : area(NULL), offset(0), size(0), nbRefLocal(1) // never remove this buffer
{
}

} // namespace mem

} // namespace flowvr
