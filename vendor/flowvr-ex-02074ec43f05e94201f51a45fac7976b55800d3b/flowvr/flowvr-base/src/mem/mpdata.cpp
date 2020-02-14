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
* File: src/mpdata.cpp                                            *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/mem/sharedmemorybuffer.h"
#include "flowvr/mem/mpdata.h"
#include <iostream>
#include <cstring>

namespace flowvr
{

	namespace mem
	{
		void MPBuffer::init()
		{
		  shmID        = 0;
		  offset       = 0;
		  suboffset    = 0;
		  subsize      = 0;
		  segments     = 0;
		  num_segments = 0;
		}

		void MPBuffer::clear()
		{
			if (shmID != 0)
			{
				// so far, the MPBuffer is attached or invalid
				if (shmID != -1)
				{
					// valid, e.g. attached.
					SharedMemoryArea* area = SharedMemoryManager::instance()->openMemoryArea(shmID);
					if (area != NULL) // should not happen, but there might be evil code out there...
					{
						// check segments
						if ( num_segments == 1 )
						{
							area->freeRef( offset );
							offset = 0;
							subsize = 0;
						}
						else
						if ( num_segments > 1 ) // are there any?
						{
							// yes, so go segment-wise
							MPBuffer *p = area->getWrite<MPBuffer>(segments);
							for (size_t n = 0; n < num_segments; ++n, ++p)
							{
								SharedMemoryArea *shmA = SharedMemoryManager::instance()->openMemoryArea( (*p).shmID );
								if( !shmA )
									continue;
								shmA->freeRef( (*p).offset );
							}

							// we have to give back the auxiliary memory needed for the task
							area->freeRef(segments);

							// finally reset the values for segment control
							segments = num_segments = 0;
						}
					}
				}
				shmID = 0; // set shm-area id to 0, this MPBuffer is detached
			}
		}

		void MPBuffer::operator=(const Buffer& buf)
		{
		  // first, detach this MPBuffer, clear will test if this MPBuffer was ever attached
		  clear();

		  if( !buf.valid() )
		  {
			  shmID = -1;
			  return; // invalid buffer, nothing to do for us.
		  }

		  // valid buffer, so we have segments...
		  const std::vector<Buffer::_bufferDesc> &descs = buf.getSegments();
		  num_segments = descs.size();
		  SharedMemoryBuffer *imp = NULL;
		  size_t n = 0;
		  while(!imp)
			  imp = (SharedMemoryBuffer*)descs[n++].imp; // at least one is valid!

		  if( imp->getArea() )
		  {
			  shmID    = imp->getArea()->getAreaId();
			  if( num_segments > 1 )
				  segments = imp->getArea()->allocBuffer( num_segments * sizeof(MPBuffer) );
			  else
				  segments = 0;
		  }
		  else
			  shmID = -1;

		  // now care for all the segments.
		  if(segments) // ok, this worked
		  {
			  imp->getArea()->addRef(segments);
			  MPBuffer *p = imp->getArea()->getWrite<MPBuffer>(segments); // get first 'MPBuffer' and iterate

			  for( std::vector<Buffer::_bufferDesc>::const_iterator cit = descs.begin(); cit != descs.end(); ++cit, ++p )
			  {
				  (*p).init();
				  SharedMemoryBuffer *shmb = (SharedMemoryBuffer*)(*cit).imp;
				  (*p).offset    =  shmb ? shmb->getOffset() : 0;
				  (*p).suboffset = (*cit).offset;
				  (*p).subsize   = (*cit).size;
				  (*p).shmID     = (shmb->getArea() ? shmb->getArea()->getAreaId() : -1);

				  if( shmb->getArea() )
					  shmb->getArea()->addRef( (*p).offset );
			  }
		  }
		  else if(num_segments == 1)
		  {
			  offset = imp->getOffset();
			  suboffset = buf.getOffset();
			  subsize = buf.getSize();
			  if( shmID > 0 )
				  imp->getArea()->addRef(offset);
		  }
		  else
			  num_segments = 0; // did not work, clear segments field.
		}

		MPBuffer::operator Buffer() const
		{
		  // this is a special case, happening only when we
		  // encounter an empty (size-0) buffer
		  if (shmID==-1)
			return Buffer(&SharedMemoryBuffer::emptyBuffer,0,0);

		  // this will test whether the shmid is > 0, so all
		  // buffers with shmid == 0 are invalid and will not be passed
		  if (!valid())
			return Buffer(); // return invalid buffer

		  SharedMemoryArea* area = SharedMemoryManager::instance()->openMemoryArea(shmID);
		  if (area==NULL)
			return Buffer(); // no area... no fun...


		  Buffer b;
		  if( num_segments == 1 )
			  b = Buffer( new SharedMemoryBuffer( area, offset, suboffset+subsize ), suboffset, subsize, true );
		  else
		  {
			  // get pointer to first MPBuffer
			  const MPBuffer *p = area->getRead<MPBuffer>(segments);

			  for( size_t n = 0; n < num_segments; ++n, ++p ) // increase n and p
			  {
				  Buffer sb( new SharedMemoryBuffer( area, (*p).offset, (*p).suboffset+(*p).subsize ),
													 (*p).suboffset, (*p).subsize, true );
				  b += sb;
			  }
		  }
		  return b;
		}

		MPBuffer::operator BufferWrite() const
		{
		  // this is a special case, happening only when we
		  // encounter an empty (size-0) buffer
		  if (shmID==-1)
			return BufferWrite(&SharedMemoryBuffer::emptyBuffer,0,0);

		  // this will test whether the shmid is > 0, so all
		  // buffers with shmid == 0 are invalid and will not be passed
		  if (!valid())
			return BufferWrite(); // invalid buffer

		  SharedMemoryArea* area = SharedMemoryManager::instance()->openMemoryArea(shmID);
		  if (area==NULL)
			return BufferWrite(); // no area, no fun...

		  // having segments, first go as normal
		  BufferWrite b;
		  if( num_segments == 1 )
			  b = BufferWrite( new SharedMemoryBuffer( area, offset, suboffset+subsize ), suboffset, subsize, true );
		  else
		  {
			  // get pointer to first MPBuffer and interate
			  const MPBuffer *p = area->getRead<MPBuffer>(segments);
			  for( size_t n = 0; n < num_segments; ++n, ++p ) // increase n and p
			  {
				  BufferWrite sb( new SharedMemoryBuffer( area, (*p).offset, (*p).suboffset+(*p).subsize ),
													 (*p).suboffset, (*p).subsize, true );
				  b += sb;
			  }
		  }
		  return b;
		}


		// ##############################################################################
		// MPString
		// ##############################################################################
		void MPString::copy(const char* str, size_t length)
		{
		  if (str==NULL)
		  {
			clear();
		  }
		  else
		  {
			if ( length== ~0 )
				length = strlen(str);

			BufferWrite wr = SharedMemoryManager::instance()->allocBuffer(length);

			if(wr.valid())
			{
				memcpy(wr.writeAccess(),str,length);
				*this = wr;
			}
		  }
		}

		MPString::operator std::string() const
		{
		  if (empty())
			return std::string();

		  SharedMemoryArea* area = SharedMemoryManager::instance()->openMemoryArea(shmID);
		  if (area==NULL)
			return std::string();


		  if( num_segments == 1 )
		  {
			  const char* start = area->getRead<char>(offset+suboffset);
			  return std::string (start,start+subsize);
		  }
		  else
		  {
			  const MPBuffer *p = area->getRead<MPBuffer>(segments);
			  Buffer b;
			  for( size_t n = 0; n < num_segments; ++n )
				  b += ((Buffer)(*p));

			  b = b.linearize();
			  return (std::string)b;
		  }
		}

	} // namespace mem
} // namespace flowvr
