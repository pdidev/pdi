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
* File: src/buffer.cpp                                            *
*                                                                 *
* Contacts:                                                       *
*  03/18/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/

#include <cstring>

#include "flowvr/allocator.h"
#include "flowvr/buffer.h"
#include "flowvr/mem/memorymanager.h"

#include <algorithm>
#include <iostream>
#include <iterator>
#include <set>

namespace
{
	using namespace flowvr;

	class _copyIn : public std::unary_function<const Buffer::_bufferDesc &, void>
	{
	public:
		_copyIn( const BufferWrite &target, size_t offset )
		: m_target(target)
		, m_offset(offset)
		{}

		void operator()( const Buffer::_bufferDesc &other )
		{
			memcpy( m_target.getWrite<void>(m_offset), other.pointer, other.size );
			m_offset   += other.size;
		}

		size_t m_offset;
		BufferWrite m_target;
	};

	class _copyTo : public std::unary_function<const Buffer::_bufferDesc &, void>
	{
	public:
		_copyTo( void *target, size_t offset )
		: m_target(target)
		, m_offset(offset)
		{}

		void operator()( const Buffer::_bufferDesc &other )
		{
			memcpy( ((ubyte*)m_target)+m_offset, other.pointer, other.size );
			m_offset   += other.size;
		}

		size_t m_offset;
		void  *m_target;
	};
}

namespace flowvr
{
	const size_t Buffer::ALLSEGMENTS        = ~0;
	const size_t Buffer::INVALIDSEGMENTSIZE = ~0;
	const size_t Buffer::CLAIMALLSIZE       = ~0;

	Buffer::_bufferDesc::_bufferDesc()
	: imp(NULL)
	, offset(0)
	, size(0)
	, pointer(NULL)
	{}

	Buffer::_bufferDesc::_bufferDesc( BufferImp *_imp, size_t _offset, size_t _size, ubyte *_pointer )
	: imp(_imp)
	, offset(_offset)
	, size(_size)
	, pointer(_pointer)
	{
		// this is a special contructor, we assume that imp is already referenced.
		// note: typically this constructor is used for low-level operation.
	}

	Buffer::_bufferDesc::_bufferDesc( const _bufferDesc &other )
	: imp( other.imp )
	, offset( other.offset )
	, size( other.size )
	, pointer( other.pointer )
	{
		reference();
	}

	Buffer::_bufferDesc &Buffer::_bufferDesc::operator=( const _bufferDesc &other)
	{
        if( &other != this ) {
		  dereference();

		  imp = other.imp;

		  reference();

		  offset   = other.offset;
		  size     = other.size;
		  pointer  = other.pointer;
        }        
        return *this;
	}

	Buffer::_bufferDesc::~_bufferDesc()
	{
		dereference();
	}

	void Buffer::_bufferDesc::reference()
	{
		  if(imp)
			  imp->claim();
	}

	void Buffer::_bufferDesc::dereference()
	{
		  if( imp )
			  imp->release();
	}

	bool Buffer::_bufferDesc::operator==( const _bufferDesc &other ) const
	{
		 return (( imp == other.imp )
		   and ( offset == other.offset )
		   and ( size == other.size ) ); // pointer is implicitly (imp->read + offset) equal
	}

	bool Buffer::_bufferDesc::operator<( const _bufferDesc &other ) const
	{
		  return pointer < other.pointer;
	}

	bool Buffer::_bufferDesc::equals( const _bufferDesc &other ) const
	{
		   return (*this == other);
	}

	bool Buffer::_bufferDesc::valid() const
	{
		  return imp!=NULL;
	}

	bool Buffer::_bufferDesc::unique() const
	{
		  return imp->uniqueOwner();
	}

	Buffer Buffer::_bufferDesc::toBuffer() const
	{
		  return Buffer(imp, offset, size);
	}

	// ##########################################################################3

	Buffer::~Buffer()
	{
		// the segments vector will be free as an auto-variable,
		// and the destructor of _bufferDesc will decrease the refcount
		// for all segments contained. That's why nothing happens here...
	}

	Buffer::Buffer()
	: segmentsSize(INVALIDSEGMENTSIZE)
	{

	}

	Buffer::Buffer(const Buffer& from)
	: segments( from.segments ) // copies, _bufferDesc takes care of claiming.
	, segmentsSize( from.segmentsSize )
	{
	}



	Buffer::Buffer(BufferImp* fromimp, size_t suboffset, size_t subsize, bool open)
	: segmentsSize(INVALIDSEGMENTSIZE)
	{
			// internal knowledge: open() the segment or claim() it
			// (first creation needs to call open(), while any operation thereafter can call claim())
		  if( open )
			  fromimp->open();
		  else
			  fromimp->claim();

		  // how many bytes are covered by fromimp? (there are no segments on the level of imps)
		  size_t impsize = fromimp->getSize();

		  size_t size; // we will record our own size here

		  if( subsize == CLAIMALLSIZE ) // take it all?
			  size = impsize; // no subsize specified, try to claim it all...
		  else
			  size = subsize; // subsize specified... set this as my size

		  // test whether caller gave invalid arguments
		  // (guard more bytes than imp really has) -> clamp against largest value)
		  if( suboffset+size>impsize )
			  size = impsize - suboffset;

		  // adjust the pointer (simple cache)
		  const ubyte *pointer = fromimp->readAccess()+suboffset;

		  // add this (first) descriptor to our list of segments.
		  segments.push_back( _bufferDesc( fromimp, suboffset, size, const_cast<ubyte*>(pointer) ) );
	}

	Buffer::Buffer(const Buffer& from, size_t suboffset, size_t subsize)
	: segmentsSize(INVALIDSEGMENTSIZE)
	{
		// Lazy copy: just copy all segments that we find in [suboffset, suboffset+subsize]
		  std::vector<_bufferDesc>::size_type firstOffset, remainingsize, lastSize;
		  std::vector<_bufferDesc>::const_iterator first = from.findFirstForOffset( suboffset, firstOffset, remainingsize );
		  std::vector<_bufferDesc>::const_iterator last  = from.findLastForSize( first, subsize, firstOffset, lastSize );

		  // could be that we did not find any segment, because subsize was set to 0
		  // or from is the invalid buffer
		  if( first == from.segments.end() )
			  return; // we are done


		  // could be that the whole interval is just in one segment
		  if( first == last )
		  {
			  // push the descriptor as first element of our segments
			  segments.push_back(*first);

			  // low-level operation: adjust the read-pointer to match "our" offset and not
			  // just mirror the offset in the other.first

			  // first.pointer-first.offset sets pointer back to start and from there we go to firstOffset
			  segments.front().pointer = (*first).pointer-(*first).offset+firstOffset;
			  // record that fact
			  segments.front().offset = firstOffset;

			  // and properly note the end of the window
			  segments.front().size = lastSize;
		  }
		  else
		  {
			  // ok, we use assign (last is guaranteed to be one to far, as assign uses [first, last) for insertion
			  segments.assign( first, last );
			  // see above
			  segments.front().pointer = (*first).pointer-(*first).offset+firstOffset;
			  segments.front().offset = firstOffset; // adjust first position in buffer
			  segments.front().size = remainingsize; // adjust tail of front buffer

			  // we might snip some part of the last segment
			  segments.back().size = lastSize; // we take the prefix (starting at its regular offset, but maybe not all of it)
		  }
	}

	// ############ OPERATORS #############################################################

	Buffer& Buffer::operator=(const Buffer& from)
	{
	  if( &from == this )
		  return *this;

	  segments     = from.segments;
	  segmentsSize = from.segmentsSize;

	  return *this;
	}

	bool Buffer::operator==(const Buffer& b) const
	{
	  // quick test: non-matching number of buffers indicates different buffers
	  if( segments.size() != b.segments.size() )
		  return false;

	  // slow test: compare element-wise
	  for( std::vector<_bufferDesc>::size_type n = 0; n < segments.size(); ++n )
	  {
		  if( !segments[n].equals( b.segments[n] ) )
			  return false;
	  }
	  return true;
	}

	Buffer::operator std::string() const
	{
		if( segments.empty() )
			return std::string();

		if( segments.size() == 1 )
		{
			if( segments[0].size == 0 )
				return std::string();
			return std::string( segments[0].pointer, segments[0].pointer+segments[0].size);
		}
		else
		{
			// this implementation is slow: it uses 2 (read: 2) copies to get the final string
			// but it is more safe to do it that way than to assume we can fiddle around with the
			// internals of std::string (play tricks on reserve / character width)

			// avoid re-allocation for every segment, instead:
			// create a vector for the total size
			std::vector<ubyte> v(getSize(ALLSEGMENTS));
			std::vector<ubyte>::size_type offset = 0; // local index in v

			// iterate over bufferDescs
			for( std::vector<_bufferDesc>::size_type n=0; n < segments.size(); ++n )
			{
				// copy from desc to v (starting at offset)
				std::copy( segments[n].pointer, segments[n].pointer+segments[n].size, &v[offset] );

				// adjust offset
				offset += segments[n].size;
			}

			// now one copy back to a string, and return
			return std::string( v.begin(), v.end() );
		}
	}



	Buffer  Buffer::operator+( const Buffer &other ) const
	{
		Buffer b(*this);
		return (b+=other);
	}

	Buffer &Buffer::operator-=( const Buffer &other )
	{
		if( this == &other )
		{
			// invalidate me
			(*this) = Buffer();
			return *this;
		}

		if( segments.empty() or other.segments.empty() )
			return *this;

		// erase all segments in me which are in other as well
		std::set<_bufferDesc> set_other( other.segments.begin(), other.segments.end() );
		std::set<_bufferDesc> set_me( segments.begin(), segments.end() );

		std::set<_bufferDesc> intersection;
		std::set_intersection(set_me.begin(), set_me.end(),
				              set_other.begin(), set_other.end(),
				              std::inserter( intersection, intersection.begin()) );

		if( intersection.empty() )
			return *this;

		/// @todo check with duplicates in this set (are they counted as 1?)
		size_t n = intersection.size();

		while( !intersection.empty() )
		{
			std::remove( segments.begin(), segments.end(), *intersection.begin() ); // swapped to the end of vector
			intersection.erase( intersection.begin() ); // pop off head
		}

		// pop off tail of vector
		segments.erase( segments.end()-n, segments.end() );

		return *this;
	}


	Buffer &Buffer::operator+=( const Buffer &other )
	{
		if( !other.valid() )
			return *this; // do not add non-valid buffers

		std::copy( other.segments.begin(), other.segments.end(), std::back_inserter( segments ) );
		if( other.segmentsSize == INVALIDSEGMENTSIZE )
			segmentsSize = INVALIDSEGMENTSIZE; // did not calculate the other's size, yet, so reset ours (be lazy)
		else if( segmentsSize != INVALIDSEGMENTSIZE ) // already calculated our size and the other's? just add...
			segmentsSize += other.segmentsSize;
		return *this;
	}


	Buffer  Buffer::operator-( const Buffer &other ) const
	{
		Buffer b(*this); // copy us
		return (b-=other); // substract other from the copy and return result
	}

	const Buffer Buffer::operator[](int segment) const
	{
		return segments[segment].toBuffer();
	}

	Buffer	Buffer::operator[](int segment)
	{
		return segments[segment].toBuffer();
	}


	// ############ /OPERATORS ##################################################

	size_t Buffer::getSize( size_t segmentIndex ) const
	{
		// no segments at all?
	  if( segments.empty() )
		  return 0;

	  if( segmentIndex == ALLSEGMENTS )
	  {
		  // already calculated?
		  if(segmentsSize == INVALIDSEGMENTSIZE)
		  {
			  // no
			  segmentsSize = 0; // reset to 0 and add up all subsizes
			  for( std::vector<_bufferDesc>::size_type n = 0; n < segments.size(); ++n )
				  segmentsSize += segments[n].size;
		  }
		  return segmentsSize; // return sum
	  }
	  else
		  return segments[segmentIndex].size; // query by index
	}


	bool Buffer::empty() const
	{
	  if( !valid() )
		  return true;

	  if( getSize(ALLSEGMENTS) == 0 )
		  return true;

	  return false;
	}


	bool Buffer::valid() const
	{
	  if( segments.empty() )
		  return false; // no segments -> invalid

	  for( std::vector<_bufferDesc>::size_type n = 0; n < segments.size(); ++n )
	  {
		  if( segments[n].imp )
			  return true; // at least one valid buffer!
	  }

	  return false; // non-empty, but only invalid buffers
	}


	const ubyte* Buffer::readAccess( size_t segment ) const
	{
	  if( segments.empty() )
		  return NULL;

	  return segments[segment].pointer;
	}



	bool Buffer::unique(size_t segmentIndex) const
	{
	  if( !valid() )
		  return false; // invalid buffer does not have an owner at all

	  if( segmentIndex == ALLSEGMENTS )
	  {
		  for( std::vector<_bufferDesc>::size_type n=0; n < segments.size(); ++n )
		  {
			  if( !segments[n].unique() )
				  return false;
		  }
	  }
	  else
		  return segments[segmentIndex].unique();

	  return true;
	}

	Allocator* Buffer::getAllocator( size_t segment ) const
	{
	  if( segments.empty() )
		  return Allocator::getDefaultAllocator();

	  Allocator* alloc = NULL;

	  if( segments[segment].valid() )
		  alloc = segments[segment].imp->getAllocator();

	  if( alloc == NULL )
		  return Allocator::getDefaultAllocator();

	  return alloc;
	}


	size_t Buffer::getNumberOfSegments() const
	{
		return segments.size();
	}

	bool Buffer::getIsSegmented() const
	{
		return segments.size() > 1;
	}


	const std::vector<Buffer::_bufferDesc> &Buffer::getSegments() const
	{
		return segments;
	}

	std::vector<Buffer::_bufferDesc> &Buffer::getSegments()
	{
		return segments;
	}

	bool Buffer::equals( const Buffer &other ) const
	{
		if( this == &other )
			return true;

		if( other.segments.size() != (*this).segments.size() )
			return false;

		// check for segments
		for( std::vector<Buffer::_bufferDesc>::size_type n=0; n < segments.size(); ++n )
			if( !segments[n].equals( other.segments[n] ) )
				return false;

		return true;
	}

	size_t Buffer::getSegmentSize(size_t segmentIndex) const
	{
		if( segments.empty() )
			return 0;
		return segments[segmentIndex].size;
	}

	Buffer Buffer::linearize() const
	{
		if( segments.empty() )
			return *this;

		if( !getIsSegmented() )
			return Buffer(*this); // no need to re-allocate memory... pass this buffer.

		size_t sz = getSize(ALLSEGMENTS);
		BufferWrite b = getAllocator()->alloc(sz);

		std::for_each( segments.begin(), segments.end(), _copyIn( b, 0 ) );

		return b;
	}

	void Buffer::copyTo( void *dest )
	{
		if( segments.empty() )
			return;

		std::for_each( segments.begin(), segments.end(), _copyTo( dest, 0 ) );
	}

	std::vector<Buffer::_bufferDesc>::const_iterator Buffer::findFirstForOffset( size_t offset,
			                                                       std::vector<_bufferDesc>::size_type &firstOffset,
			                                                       std::vector<_bufferDesc>::size_type &remainingsize ) const
	  {
		  // advance as many segments as we need to reach 'offset'
		  std::vector<_bufferDesc>::size_type n = 0; // we start at 0 (n records the bytes already eaten)
		  for( std::vector<_bufferDesc>::const_iterator cit = segments.begin(); cit != segments.end(); ++cit )
		  {
			  if( !(*cit).valid() ) // skip invalid buffers (should not happen)
				  continue;

			  if( offset < (n + (*cit).size) )
			  {
				  // segment found that contains 'offset'
				  firstOffset = (*cit).offset + offset; // absolute(!) position in segment starts at (0->(*cit).offset (and we take a slice from that))
				  remainingsize = (*cit).imp->getSize() - firstOffset; // get remainder of this segment
				  return cit; // return this segment
			  }
			  else
				  n += (*cit).size; // advance thw whole segment
		  }

		  return segments.end(); // did not find it (invalid buffer / offset too large for subwindow / only invalid segments)
	  }

	  std::vector<Buffer::_bufferDesc>::const_iterator Buffer::findLastForSize( std::vector<_bufferDesc>::const_iterator startPos,
			                                              size_t subsize,
			                                              size_t firstOffset,
			                                              std::vector<_bufferDesc>::size_type &lastSize) const
	  {
		  // sanity check: anything to do here?
		  if( startPos == segments.end() )
			  return segments.end();

		  if( subsize == CLAIMALLSIZE ) // should we take it all?
			  return segments.end(); // take all the rest now

		  std::vector<_bufferDesc>::size_type n = firstOffset; // start to consume up to subsize bytes, starting at the given offset
		  size_t remainingSecSize=0;
		  for( std::vector<_bufferDesc>::const_iterator cit = startPos; cit != segments.end(); ++cit )
		  {
			  if( !(*cit).imp )
				  continue; // skip (but keep) invalid descriptors

			  remainingSecSize = (*cit).size-n; // this is the number of bytes already eaten (n is only on the first iteration eventually != 0)
			  if( subsize <=  remainingSecSize)
			  {
				  // this tail is already in the current block,
				  // stop iteration
				  lastSize = subsize;
				  return ++cit; // we want to include last, that's why we use ++cit here
				                // (we found out desired position in cit, but assign() copies [first,last))
			  }
			  else
				  subsize -= remainingSecSize; // note how many bytes are left to read
			  n = 0; // reset n, we just need it for the first iteration (after that it will remain 0)
		  }

		  lastSize = remainingSecSize; // give back the remainder to the caller
		  return segments.end(); // copy up to the last one
	  }


	  // ############### BUFFERWRITE ######################################################
	bool BufferWrite::reserve(size_t requestedMinSize, bool expandOnly, bool amortized, int segment)
	{
		  // If we do not have an available allocator then we report failure.
		if (getAllocator(segment) == NULL)
			return false;

		if( segments.empty() )
			return false;

		_bufferDesc &desc = segments[segment];

		BufferImp *imp = desc.imp;

		  // Check if it is not some bogus call.
		  if (desc.size >= requestedMinSize)
			  return true; // buffer already bigger than requested

		  size_t preferredSize = requestedMinSize; // we want to have exactly this number of bytes

		  // does the caller prefer a growth by order 2?
		  if (amortized)
			  preferredSize = std::max(requestedMinSize, 2*desc.size);

		  if (imp && imp->resize(desc.offset+requestedMinSize, desc.offset+preferredSize, expandOnly))
		  {
			// We managed to reserve.
			desc.pointer  = const_cast<ubyte*>(imp->readAccess())  + desc.offset;
			desc.size     = preferredSize;
			return true;
		  }

		  // The implementation above did not manage to resize.
		  // Last solution is to allocate a new buffer with size preferredSize;

		  // If we can only expand and that data is already present we must report failure.
		  if (expandOnly && (desc.size != 0))
			  return false;


		  // temporarily create newBuffer
		  BufferWrite newBuffer = getAllocator(segment)->alloc(desc.offset+preferredSize);
		  if (!newBuffer.valid())
			  return false; // No more free space apparently.

		  // copy over content to newBuffer (as segment 0)
		  memcpy(newBuffer.writeAccess(), imp->readAccess(), desc.offset+desc.size);

		  // release my old segment and assign the newBuffer-segment 0
		  segments[segment] = newBuffer.segments[0];

		  segmentsSize = INVALIDSEGMENTSIZE;
		  return true;
	}

	bool BufferWrite::resize(size_t newSize, bool amortized, int segment)
	{
		if (!reserve(newSize, false, amortized, segment))
			return false;

		segmentsSize = INVALIDSEGMENTSIZE; // invalidate size

		return true;
	}

	bool BufferWrite::expand(int newSize, bool amortized, int segment)
	{
		if ( !reserve(newSize, true, amortized, segment) )
			return false;

		segmentsSize = INVALIDSEGMENTSIZE;

		return true;
	}

	BufferWrite BufferWrite::linearize() const
	{
		BufferWrite b;
		b.Buffer::operator=( Buffer::linearize() );
		return b;
	}

} // namespace flowvr
