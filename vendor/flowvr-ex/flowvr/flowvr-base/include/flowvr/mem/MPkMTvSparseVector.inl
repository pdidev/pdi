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
* File: include/flowvr/flowvr-base/mem/MPkMTvSparseVector.inl     *
*                                                                 *
* Contacts:                                                       *
*  29/11/2012 Jeremy Jaussaud <Jeremy.Jaussaud@inria.fr>          *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_SHAREDMEM_MPKMTCSPARSEVECTOR_INL
#define FLOWVR_SHAREDMEM_MPKMTCSPARSEVECTOR_INL

#include "flowvr/mem/MPkMTvSparseVector.h"

#include "flowvr/buffer.h"
#include "flowvr/ipc/atomic.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/ipc/mplock.h"

#include <cstddef> // ptrdiff_t
#include <climits> // INT_MAX


namespace flowvr
{

namespace mem
{


////////////////////////////////////////////////////////////////////////////
//    MPkMTvSparseVector< Tkey, Tval >
////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
//    Public methods
////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////
//    construction / destruction

template< typename Tkey, typename Tval >
MPkMTvSparseVector< Tkey, Tval >::
MPkMTvSparseVector()
    : _mpBuf( new BufferWrite )
    , _mpPointer( NULL )
    , _mappingLock( NULL )
    , _val( NULL )
    , _mappingIndex( NULL )
{
}

template< typename Tkey, typename Tval >
MPkMTvSparseVector< Tkey, Tval >::
MPkMTvSparseVector( const MPkMTvSparseVector& m )
    : _mpBuf( new BufferWrite )
    , _mpPointer( NULL )
    , _mappingLock( NULL )
    , _val( NULL )
    , _mappingIndex( NULL )
{
    if ( m.valid() )
    {
        init( *m._mpBuf );
        for ( typename MPkMTvSparseVector::const_iterator it = m.begin() ; it != m.end() ; ++it )
        {
            if ( it.isMapping() )
                map( it.key(), it.value() );
        }
    }
}

template< typename Tkey, typename Tval >
MPkMTvSparseVector< Tkey, Tval >::
MPkMTvSparseVector( const BufferWrite& mpBuf )
    : _mpBuf( new BufferWrite )
    , _mpPointer( NULL )
    , _mappingLock( NULL )
    , _val( NULL )
    , _mappingIndex( NULL )
{
    init( mpBuf );
}

template< typename Tkey, typename Tval >
MPkMTvSparseVector< Tkey, Tval >::
~MPkMTvSparseVector()
{
    destroy();
    delete _mpBuf;
}

template< typename Tkey, typename Tval >
void
MPkMTvSparseVector< Tkey, Tval >::
init( const BufferWrite& mpBuf )
{
    if ( mpBuf.valid() )
    {
        *_mpBuf = mpBuf;
        _mpPointer = mpBuf.getWrite<MPData>();
        
        // init simple members
        _KeyBegin = _mpPointer->_KeyBegin; // local copy of const value
        _KeyEnd = _mpPointer->_KeyEnd;     // local copy of const value
        _maxSize = _mpPointer->_maxSize;   // local copy of const value
        _key = _mpPointer->_key;           // get pointer to shared-memory array
        
        // alloc and init local dynamic arrays
        _val = new Tval[ _maxSize ];     // local values
        _mappingLock = new ipc::MTLock[ _maxSize ]; // mapping locks
        _mappingIndex = new ipc::MTAtomicInt[ _KeyEnd - _KeyBegin ]; // flags
        for ( Tkey i = 0 ; i < _KeyEnd - _KeyBegin ; ++i )
        {
            _mappingIndex[ i ].set( end().index() );
        }
    }
}

template< typename Tkey, typename Tval >
void
MPkMTvSparseVector< Tkey, Tval >::
destroy()
{
    if ( this->valid() )
    {
        delete[] _mappingLock;
        delete[] _val;
        delete[] _mappingIndex;
        _mpBuf->clear();// = BufferWrite();
    }
}


template< typename Tkey, typename Tval >
MPkMTvSparseVector< Tkey, Tval >&
MPkMTvSparseVector< Tkey, Tval >::
operator = ( const MPkMTvSparseVector & m )
{
    if ( this != & m )
    {
        destroy();
        if ( m.valid() )
        {
            // re-initialize the instance
            init( *m._mpBuf );
            // copy mapped values
            for ( typename MPkMTvSparseVector::const_iterator it = m.begin() ; it != m.end() ; ++it )
            {
                if ( it.isMapping() )
                    map( it.key(), it.value() );
            }
        }
    }
}

//////////////////////////////////////
//    mutators

template< typename Tkey, typename Tval >
void
MPkMTvSparseVector< Tkey, Tval >::
insert( Tkey key, const_reference val )
{   
    // You must be sure to be the first one to acquire the mapping lock of the
    //  new key, so acquire this lock right now.
    ipc::ScopedMTLock locker( _mappingLock[ _mpPointer->_size ] );
    
    // store the key
    _key[ _mpPointer->_size ] = key;
    
    // Note: The following memory barrier is important since it ensures other
    //  processes won't see the incrementing of the size of the array before the
    //  assignment of the new key.
    ipc::memory_barrier();
    
    // increment the size of the container to make the new key visible
    _mpPointer->_size.inc();
    
    // The key is inserted. Now map it.
    map( key, val );
}

template< typename Tkey, typename Tval >
void
MPkMTvSparseVector< Tkey, Tval >::
map( Tkey key, const_reference value )
{
    // find the index of the (already inserted) key
    iterator it = find( key );
    
    if ( end() == it )
    {
        std::cerr << "error while mapping key " << key
                  << ". The key hasn't been inserted in the container before trying to map it." << std::endl;
        return;
    }
        
    // Set the mapped value.
    *it = value;
    
    // memory barrier ensuring the order "assign, then flag" is respected.
    ipc::memory_barrier();
    
    // flag the mapping as done
    if ( ! mappingIndex( key ).compare_and_swap( end().index(), it.index() ) )
        std::cerr << "error while mapping key " << key
                  << " at index " << it.index()
                  << " with value " << value
                  << ". The key was already mapped." << std::endl;
}

template< typename Tkey, typename Tval >
void
MPkMTvSparseVector< Tkey, Tval >::
unmap( Tkey key )
{
    if ( end() != getMapped( key ) )
    {
        mappingIndex( key ).set( end().index() );
    }
}

/////////////////////////////////////
//    iterator-related methods

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::
begin()
{
  return iterator::begin( this );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::
end()
{
  return iterator::end( this );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::
begin() const
{
  return const_iterator::begin( this );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::
end() const
{
  return const_iterator::end( this );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::reverse_iterator
MPkMTvSparseVector< Tkey, Tval >::
rbegin()
{
  return reverse_iterator( end() );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::reverse_iterator
MPkMTvSparseVector< Tkey, Tval >::
rend()
{
    return reverse_iterator( begin() );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_reverse_iterator
MPkMTvSparseVector< Tkey, Tval >::
rbegin() const
{
    return const_reverse_iterator( end() );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_reverse_iterator
MPkMTvSparseVector< Tkey, Tval >::
rend() const
{
    return const_reverse_iterator( begin() );
}

////////////////////////////////////////////////////////////////////////////
//    static functions
////////////////////////////////////////////////////////////////////////////

template< typename Tkey, typename Tval >
template< typename IT >
IT
MPkMTvSparseVector< Tkey, Tval >::
find( const IT & begin, const IT & end, Tkey key  )
{
  IT it = begin;
  for ( ; it != end ; ++it )
  {
      if ( it.key() == key )
          break;
  }
  return it;
}


  
  

////////////////////////////////////////////////////////////////////////////
//    MPkMTvSparseVector< Tkey, Tval > :: const_iterator
////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
//    Public methods
////////////////////////////////////////////////////////////////////////////

template< typename Tkey, typename Tval >
bool
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator == ( const const_iterator& it ) const
{// check index first since you usually compare iterators from the same instance
    return _index == it._index && _const_parent == it._const_parent;
}

template< typename Tkey, typename Tval >
bool
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator != ( const const_iterator& it ) const
{// check for equality since you the iterators are likely to be differents
    return ! operator==( it );
}

template< typename Tkey, typename Tval >
bool
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator < ( const const_iterator& it ) const
{
    return _index < it._index;
}

template< typename Tkey, typename Tval >
bool
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator > ( const const_iterator& it ) const
{
    return _index > it._index;
}

template< typename Tkey, typename Tval >
bool
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator <= ( const const_iterator& it ) const
{
    return _index <= it._index;
}

template< typename Tkey, typename Tval >
bool
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator >= ( const const_iterator& it ) const
{
    return _index >= it._index;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::difference_type
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator - ( const const_iterator& it ) const 
{
    // get the (volatile) size only once to ensure that 0 == (end() - end())
    size_type size = _const_parent->size();
    
    // get first index, handling the 'end' special case
    difference_type i1 = _index;
    if ( i1 == end( _const_parent ).index() )
        i1 = size;
    
    // get second index, handling the 'end' special case
    difference_type i2 = it._index;
    if ( i2 == end( it._const_parent ).index() )
        i2 = size;
    
    return i1 - i2;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator + ( const difference_type & offset ) const 
{
    return const_iterator( *this ) += offset;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator - ( const difference_type & offset ) const 
{
    return const_iterator( *this ) -= offset;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator&
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator += ( const difference_type & d )
{
    // get the (volatile) size only once to ensure that (end() += 0) == end()
    size_type size = _const_parent->size();
    
    // check if we start with the 'end' iterator
    if ( _index == end( _const_parent ).index() )
        _index = size;
    
    _index += d;
    
    // check if new index equals size (ie: end of the container)
    if ( _index == size )
        _index = end( _const_parent ).index();
    
    return *this;
}

// increment/decrement operators
template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator&
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator ++ ()
{
    // check if new index equals size (ie: end of the container)
    if ( ++_index == _const_parent->size() )
    {
        // Set manually equal to the end() iterator. 
        _index = end( _const_parent ).index(); // *this = end( _const_parent );
    }
    return *this;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator&
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator -- ()
{
    // check if we decrement the end iterator
    if ( _index-- == end( _const_parent ).index() ) // if ( *this == end( _const_parent ) )
    {
        // Set manually to the last element
        _index = _const_parent->size() - 1;
    }
    return *this;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator ++ ( int )
{
    const_iterator res = *this;
    this->operator++();
    return res;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
operator -- ( int )
{
    const_iterator res = *this;
    this->operator--();
    return res;
}

////////////////////////////////////////////////////////////////////////////
//    Public static functions
////////////////////////////////////////////////////////////////////////////

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
begin( const MPkMTvSparseVector<Tkey,Tval> *parent )
{   // equals end(parent) if the container is empty
    return const_iterator( parent, parent->empty() * INT_MAX );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::const_iterator
MPkMTvSparseVector< Tkey, Tval >::const_iterator::
end( const MPkMTvSparseVector<Tkey,Tval> *parent )
{   // Independent from the size of the container. Otherwise, it would
    //      turn into a valid one upon a key insertion in the shared container.
    // \note The chosen value must fit in both \c 'this->_index' and
    //      \c 'parent->_mappingIndex[0]'
    return const_iterator( parent, INT_MAX );
}








////////////////////////////////////////////////////////////////////////////
//    MPkMTvSparseVector< Tkey, Tval > :: iterator
////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
//    Public methods
////////////////////////////////////////////////////////////////////////////

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator + ( const difference_type & offset ) const
{
    return iterator( *this ) += offset;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator - ( const difference_type & offset ) const
{
    return iterator( *this ) -= offset;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator&
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator += ( const difference_type & d )
{
    return static_cast< iterator& >( const_iterator::operator+=( d ) );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator&
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator -= ( const difference_type & d )
{
    return static_cast< iterator& >( const_iterator::operator-=( d ) );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator&
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator ++ ( )
{
    return static_cast< iterator& >( const_iterator::operator++() );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator&
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator -- ( )
{
    return static_cast< iterator& >( const_iterator::operator--() );
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator ++ ( int )
{
    iterator res = *this;
    this->operator++();
    return res;
}

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::iterator::
operator -- ( int )
{
    iterator res = *this;
    this->operator--();
    return res;
}

////////////////////////////////////////////////////////////////////////////
//    Public static functions
////////////////////////////////////////////////////////////////////////////

template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::iterator::
begin( MPkMTvSparseVector<Tkey,Tval> *parent )
{
    return iterator( parent, const_iterator::begin( parent ).index() );
}


template< typename Tkey, typename Tval >
typename MPkMTvSparseVector< Tkey, Tval >::iterator
MPkMTvSparseVector< Tkey, Tval >::iterator::
end( MPkMTvSparseVector<Tkey,Tval> *parent )
{
    return iterator( parent, const_iterator::end( parent ).index() );
}










        
} // namespace mem

} // namespace flowvr
#endif
