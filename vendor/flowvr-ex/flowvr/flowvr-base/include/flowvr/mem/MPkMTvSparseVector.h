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
* File: include/flowvr/flowvr-base/mem/MPkMTvSparseVector.h       *
*                                                                 *
* Contacts:                                                       *
*  29/11/2012 Jeremy Jaussaud <Jeremy.Jaussaud@inria.fr>          *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_SHAREDMEM_MPKLCSPARSEVECTOR_H
#define FLOWVR_SHAREDMEM_MPKLCSPARSEVECTOR_H


#include <type_traits>
#include "flowvr/common.h"
#include "flowvr/allocator.h"
#include "flowvr/buffer.h"

#include "flowvr/ipc/atomic.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/ipc/mplock.h"

#include <cstddef> // ptrdiff_t



namespace flowvr
{

class BufferWrite;

namespace mem
{

class MPBuffer;


/**
 * \class MPkMTvSparseVector
 * \brief Specialized sparse vector storing MP integral keys in shared memory
 *  with some local mapped values.
 *
 * The purpose of this structure is to offer some sort of a specialized map.
 *  The list of inserted keys is shared by several instances, each whithin a
 *  different process. On the contrary, mapped values are local to the instance
 *  and hidden to other ones.
 *
 * The instance is meant to be shared by several thread while the list of keys
 *  is meant to be shared by many processes. To enable performance, you can read
 *  the data structure without having to perform any lock or atomic operation,
 *  even if another process insert elements. A lock will however be necessary
 *  for concurrent insertions (or setting of the mapped values) and it's up
 *  to the user to ensure the safety using locks supplied by the class.
 *
 * While being fully compliant with its primary usage, it is not possible to
 *  remove or change a key inserted key in the container and you cannot change
 *  the capacity of the container as well. Futhermore, it is not possible to
 *  change a mapped value once it's set while there is a concurrent access to
 *  the instance. Finaly, range-checking is performed neither when inserting a
 *  new key nor when accessing an already inserted element.
 *
 * \tparam Tkey integral type of the keys from which we get the elements back
 * \tparam Tval type of the elements stored in the container
 */
template< typename Tkey, typename Tval >
class MPkMTvSparseVector
{    
public:
    
  ////////////////////////////////////////////////////////////////////////////
  //    Public types
  ////////////////////////////////////////////////////////////////////////////
    
  //////////////////////////////////////
  //    container types
    
  class iterator;
  class const_iterator;
  class reverse_iterator;
  class const_reverse_iterator;

  typedef Tkey                  key_type;       //  must be integral type!
  typedef Tval                  mapped_type;
  typedef mapped_type           value_type;
  typedef std::size_t           size_type;
  typedef std::ptrdiff_t        difference_type;



  typedef volatile value_type&  reference;
  // typedef value_type&  reference;
  //typedef const reference       const_reference;
  //typedef typename std::add_lvalue_reference<value_type>::type       reference;
  typedef typename std::add_lvalue_reference<const volatile value_type>::type const_reference;

  typedef volatile value_type*  pointer;
  typedef const pointer         const_pointer;
  
  //////////////////////////////////////
  //    shared data
  
  /**
   * @brief The MPData struct contain all the necessary shared data.
   *
   * An instance must be allocated in shared space (and initialized by the user)
   *  per group of 'MPkMTvSparseVector' sharing their key list. This data needed for
   *  initializing each instance of the group and its storage must live as long
   *  as the instances (as least one) initialized with it live.
   */
  struct MPData
  {
      ipc::MPLock       _insertLock;///< lock used for inserting a key
      
      Tkey              _KeyBegin;  ///< read-only once initialized
      Tkey              _KeyEnd;    ///< read-only once initialized
      size_t            _maxSize;   ///< read-only once initialized
              
      ipc::MPAtomicInt  _size;      ///< number of allocated areas
      volatile Tkey     _key[1];    ///< variable-length array
      
      /// initialize
      void init( int maxCount, Tkey KeyBegin, Tkey KeyEnd )
      {
          _insertLock.init();
          _KeyBegin = KeyBegin;
          _KeyEnd = KeyEnd;
          _maxSize = maxCount;
          _size.set( 0 );
      }
      
      static size_t size( int maxCount )
      {
          return sizeof(MPData) + (maxCount-1) * sizeof(key_type);
      }
  };  

  ////////////////////////////////////////////////////////////////////////////
  //    Public members
  ////////////////////////////////////////////////////////////////////////////
  
  ////////////////////////////////////////////////////////////////////////////
  //    Public methods
  ////////////////////////////////////////////////////////////////////////////
  
  //////////////////////////////////////
  //    construction / destruction
  
  /// construct an invalid instance that can be initialized afterward.
  MPkMTvSparseVector();
  
  /// copy-constructor
  MPkMTvSparseVector( const MPkMTvSparseVector& );
  
  /**
   * @brief construct an initialized instance of the class
   * @param mpBuf A buffer containing the shared data.
   * @param lockName the name that will be given to the mapping locks.
   */
  MPkMTvSparseVector( const BufferWrite& mpBuf );
  
  ///< destructor
  ~MPkMTvSparseVector();
  
  /// init a default-constructed instance.
  void init( const BufferWrite& mpBuf );
  
  /// copy-assignment operator
  MPkMTvSparseVector& operator = ( const MPkMTvSparseVector & );
  
  /// casts the instance to the (read-only) Buffer type.
  operator const Buffer& () const { return * static_cast< const flowvr::Buffer* >( _mpBuf ); }
    
  //////////////////////////////////////
  //    container informations
  
  bool valid() const { return NULL != _mpPointer; } ///< true if initialized
  bool empty() const { return 0 == _mpPointer->_size; } ///< you can guess...
  size_t size() const { return _mpPointer->_size; } ///< number of inserted keys
  size_t max_size() const { return _maxSize; } ///< maximum number of keys

  Tkey keyRange_begin() const { return _KeyBegin; } ///< minimum value for keys
  Tkey keyRange_end() const { return _KeyEnd; } ///< 1 + maximum value for keys
  
  //////////////////////////////////////
  //    mutators

  /**
   * @brief insert Insert a new key/value couple into the container
   *
   * This is the only method allowing to modify the container itself since you
   *  can't remove an inserted key. It is necessary to protect the calls to
   *  this method using the lock given by the \c getInsertLock method. This way,
   *  you can protect at the same time the creation of the mapped value itself.
   *
   * @param key The shared key to insert into the container
   * @param val The value the key will map in this peculiar instance
   *
   * @see getInsertLock
   */
  void insert( Tkey key, const_reference val );
  
  /**
   * @brief map Allow to map a value to an already inserted key that don't map
   *    any value yet within this instance.
   *
   * It is necessary to protect the calls to this method using the lock given by
   *  the \c getMappingLock method. This way, you can protect at the same time
   *  the creation of the mapped value itself.
   *
   * @param key A shared key previously inserted in the container and that don't
   *    map any value yet whithin the instance.
   * @param val The value the key will map in this peculiar instance
   *
   * @see unmap, getMappingLock
   */
  void map( Tkey key, const_reference value );
  
  /**
   * @brief unmap Flag the key as unmapped. The mapped value isn't destructed
   *
   * @param key A shared key previously inserted in the container.
   *
   * \attention This function shall not be called while another thread
   *    performs a concurrent access to the mapping of \c key.
   */
  void unmap( Tkey key );
  
  /**
   * @brief getInsertLock get a lock to protect a key insertion
   *
   * @return a reference to the lock protecting the key insertion
   *
   * @see insert
   */
  ipc::MPLock& getInsertLock() { return _mpPointer->_insertLock; }
  
  /**
   * @brief getMappingLock get a lock to protect the modification of the mapping
   *    of a key of which as already been inserted in the container.
   *
   * @param key a key that is already contained in the shared list
   * @return a reference to the lock protecting the modification of the local
   *    mapping of the shared \c key
   *
   * @see map, unmap
   */
  ipc::MTLock& getMappingLock( Tkey key ) { return _mappingLock[ find( key ).index() ]; }
  
  //////////////////////////////////////
  //    accessors
  
  /**
   * @brief getMapped return an iterator to the (local) value mapped by a key.
   *    The key must be effectively mapping a value.
   * @note This methods performs in constant time.
   * @param key a key which have been previously inserted in the container
   * @return iterator to the mapped value, or \c end iterator if the given key
   *    does not map any value yet.
   */
  const_iterator getMapped( Tkey key ) const { return const_iterator( this, mappingIndex(key) ); }
  
  /**
   * @brief find allows to search a key in the shared list of the container
   *    no matter if this key is inserted or mapping a value.
   * @param key to search in the container
   * @return iterator to the searched key, \c end iterator if it isn't inserted
   */
  iterator find( Tkey key ) { return find( begin(), end(), key ); }
  
  /**
   * @brief find allows to search a key in the shared list of the container
   *    no matter if this key is inserted or mapping a value.
   * @param key to search in the container
   * @return iterator to the searched key, \c end iterator if it isn't inserted
   */
  const_iterator find( Tkey key ) const { return find( begin(), end(), key ); }

  //////////////////////////////////////
  //    begin-end methods
  
  iterator begin();
  inline iterator end();
  inline const_iterator begin() const;
  inline const_iterator end() const;
  
  inline reverse_iterator rbegin();
  inline reverse_iterator rend();
  inline const_reverse_iterator rbegin() const;
  inline const_reverse_iterator rend() const;


protected:
    
  ////////////////////////////////////////////////////////////////////////////
  //    Protected members
  ////////////////////////////////////////////////////////////////////////////
    
  ////////////////////////////////////////////////////////////////////////////
  //    Protected methods
  ////////////////////////////////////////////////////////////////////////////
  
  /**
   * @brief destroy Destroys the instance. Do nothing if the instance is
   *    default-constructed
   */
  void destroy();
  
  /// returns A const reference to the atomic status of the mapping.
  const ipc::MTAtomicInt& mappingIndex( Tkey key ) const { return _mappingIndex[ key - _KeyBegin ]; }
  /// returns A reference to the atomic status of the mapping
  ipc::MTAtomicInt& mappingIndex( Tkey key ) { return _mappingIndex[ key - _KeyBegin ]; }
  
  ////////////////////////////////////////////////////////////////////////////
  //    Protected static function
  ////////////////////////////////////////////////////////////////////////////
  
  /**
   * @brief find allows to search a part of the container for a given key
   * 
   * @tparam IT the type of the iterators passed as parameters
   * @param begin the iterator of a \c MPkMTvSparseVector where to begin the search
   * @param end the iterator of the same \c MPkMTvSparseVector where to end the search
   * @param key the key you are lokking for
   * @return An iterator to the found element or the \c end parameter
   */
  template< typename IT >
  static IT find( const IT & begin, const IT & end, Tkey key );


private:
  
  ////////////////////////////////////////////////////////////////////////////
  //    Private members
  ////////////////////////////////////////////////////////////////////////////
  
  //////////////////////////////////////
  //    shared-memory members
  
  /**
   * \brief Buffer of the shared memory containing the shared data.
   *
   * We must keep a local copy for the sake of reference counting.
   **/
  BufferWrite * _mpBuf;
  
  /**
   * \brief Pointer to the shared memory storing the shared data.
   **/
  MPData * _mpPointer;
  
  //////////////////////////////////////
  //    local copies of const values
  
  Tkey _KeyBegin;   ///< begining of the possible range (subset) of keys used
  Tkey _KeyEnd;     ///< begining of the possible range (subset) of keys used
  size_t _maxSize;  ///< maximum size of the container
    
  //////////////////////////////////////
  //    variable-length array
  
  /**
   * \brief Shared array storing the keys by insertion order
   *
   * This array (size \c _maxSize) is allocated outside of the class.
   **/
  volatile Tkey *_key;
  
  /**
   * \brief Array allocated dynamically storing the values.
   *
   * Its size (\c _maxSize) is fixed at initialization time
   **/
  volatile Tval *_val;
  
  /**
   * \brief Array whose each value is set to the index of the corresponding key
   *    in the insertion list if it is effectively inserted and mapping a value.
   *    Otherwise, it stores the index of the \c end() iterator.
   *
   * Its size (\c _KeyEnd - \c _KeyBegin) is fixed at initialization time.
   **/
  ipc::MTAtomicInt *_mappingIndex;
  
  /**
   * \brief Array allocated dynamically storing MT locks to protect the
   *    modification of the mapped values.
   *
   * Its size (\c _maxSize) is fixed at initialization time
   **/
  ipc::MTLock *_mappingLock;
  
  ////////////////////////////////////////////////////////////////////////////
  //    Private methods
  ////////////////////////////////////////////////////////////////////////////
  
};


/**
 * \c const_iterator is the class that truly implements the iterator of the
 *  \c MPkMTvSparseVector class. The implementations of others iterators rely
 *  entirely on this one.
 *
 * @note No iterator is invalidated when inserting a new key in the container or
 *  by mapping a new value. One consequence is that the end iterator of a given
 *  container will always have the same index.
 */
template< typename Tkey, typename Tval >
class MPkMTvSparseVector<Tkey,Tval>::const_iterator
{
public:
    
  ////////////////////////////////////////////////////////////////////////////
  //    Public types
  ////////////////////////////////////////////////////////////////////////////
    
  typedef typename std::random_access_iterator_tag     iterator_category;
    
  //////////////////////////////////////
  //    types from the container
    
  typedef typename MPkMTvSparseVector<Tkey,Tval>::value_type        value_type;
  typedef typename MPkMTvSparseVector<Tkey,Tval>::reference         reference;
  typedef typename MPkMTvSparseVector<Tkey,Tval>::const_reference   const_reference;
  typedef typename MPkMTvSparseVector<Tkey,Tval>::size_type         size_type;
  typedef typename MPkMTvSparseVector<Tkey,Tval>::difference_type   difference_type;
  typedef typename MPkMTvSparseVector<Tkey,Tval>::pointer           pointer;
  typedef typename MPkMTvSparseVector<Tkey,Tval>::const_pointer     const_pointer;
    
  ////////////////////////////////////////////////////////////////////////////
  //    Public methods
  ////////////////////////////////////////////////////////////////////////////

  inline const_iterator( const MPkMTvSparseVector<Tkey,Tval> *parent, int index )
      : _const_parent( parent )
      , _index( index )
  {}

  //////////////////////////////////////
  //    accessors
    
  /// index of the key within the container, valid for 'end' iterators.
  int index() const { return _index; }
  
  /// key associed with the element pointed by a valid ( != \c end ) iterator
  Tkey key() const { return _const_parent->_key[ index() ]; }
  /// ref to the mapped value pointed by a valid ( != \c end ) iterator
  const_reference value() const { return _const_parent->_val[ index() ]; }
  /// \c true if the associed key is mapping a value
  bool isMapping() const { return _index == _const_parent->mappingIndex( key() ); }
  
  const_reference operator * () const { return value(); }
  const_pointer operator -> () const { return & value(); }
  const_reference operator [] ( difference_type i ) const { return *(*this+i); }
  
  //////////////////////////////////////
  //    comparison operators
  
  inline bool operator == ( const const_iterator& it ) const;
  inline bool operator != ( const const_iterator& it ) const;
  inline bool operator <  ( const const_iterator& it ) const;
  inline bool operator >  ( const const_iterator& it ) const;
  inline bool operator <= ( const const_iterator& it ) const;
  inline bool operator >= ( const const_iterator& it ) const;
  
  //////////////////////////////////////
  //    arithmetic operators
  
  difference_type operator - ( const const_iterator& it ) const ;
  inline const_iterator operator + ( const difference_type& offset ) const ;
  inline const_iterator operator - ( const difference_type& offset ) const ;
  friend const_iterator operator + ( const difference_type& offset, const const_iterator& it ) { return it + offset; }
  
  //////////////////////////////////////
  //    compound assignment operators
  
  const_iterator& operator += ( const difference_type& );
  const_iterator& operator -= ( const difference_type& d ) { return *this += -d; }
  
  //////////////////////////////////////
  //   increment / decrement operators
  
  inline const_iterator& operator ++ ();
  inline const_iterator& operator -- ();
  inline const_iterator operator ++ ( int );
  inline const_iterator operator -- ( int );
  
  ////////////////////////////////////////////////////////////////////////////
  //    Public static functions
  ////////////////////////////////////////////////////////////////////////////

  /// return the begin iterator of \p parent
  inline static const_iterator begin( const MPkMTvSparseVector<Tkey,Tval> *parent );
  /// return the end iterator of \p parent
  inline static const_iterator end( const MPkMTvSparseVector<Tkey,Tval> *parent );

private:
  
  ////////////////////////////////////////////////////////////////////////////
  //    Private members
  ////////////////////////////////////////////////////////////////////////////
  
  /**
   * @brief _const_parent a pointer to the container
   */
  const MPkMTvSparseVector<Tkey,Tval> *_const_parent;
  
  /**
   * @brief _index the index in the container.
   *
   * \note Must be signed to store the \c -1 value, the index of the iterators
   *      returned by the \c rbegin methods.
   */
  int _index;
  
};


/**
 * The implementation of this class rely entirely on those of 'MPkMTvSparseVector'
 *   and 'MPkMTvSparseVector::const_iterator'.
 * @see MPkMTvSparseVector::const_iterator
 */
template< typename Tkey, typename Tval >
class MPkMTvSparseVector<Tkey,Tval>::iterator
        : public MPkMTvSparseVector<Tkey,Tval>::const_iterator
{
public:
    
  inline iterator( MPkMTvSparseVector< Tkey, Tval > *parent, int index )
      : const_iterator( parent, index )
      , _parent( parent )
  {}
    
  reference value() const { return _parent->_val[ this->index() ]; }
  reference operator * () const { return value(); }
  pointer operator -> () const { return & value(); }
  reference operator [] ( const difference_type & i ) const { return *(*this+i); }
  
  inline iterator operator + ( const difference_type & offset ) const ;
  inline iterator operator - ( const difference_type & offset ) const ;
  friend iterator operator + ( const difference_type & offset, const iterator &it ) { return it + offset; }
  
  inline iterator& operator += ( const difference_type & );
  inline iterator& operator -= ( const difference_type & );
  
  inline iterator& operator ++ ();
  inline iterator& operator -- ();
  inline iterator operator ++ ( int );
  inline iterator operator -- ( int );
  
  static inline iterator begin( MPkMTvSparseVector<Tkey,Tval> * parent );
  static inline iterator end( MPkMTvSparseVector<Tkey,Tval> * parent );

private:
    
  MPkMTvSparseVector<Tkey,Tval> *_parent;
    
};


/**
 * wrapper inhiriting of std::reverse_iterator<...> in order to add to it the
 *   \c index(), \c key(), \c value() and \c isMapping methods
 * @see MPkMTvSparseVector::const_iterator
 */
template< typename Tkey, typename Tval >
struct MPkMTvSparseVector<Tkey,Tval>::const_reverse_iterator
        : public std::reverse_iterator< typename MPkMTvSparseVector<Tkey,Tval>::const_iterator >
{
  const_reverse_iterator( const const_iterator& it )
      : std::reverse_iterator< typename MPkMTvSparseVector<Tkey,Tval>::const_iterator >( it )
  {}
  
  int index() const { return (--this->base()).index(); }
  Tkey key() const { return (--this->base()).key(); }
  const_reference value() const { return (--this->base()).value(); }
  bool isMapping() const { return (--this->base()).isMapping(); }
};


/**
 * wrapper inhiriting of std::reverse_iterator<...> in order to add to it the
 *   \c index(), \c key(), \c value() and \c isMapping methods from iterator
 * @see MPkMTvSparseVector::iterator
 */
template< typename Tkey, typename Tval >
struct MPkMTvSparseVector<Tkey,Tval>::reverse_iterator
        : public std::reverse_iterator< typename MPkMTvSparseVector<Tkey,Tval>::iterator >
{
  reverse_iterator( const iterator& it )
      : std::reverse_iterator< typename MPkMTvSparseVector<Tkey,Tval>::iterator >( it )
  {}
    
  int index() const { return (--this->base()).index(); }
  Tkey key() const { return (--this->base()).key(); }
  reference value() const { return (--this->base()).value(); }
  bool isMapping() const { return (--this->base()).isMapping(); }
};



        
} // namespace mem

} // namespace flowvr
#endif
