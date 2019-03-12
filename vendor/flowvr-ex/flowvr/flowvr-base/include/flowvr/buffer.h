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
* File: include/flowvr/buffer.h                                   *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_BUFFER_H
#define FLOWVR_BUFFER_H

#include "flowvr/common.h"
#include "flowvr/bufferimp.h"
#include <vector>


namespace flowvr
{

class Allocator;
class BufferWrite;


/**
 *  @brief Plain interface to a memory buffer.
 *
 *  A buffer is just a 'header' structure to work on an 'implementation' of a buffer which
 *  holds the data itself. A buffer can be used to show all of the implementation buffer,
 *  but can also be parameterized to show a 'window' of the original implementation buffer.
 *  The buffer structure works on the reference count and avoids memory copies of the
 *  real data, unless noted otherwise.
 *  A buffer can only be read, as only const pointers are given by the API. In case you
 *  need write access to a buffer implementation, use BufferWrite instead.
 *
 *  A Buffer can be constructed using <emph>segments</emph>. Segments can be 'added' or
 *  'subtracted' from a buffer by passing other Buffers. The cost for adding a segment
 *  is low, it just claims the other buffer's implementations. However, the memory in a
 *  segmented buffer is <emph>non-contiguous</emph>! You should access it carefully, always
 *  iterating over segment indices or use a Buffer::linearize() first.
 *
 *  @see BufferWrite
 *  @ingroup Messagehandling
 */
class Buffer
{
 public:
  /// @name Construction
  /// @{

	/**
	* @brief destructor
	*/
	virtual ~Buffer();


  /**
   * @brief Constructs an invalid buffer.
   *
   * Creates an invalid buffer with no implementation, no size and no data.
   * A buffer like this can be used to signal allocation failure or can be
   * passed around and populated later.
   *
   * This Buffer is created and valid() will return false.
   */
  Buffer();

  /**
   * @brief Sub-buffer constructor from another buffer
   *
   * Can be used to construct a buffer-window from another buffer.
   * Note that Buffer(from, 0) is an alias to the copy constructor / copy operator (but more expensive).
   *
   * In case you specify an offset that is beyond the size of the original buffer, this buffer will reflect
   * a window of size 0 and is marked invalid.
   *
   * In case you try to have a larger window than the original buffer allows, this
   * buffer will reflect all segments of the original buffer, but the size will be clamped to the total size
   * of the original buffer.
   *
   * @param from the buffer to create the sub-window from. Can be invalid.
   * @param suboffset the offset to start in from; must not be larger than from.getSize(ALLSEGMENTS)-1
   * @param subsize the size of the resulting window (~0 means to create a window
   *        [suboffset ; from.getSize()-suboffset]
   *
   */
  Buffer(const Buffer& from, size_t suboffset, size_t subsize=CLAIMALLSIZE);

  /**
   *  @brief Copy constructor
   */
  Buffer(const Buffer& from);

  /**
   * @brief Sub-buffer constructor from buffer implementation.
   *
   * In case you try to have a larger window than the original BufferImp allows, this
   * buffer size will be clamped against the largest value possible with the given implementation
   * and offset. Example: imp size = 100, suboffset = 50, subsize = 60 -> resulting size will be 50.
   *
   * @param fromimp the implementation to base the buffer window on.
   *        This pointer may not be NULL, the BufferImp is adopted.
   * @param suboffset the offset in bytes from fromimp->readAccess()
   *        to start this window from. Must not be larger than fromimp->getSize(), can be 0
   * @param subsize the length in bytes for this window.
   *        Note that the length is adjusted to be conform with the
   *        fromimp->getSize() constraint (i.e. the window can not be
   *        larger than the size of fromimp allows).
   *        A value of ~0 indicates to align the size of the window
   *        to match the tail of fromimp (take it all starting at suboffset).
   *        Can be 0.
   * @param open use open() instead of claim() on the implementation.
   *        Setting this to true is typically done by the memory-manager / allocator
   *        used (expert API).
   *
   * This whole API is related to low-level / expert operations.
   * @private
   */
  Buffer(BufferImp* fromimp, size_t suboffset, size_t subsize=CLAIMALLSIZE, bool open = false);

  /// @}


  /// @name operators
  /// @{

  /**
   *  @brief assignment operator
   *
   *  The assignment operator will close a current implementation and claim
   *  the implementation from buffer 'from'. The buffer will contain the
   *  same segments in the same order as from after the operation.
   *  All segments claimed in 'this' will be released during the assignment
   *  of all segments from 'from'.
   *  Note that 'from' can be 'this' (this is checked and *this returned without
   *  any operation in between).
   *
   *  @return *this always.
   */
  Buffer& operator=(const Buffer& from);

  /**
   * @brief Comparison operator.
   *
   * Two buffers are equal if
   *  - they contain the same number of segments
   *  - each segment from each buffer are pairwise equal (index by index)
   *
   * Note that the invalid buffer is only equal to the invalid buffer by this definition.
   *
   * @see _bufferDesc::equals()
   * @param b the buffer to compare this buffer to
   * @return true when b equals *this by the rational stated above
   */

  bool operator==(const Buffer& b) const;

  /**
   * @brief Comparison (not-equal) operator.
   *
   * @param b the other buffer to compare this against for un-equality.
   * @return !(*this == b)
   */
  bool operator!=(const Buffer& b) const
  {
    return !(*this == b);
  }


  /**
   * @brief convert buffer to type string
   *
   * The conversion operator <i>copies</i> this buffer to a string object.
   * In case your buffer is segmented, it will copy all segments segment-wise
   * to the resulting string. This impairs overhead, depending on the number of
   * segments of this buffer.
   *
   * @return a std::string representation of the contents of this buffer as a copy.
   */
  operator std::string() const;

  /// @}

  /// @name introspection
  /// @{
  /**
   * @brief query the buffer size (optionally by segment)
   * @param segmentIndex the index of the segment to get its size from,
   *        defaults to 0, pass Buffer::ALLSEGMENTS to get the total size of
   *        this buffer and all its segments.
   *
   * This method is basically an alias to getSegmentSize() with the option to
   * sum up the total size in all the buffers.
   *
   * @see getSegmentSize()
   * @see getNumberOfSegments()
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   *
   * @return the number of bytes contained by this buffer (within the segment by the index given).
   *         The invalid buffer (no segments at all) return a size of 0.
   */
  size_t getSize( size_t segmentIndex = 0 ) const;

  /**
   * @brief test whether this buffer is empty.
   *
   * A buffer is empty when
   * - it is invalid
   * - the total size of all segments is 0
   *
   * @return true if there is any data covered by this buffer (is true for an alloc'ed(0) buffer)
   */
  bool empty() const;

  /**
   *  @brief Return true if this buffer is valid, false otherwise
   *
   * A buffer is valid if
   * - it does contain at least one segment (for example a valid implementation on a subwindow of size 0)
   * - at least one segment is valid
   *
   * @return
   *         - false: if this buffer contains no segments
   *         - true: at least one segment is found valid.
   */
  bool valid() const;


  /**
   * @brief see whether there is only one owner of our implementation
   *
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   * @param the segment index to test (or ALLSEGMENTS to test them all)
   *
   * @return
   *    - false when this buffer is not valid (empty buffers are not unique)
   *    - when segmentIndex is ALLSEGMENTS:
   *      at least one segment is not unique
   *    - else the uniqueness of segment at index 'segmentIndex'
   */
  bool unique(size_t segmentIndex = 0) const;

  /// @}

  /// @name accessors
  /// @{

  /**
   *  @brief get the pointer to read the buffer data
   *  @param segment the segment index to get read access to, you may not pass ALLSEGMENTS here.
   *
   *  Note that this is a low level operation, so when using segments do only expect to
   *  read getSegmentSize(segment) number of bytes as continuous space.
   *
   *  @return the pointer to the first byte in this buffer at the given segment, or NULL
   *          in case this buffer is not valid.
   *
   *  @pre 0 <= segmentIndex < getNumberOfSegments()
   *  @see linearize()
   *  @see valid()
   */
  const ubyte* readAccess( size_t segment = 0 ) const;

  /**
   * @brief claim a read-pointer to the data in this buffer of type const T*
   *
   * @param pos the offset in the data <i>in bytes</i>, as an offset from the
   *        start of this buffer.
   * @param segment the segment to get read access from as type T
   * @warning the offset <i>pos</i> is not in units of size(T).
   *          Note that this is a low-level API, to not expect to have
   *          more than getSegmentSize(segment)/sizeof(T) units to read from the segment
   *          in one row.
   *
   * @see linearize()
   * @todo modify the API to return the unit of read (e.g., pos = n*sizeof(T))?
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   * @pre 0 <= pos < getSegmentSize(segment)
   *
   * @return
   *         - a pointer to the data of segment 'segment' starting there with a local
   *         offset of 'pos'
   *         - NULL when this buffer contains no segments (valid() == false)
   */
  template<class T>
    const T* getRead(size_t pos=0, size_t segment = 0) const
    {
	  if( segments.empty() )
		  return NULL;
	  return (const T*) ((*this).segments[segment].pointer+pos);
    }


  /// @}



  /// @name expert / low-level API
  /// @{
  /**
   *  @brief Return the allocator used to allocate this buffer at the give segment.
   *
   *  Though it is not wise to use different allocators under the hood of one buffer,
   *  this API gets the allocator that was use to allocate the memory for the given
   *  segment.
   *
   *  In case this buffer is not valid, or the segment queries was not allocated with
   *  an allocator, Allocator::getDefaultAllocator() is returned.
   *
   *  @pre 0 <= segmentIndex < getNumberOfSegments()
   */
  Allocator* getAllocator(size_t segment = 0) const;

  /**
   * @brief claim the implementation of this buffer for the given segment.
   *
   * @param segment the segment index to query
   * @return the implementation for the segment queried, or NULL when this buffer does
   *         not contain any segment (or the segment implementation is NULL).
   *
   * This is a debug API, typically to check the reference count on the implementation.
   * Normal users should not use it / need to use it.
   *
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   * @private
   */
  BufferImp *getImp(size_t segment = 0) const
  {
	  if( segments.empty() )
		  return NULL;
	  return segments[segment].imp;
  }

  /**
   * @brief return offset given during construction
   *
   * @param segment the segment index to query.
   *
   * @return the offset of the buffer in its implementation
   *         - 0 in case this buffer is not valid
   *         - the offset of the segment else
   *
   * This is an internal API, typically used on low level operations
   * on the physical memory block. Normal users should ignore it.
   *
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   * @private
   */
  size_t     getOffset(size_t segment = 0) const
  {
	  if( segments.empty() )
		  return 0;
	  return segments[segment].offset;
  }

  /// @}


  /// @name state
  /// @{

  /**
   * @brief flush the buffer contents.
   *
   * All claimed buffer implementations will be released.
   * After a call to clear, this buffer will be invalid.
   *
   * @post valid() == false
   */
  void clear()
  {
	  segments.clear();
	  segmentsSize = INVALIDSEGMENTSIZE;
  }

  /// @}


  ///@name segments
  ///@{
  /**
   * @brief adds a buffer to this buffer as direct sub-buffer
   *
   * Adding means to merge segments in the returned buffer, so
   * assume a and b are buffers with segments a.1, a.2 and b.1
   * the result c=a+b will comprise {a.1, a.2, b.1} in that order.
   *
   * Invalid buffers do not add anything to another buffer.
   *
   * @param other the buffer to add to this buffer.
   * @return a new buffer containing *this + other
   */
  Buffer operator+( const Buffer &other ) const;

  /**
   * @brief erase a buffer from this buffer
   *
   * Returns a buffer that contains the difference in segments between 'this' and 'other'.
   * Any segment that is in 'other' and that is found in '*this' will be removed from
   * the resulting buffer. So if a = { a.1, b.1, b.1 } and b = { b.1 } then c=a-b
   * will be c={a.1}.
   *
   * As stated above, all segments that compare 'equal' are removed (duplicates are eliminated).
   * The empty buffer will not change anything (quick pre-test).
   *
   * @param other the segments to remove from *this
   * @return a new buffer containing *this - other
   */
  Buffer operator-( const Buffer &other ) const;

  /**
   * @brief shorthand notation for adding a buffer to this buffer
   *
   * if this buffer contains {a.1, a.2} and other={c.1,a.1}, this
   * buffer will be *this + other = {a.1, a.2, c.1, a.1}.
   *
   * @see Buffer::operator+
   * @param other the buffer to add
   * @return *this
   */
  Buffer &operator+=( const Buffer &other );

  /**
   * @brief a shorthand notation for subtraction of a buffer from this buffer.
   *
   * This will change this buffer to contain only those segments that are not
   * found in 'other'. If 'this'={a.1, b.1} and 'other'={b.1} than
   * this - other={a.1} after this operation. Duplicate segments will be eliminated,
   * so all occurrences of b.1 will be erased in the above example.
   *
   * @param other the direct child to subtract from this buffer
   * @return *this
   * @see Buffer::operator-
   */
  Buffer &operator-=( const Buffer &other );

  /**
   * @brief returns the number of segments
   *
   * Invalid buffers return 0 here.
   *
   * @return the number of segments that are contained in this buffer.
   */
  size_t getNumberOfSegments() const;

  /**
   * @brief quick test to see whether this buffer contains more than 1 segment.
   *
   * Any buffer containing 0 or 1 segment is considered non-segmented.
   *
   * @return true if this buffer contains more than one segment.
   */
  bool getIsSegmented() const;


  /**
   * @brief get the size of an individual segment
   *
   * @param segmentIndex the index of the segment to look at.
   *
   * @return the size of the segment given at segmentIndex.
   *
   * @see getNumberOfSegments()
   * @pre 0 <= segmentsIndex < getNumberOfSegments()
   */
  size_t getSegmentSize(size_t segmentIndex) const;

  /**
   * @brief accessor to get access to a segment of this buffer by index.
   *
   * @param segment the segment index ( 0 <= segment < getNumberOfSegments() )
   * @return the buffer at index 'segment'
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   */
  const Buffer operator[](int segment) const;

  /**
   * @brief accessor to get access to a segment of this buffer by index
   *
   * @param segment the segment index ( 0 <= segment < getNumberOfSegments() )
   * @return the buffer at index 'segment'
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   */
  Buffer       operator[](int segment);

	/**
	* @brief helper structure to maintain segmented buffers.
	*
	* This API is for low-level or expert use only.
	*
	* @private
	*/
	class _bufferDesc
	{
	public:
	  ~_bufferDesc();
	  _bufferDesc();
	  _bufferDesc( const _bufferDesc &other );

	  /**
	   * @private
	   */
	  _bufferDesc( BufferImp *_imp, size_t _offset, size_t _size, ubyte *_pointer );


	  /**
	   * @brief assignment operator
	   */
	  _bufferDesc &operator=( const _bufferDesc &other);

	  /**
	   * @brief low-level ref-up operation
	   */
	  void reference();

	  /**
	   * @brief low-level ref-down operation
	   */
	  void dereference();

	  /**
	   * @brief equality operator
	   *
	   * two buffer descriptors are equal if:
	   * - they share the same implementation
	   * - they have the same offset
	   * - they have the same sub-window size
	   *
	   * @return see above
	   */
	  bool operator==( const _bufferDesc &other ) const;

	  /**
	   * @brief order operator
	   *
	   * Orders by pointer
	   *
	   * @return this->pointer < other.pointer
	   */
	  bool operator<( const _bufferDesc &other ) const;

	  /**
	   * @brief compares this to other for equality.
	   *
	   * This is an alias to _bufferDesc::operator==.
	   *
	   * @return
	   *         - true if other==*this
	   *         - false else
	   */
	  bool equals( const _bufferDesc &other ) const;

	  /**
	   * @brief test for the validity of the implementation
	   *
	   * @return
	   *          - true if this descriptor points to a non-zero implementation
	   *          - false else
	   */
	  bool valid() const;

	  /**
	   * @brief tests whether the implementation reports it is unique or not.
	   *
	   * @pre valid() == true
	   *
	   * @return
	   *          - false if the implementation is not unique
	   *          - true else
	   */
	  bool unique() const;

	  Buffer toBuffer() const;

	  BufferImp* imp;     ///< Implementation of the containing buffer
	  size_t     offset;  ///< Offset inside the real buffer
	  size_t     size;    ///< Size of this (sub)buffer
	  ubyte*     pointer; ///< pointer to the memory to work on (short-hand notation / cache of imp->readAccess() + offset)
	};

  /**
   * @brief accessor the the segment buffer (may be handy for expert level access without friends)
   * Use with care.
   *
   * @return a reference to the vector of segments descriptors of this buffer
   * @private
   */
  const std::vector<_bufferDesc> &getSegments() const;

  /**
   * @brief accessor the the segment buffer (may be handy for expert level access without friends)
   * Use with care.
   *
   * @return a reference to the vector of segment descriptors of this buffer (direct children)
   * @private
   */
  std::vector<_bufferDesc> &getSegments();

  /**
   * @brief complete equality test for this buffer and all its segments
   *
   * @return true when
   *    - other == &this
   * 	- equals() was true for each segment (pairwise) in 'this' and 'other' buffer
   *    - false else (i.e. when buffers mismatch in size or at least one buffer does not equal pairwise)
   *
   * @see _bufferDesc::equals()
   */
  bool equals( const Buffer &other ) const;

  /**
   * @brief get a continuous (one segment) version of this buffer.
   *
   * In case this buffer is segmented, linearize() will glue all segments in a single segment.
   * This method calls 'memcpy' several times and as such involves costs. A buffer that contains just
   * 1 segment is passed back, no copy costs involved.
   *
   *
   * @return the buffer representing a 'linearized' copy of this buffer.
   *         - the invalid buffer will return *this
   *         - a buffer with 1 segment will return Buffer(*this)
   *         - a new buffer will be allocated (size: getSize(ALLSEGMENTS)) and all data
   *           will be <emph>copied</emph> to this new buffer.
   */
  Buffer linearize() const;

  /**
   * @brief copy-out method (to copy from flowvr-memory to conventional memory)
   *
   * @pre    mem-block starting at dest is of size getSize(ALLSEGMENTS)
   * @param  dest the destination pointer to write this buffer to
   *         as a continuous blob of memory.
   *
   * The invalid buffer does not copy anything, the empty buffer will copy nothing as well,
   * but copyTo will take slightly longer to detect that.
   * This method probably calls a sequence of memcpy operations, so it induces costs.
   */
  void copyTo( void *dest );

  /// @}



  static const size_t ALLSEGMENTS, /**< constant to be passed to getSize() and unique() to regard all segments attached */
                      INVALIDSEGMENTSIZE,  /**< internally used constant to mark an invalid segments size (invalid buffer) */
                      CLAIMALLSIZE; /**< argument to be passed to subwindow-constructors to specify 'take-it-all' */

 protected:
	  /**
	   * @brief helper API to find a segment for a given <i>absolute</i> offset in the data space represented
	   *        by all segments (so within [0..getSize(ALLSEGMENTS)).
	   *
	   *
	   * @param the <i>absolute</i> offset within the segment space to locate
	   * @param fistOffset (out-parameter) the relative offset in the segment that contains 'offset'
	   *        (undefined in case the offset could not be located)
	   * @param remainingsize (out-parameter) the number of bytes that remain in the segment when starting
	   *        at firstOffset (for sub-window construction)
	   *
	   * @return
	   *         - segments.end() in case 'offset' could not be located.
	   *         - the iterator representing the segment that contains 'offset' as absolute index.
	   *
	   * @warning always check if the return value is segments.end() or not before using any of the out-parameters.
	   *           They will not be initialized to a decent value.
	   */
  std::vector<_bufferDesc>::const_iterator findFirstForOffset( size_t offset,
		                                                       std::vector<_bufferDesc>::size_type &firstOffset,
		                                                       std::vector<_bufferDesc>::size_type &remainingsize ) const;

  /**
   * @brief helper function to locate a segment that contains the index given by subsize.
   *
   * This method makes more sense in conjunction with findFirstForOffset().
   * It is used to locate the successor of the last segment that is spanned by a sub-window-size specifier 'subsize'.
   *
   *
   * @param startPos the starting segment to search from
   * @param firstOffset the offset in this first segment (startPos) to regard as 0 of the relative window spanned by subsize
   * @param lastSize (out-parameter) the pre-fix size of the returned segment.
   *
   * @return the successor of the segment that contains the index given by subsize (can be segments.end()).
   */
  std::vector<_bufferDesc>::const_iterator findLastForSize( std::vector<_bufferDesc>::const_iterator startPos,
		                                              size_t subsize,
		                                              size_t firstOffset,
		                                              std::vector<_bufferDesc>::size_type &lastSize) const;


  std::vector<_bufferDesc> segments;
  mutable std::vector<_bufferDesc>::size_type segmentsSize;
}; // class Buffer

/**
 * @brief interface to access a buffer for writing
 *
 * Write interface for a buffer given.
 * Can also be used (of course) to just read data, but for the sake of clarity,
 * you should use a simple Buffer for that purpose.
 * The BufferWrite can be used to enable writing in sub-windows of other Buffers.
 *
 * @ingroup Messagehandling
 */
class BufferWrite : public Buffer
{
 public:

  /// @name construction and assignment
  /// @{
  /**
   * Creates an invalid BufferWrite with a NULL write-pointer.
   */
  BufferWrite()
  {}

  /**
   * @brief Copy constructor.
   */
  BufferWrite(const BufferWrite& from)
  : Buffer(from)
  {}


  /**
   * @brief Sub-buffer constructor.
   *
   * This constructor can be used to create a writable area on top of
   * another given BufferWrite.
   * When using this constructor, you effectively create a writable buffer
   * that mimics [ from[suboffset] ; from[ suboffset+subsize ] ]
   *
   * @param from the buffer to excerpt a buffer-window from
   * @param suboffset the offset for the data-window relative to from[0]
   * @param subsize the number of bytes to have in the window.
   *        Note that the window can not be larger than the original buffer.
   */
  BufferWrite(const BufferWrite& from, size_t suboffset, size_t subsize=-1)
  : Buffer(from, suboffset, subsize)
  {}


  /**
   * @brief Sub-buffer constructor from buffer implementation.
   *
   * This constructor is typically used when creating a buffer from a new implementation.
   *
   *
   * @param fromimp the implementation to sub-window
   * @param suboffset the offset to the absolute start of this buffer
   * @param subsize the number of bytes to be contained in this window
   *        (can not use more bytes than in the original buffer has).
   * @param open use open instead of claim on the implementation.
   *        Should be set to 'true' after the buffer was created, false else.
   *        This is typically done by the Allocator used.
   */
  BufferWrite(BufferImp* fromimp, size_t suboffset, size_t subsize=-1, bool open=false)
    : Buffer(fromimp, suboffset, subsize, open)
  {
  }

  /// @}

  /// @name operators
  /// @{
  /**
   *  @brief Copy operator.
   *
   *  Simply extends Buffer::operator= to BufferWrite.
   *  @return *this
   */
  BufferWrite& operator=(const BufferWrite& from)
  {
	  if( &from == this )
		  return *this;

	  Buffer::operator=( from );

	  return *this;
  }

  /// @}


  /// @name size management
  /// @{

  /**
   * @brief fine controlled size change of one segment of a buffer
   *
   * Reserve at least minSize bytes for segment storage.
   * Note that it is not possible to change the size of an invalid buffer, as we need an explicit
   * Allocator to do the dirty work.
   *
   * @todo check behavior on buffers that are not unique (unique ?)
   *
   *
   * @param requestedMinSize the minimum number of bytes to have for this buffer.
   *        Note: the buffer finally allocated can be bigger, but is never smaller
   *        (with the exception of a failure of allocation)
   * @param expandOnly if set to true, the reserve will fail when requestedMinSize is
   *        smaller than the current size. The buffer will only grow AND it is only
   *        legal to use the same memory buffer as before (e.g., <b>no re-allocation</b>).
   * @param - amortized if set to true, the new size will be computed as
   *          - the requestedMinSize, when requestedminSize is bigger than 2*segments[segment].size
   *          - 2*segments[segment].size else (growth by order of 2)
   *        - when set to false, 'requestedMinSize' is used as given.
   *
   * @return
   *  - true when
   *        - the requestedMinSize is smaller than the current size
   *        - the physical buffer was resized to:
   *          - the requestedMinSize
   *          - if amortized was set to true: to either the requestedMinSize or 2*getSize()
   *          - if there is an implementation underneath this buffer (getValid() == true)
   *            and the implementation class allowed a resize on the given bounds
   *            (in this case the old buffer could be sucessfully expanded)
   *          - the old physical buffer could not be expanded but a new buffer with
   *            the given size requests was allocated and filled with the old contents
   *            by a memory copy
   *  - false when:
   *          - expandOlny was given, but expanding failed and data is present in this buffer
   *            (in this case, a memory copy would have to be done)
   *          - there is no Allocator assigned to the buffer reflected by segment 'segment'
   *          - this buffer is invalid (contains no segments)
   *          - the Allocator assigned to this buffer failed to deliver the amount of memory requested
   *          - memory allocation failed
   *
   * @post after this operation succeeded
   *      - segments[segment].size should be equal or larger than requestedMinSize
   *      - segments[segment].pointer should contain the same bytes than before (possibly with debris in its tail)
   *
   * In case this operation returned 'false', you can consider 'this' to be unchanged.
   *
   * @pre 0 <= segment < getNumberOfSegments()
   */
  bool reserve(size_t requestedMinSize,
		       bool expandOnly = false,
		       bool amortized = false,
		       int segment = 0 );

  /**
   * @brief grow or shrink a \c segment of a buffer, using a possible memcpy
   *
   * Resize the \c segment to the \c newSize.
   * Effectively this is a short hand notation for a call to reserve().
   * Note that for this call, the expandOnly flag of reserve is set to
   * 'false'. In case you want this behavior, use expand()
   *
   * If this call succeeds, getSize(segment) will reflect \c newSize.
   *
   * @see reserve() for explanation of the return codes
   * @see expand()
   *
   * @param newSize the size in bytes to resize segment 'segment' to
   * @param amortized prefer growing by power of 2 with old size
   *        (only when 2*segments[segment].size is larger than newSize)
   *
   * @return
   *   - false when reserve() failed
   *   - true else
   *
   * @pre 0 <= segment < getNumberOfSegments()
   */
  bool resize(size_t newSize, bool amortized = false, int segment = 0);

  /**
   * @brief grow or shrink a \c segment, avoiding memcpy at the risk of failing
   *
   * Expand a \c segment of the buffer to the \c newSize.
   * Effectively this is a short hand notation for a call to \c reserve().
   * This method tries to ensure that a growing of the buffer is archived
   * by changing just the upper bounds of the buffer in memory. This will
   * avoid a memory copy on the data, but may fail if there is already data
   * after the physical block of this buffer in memory.
   *
   * If this call succeeds, getSize(segment) will reflect \c newSize.
   *
   * @see reserve() for an explanation of the return codes
   * @see resize() for reshaping with a possible memory move
   *
   * @return
   *   - false when reserve() failed by expanding only
   *   - true otherwise
   *
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   */
  bool expand(int newSize, bool amortized = false, int segment = 0);

  /// @}

  /**
   * @brief get a continuous (one segment) version of this buffer.
   *
   * In case this buffer is segmented, linearize() will glue all segments in a single segment.
   * This method calls 'memcpy' several times and as such involves costs. A buffer that contains just
   * 1 segment is passed back, no copy costs involved.
   *
   *
   * @return the buffer representing a 'linearized' copy of this buffer.
   *         - the invalid buffer will return *this
   *         - a buffer with 1 segment will return *this
   *         - otherwise a new buffer will be allocated (size: getSize(ALLSEGMENTS))
   *           and all data will be <emph>copied</emph> to this new buffer.
   */
  BufferWrite linearize() const;

  /// @name accessors
  /// @{

  /**
   * @brief get byte-wise access to buffer (non-const)
   *
   * Get the pointer to write to the buffer data as pointer to ubyte.
   * @return
   *  - NULL if the buffer is not valid (i.e. no physical buffer obtained)
   *  - else: a pointer to write to (for segment 'segment')
   *
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   */
  ubyte* writeAccess(int segment = 0) const
  {
	  if( segments.empty() )
		  return NULL;

	  return segments[segment].pointer;
  }

  /**
   * @brief get pointer-to-type access to buffer (non-const)
   *
   * Macro to get a write pointer as type T.
   * @param pos the number of bytes to offset in the data.
   *
   * @return a pointer into the writable field, with an offset in bytes
   *         as determined by pos.
   *         You may <b>not</b> pass ALLSEGMENTS here.
   *
   * @warning Note that this is meant to be in bytes, <b>not</b> in
   *          size of units of T
   *
   * @pre 0 <= segmentIndex < getNumberOfSegments()
   * @pre 0 <= pos < getSize(segment)
   */
  template<class T>
    T* getWrite(size_t pos=0, int segment = 0) const
    {
	  if( segments.empty() )
		  return NULL;

	  return (T*) (segments[segment].pointer+pos);
    }

  /// @}

  ///@name segments
  ///@{
    
    BufferWrite operator+( const BufferWrite &other ) const { return BufferWrite(*this) += other; }
    BufferWrite operator-( const Buffer &other ) const { return BufferWrite(*this) -= other ; }
    BufferWrite &operator+=( const BufferWrite &other ) { return this->Buffer::operator += (other), *this; }
    BufferWrite &operator-=( const Buffer &other ) { return this->Buffer::operator -= (other), *this; }
    
    const BufferWrite operator[](int segment) const { return _const_cast( Buffer::operator []( segment ) ); }
    BufferWrite       operator[](int segment) { return _const_cast( Buffer::operator []( segment ) ); }
    
  ///@}
    
 protected:
    
 private:
    
    static BufferWrite _const_cast( const Buffer & b ) { return BufferWrite( b, '\0' ); }
    BufferWrite( const Buffer & b, char ) : Buffer( b ) {}
    
}; // class BufferWrite


} // namespace flowvr

#endif
