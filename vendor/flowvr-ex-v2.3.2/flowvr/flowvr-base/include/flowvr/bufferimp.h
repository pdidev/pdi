/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                                                                 *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
*                                                                 *
*  Contact :                                                      *
*                                                                 *
******************************************************************/

#ifndef BUFFERIMP_H_
#define BUFFERIMP_H_

#include <sys/types.h>
#include <flowvr/common.h>

namespace flowvr
{
	class Allocator;


	/**
	 * @brief abstract interface for management classes on physical memory blocks
	 *
	 * Interface as a base for implementation classes delivering real
	 * buffer management and allocation services.
	 */
	class BufferImp
	{
	 public:

	  /// @name construction / de-construction
	  /// @{
	  /**
	   * @brief Empty destructor.
	   *
	   * This class is meant to be sub-classed, so we provide the virtual destructor here.
	   */
	  virtual ~BufferImp();

	  /// @}


	  /// @name introspection
	  /// @{
	  /**
	   * @return the current reference count for this buffer imp.
	   *         Note that this count should always be >= 1, as this
	   *         object holds a count of 1 to the buffer.
	   */
	  virtual int getCount() const = 0;


	  /**
	   * Tests, if this buffer was only opened() and not claimed().
	   * Typically this means that the BufferImp is the only interested party
	   * in the physical buffer ('sole owner property').
	   * @true iff this BufferImp is the only owner of the physical buffer,
	   *       false else
	   */
	  virtual bool uniqueOwner() const;


	  /**
	   * @brief get the size of this buffer
	   *
	   * @returns the size of this buffer in bytes
	   */
	  virtual size_t getSize() const = 0;

	  /**
	   * @brief get allocator used for this physical buffer
	   *
	   * @return the allocator used or NULL (for invalid buffers)
	   */
	  virtual Allocator* getAllocator() const = 0;

	  /// @}


	  /// @name size- and state-management
	  /// @{

	  /**
	   * @brief resize the  buffer to at least minSize with preferredSize.
	   *
	   * @param  minSize  minimum size to guarantee by the implementation to allocate)
	   * @param  preferredSize desired size  (allocate if possible, should be >= minSize)
	   * @param  expandOnly user indicates that memcpy or memory moves must be avoided,
	   *         so the implementation tries to avoid that by re-sizing the buffer (tail move).
	   *         Note that a resize may fail if this effort fails returning 'false' in that case.
	   *
	   * @return
	   *     - true: getSize() >= minSize
	   *     - false: else (no memory available / expandOnly failed...)
	   *
	   * @note The implementation can choose  to resize the buffer to be of a bigger size
	   *       than preferredSize stated.
	   */
	  virtual bool resize(size_t minSize, size_t preferredSize, bool expandOnly) = 0;


	  /**
	   * Memory can be 'opened', making the allocation effective.
	   * Note that this is an implementation detail, as implementations
	   * can chose to allocate resources already during construction or
	   * re-sizing. In case you work on a BufferImp, and you want to
	   * defer a possible resource-allocation step, it is safe to call
	   * open() then.
	   * The implementation surely will at least increase a reference
	   * counter by one upon a call to open().
	   * This method should only be called by the BufferImp that creates
	   * the buffer. Other users of the real buffer should call claim() instead.
	   *
	   * @see claim()
	   *
	   * @return the size of the buffer,
	   *         determined by the underlying implementation
	   */
	  virtual size_t open() = 0;


	  /**
	   * Claim the buffer to be owned by an instance.
	   * Callers of this method did not open the buffer, but need to be aware
	   * of the existence of this buffer until released.
	   * Implementations will most likely implement a reference-counting scheme here.
	   * @see release()
	   */
	  virtual void claim() = 0;

	  /**
	   * @brief release a buffer, possibly closing it (releasing resources)
	   *
	   * This call is the inverse call to claim(). Any instance claiming a buffer should
	   * use the same amount of calls to release.
	   *
	   * @see claim()
	   * @see open()
	   * @see close()
	   */
	  virtual void release() = 0;

	  /**
	   * Callers of this method indicate that they have
	   * no longer an interest in this buffer. Note that the physical
	   * buffer is typically deleted / freed, when the number of calls to
	   * close() equals 1 + n*claim() (one for open and n-times claim).
	   *
	   * Implementations typically chose to implement their own reference-
	   * counting scheme, and will decrease the reference-count by one upon a call
	   * to close.
	   *
	   * @see claim()
	   * @see open()
	   */
	  virtual void close() = 0;

	  /// @}


	  /// @name accessors
	  /// @{

	  /**
	   * @brief const byte-wise access to the physical memory
	   *
	   * @return a const-pointer to the first byte to be written to
	   *         in the physical memory block, can be NULL for unitialized
	   *         buffers
	   */
	  virtual const ubyte* readAccess() const = 0;

	  /**
	   * @brief non-const byte-wise access to the physical memory
	   *
	   * @return a pointer to the first byte to be written to in the
	   *         physical memory block, can be NULL for uninitialized buffers
	   */
	  virtual ubyte* writeAccess() const = 0;

	  /// @}
	};

}



#endif // BUFFERIMP_H_
