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

#ifndef ALLOCATOR_H_
#define ALLOCATOR_H_

#include <sys/types.h>

namespace flowvr
{
	class BufferWrite;

	/**
	 * @brief Abstract interface for memory acquisition.
	 *
	 * Base interface for memory management.
	 * Allocators are used to <b>allocate</b> memory, or to re-size existing
	 * buffers to a new size. Memory, in general, that was allocated by an
	 * allocator if freed by the underlying reference-counting mechanism.
	 */
	class Allocator
	{
	 public:
		/**
		 * empty destructor
		 */
	  virtual ~Allocator() {}

	  virtual int attach();
	  virtual bool detach();

	  /**
	   * Alloc a new writeable buffer. This method always succeeds.
	   * If the size given could not be allocated, an invalid BufferWrite
	   * is returned.
	   * @param size the number of bytes to allocate.
	   * @return an instance of a buffer write. To test whether the allocation
	   *         actually worked, use the BufferWrite::valid() API or test
	   *         if size == BufferWrite::getSize()
	   *
	   */
	  virtual BufferWrite alloc(size_t size) = 0;

	  /**
	   * Realloc a buffer given its BufferWrite representation to a new size.
	   * The size can be bigger or smaller than the original size of buffer.
	   * The buffer given can be invalid. If so, the implementation can decide
	   * to either fail or to allocate a new buffer for the given size.
	   * @param buffer the buffer handle to resize / realloc
	   * @param size the new size to attach to the buffer (see BufferWrite
	   *        for more information on the range of size)
	   * @param amortized see BufferWrite::resize() for more information
	   */
	  virtual bool realloc(BufferWrite &buffer,
			               size_t size,
			               bool amortized=false) = 0;

	  /**
	   * get the default allocator.
	   * @todo is the default-allocator interface still needed?
	   * @return the default Allocator set up by the user, can be NULL
	   */
	  static Allocator *getDefaultAllocator();

	  /**
	   * get the user-defined Allocator
	   * @return the allocator that was set by setAllocator(), can be NULL
	   */
	  static Allocator *getAllocator();

	  /**
	   * This is just an alias for getAllocator()
	   * @see getAllocator()
	   */
	  static Allocator *the();



	  /**
	   * sets the user-defined allocator to return by getAllocator().
	   * This method can be used to set the default Allocator().
	   *  - if overwrite default is set to true OR the default allocator
	   *     was not set before. (Effectively, the first call to setAllocator()
	   *     sets the default. This can be prohibited by passing false for
	   *     bOverwriteDefault on the 'first' call. In that case getDefault()
	   *     will still return NULL (i.e.: there is no default)
	   *  - should be used to set the system wide allocator to NULL (and the default)
	   *    when the Allocator was deleted
	   *
	   * @param alloc the pointer to set that is returned by the() and getAllocator()
	   * @param bOverwriteDefault sets alloc as the allocator to
	   *        return on call to getDefault()
	   *
	   * @see the()
	   * @see getAllocator()
	   */
	  static void setAllocator( Allocator *alloc, bool bOverwriteDefault = true );
	};
} // namespace flowvr


#endif // ALLOCATOR_H_
