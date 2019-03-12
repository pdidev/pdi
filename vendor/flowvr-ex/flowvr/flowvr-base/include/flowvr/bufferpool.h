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
* File: include/flowvr/bufferpool.h                               *
*                                                                 *
* Contacts:                                                       *
*  01/21/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_BUFFERPOOL_H
#define FLOWVR_BUFFERPOOL_H

#include "flowvr/allocator.h"
#include "flowvr/buffer.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/ipc/locker.h"
#include <list>

namespace flowvr
{

	/**
	 * @brief memory management strategy for re-occurring memory allocations / deallocations
	 *
	 * A buffer pool reduces overhead  for repeated allocation of fixed-size buffers by
	 * keeping  a  reference  to  old  buffers  and  reusing  any  unique
	 * reference.
	 * The idea is that most modules sent constant size memory buffers each iteration.
	 * Those modules can benefit by using a buffer pool as the pool tries to keep
	 * up references of buffers as long as possible and simply returns an already allocated
	 * but non-used buffer with a matching size.
	 */
	class BufferPool
	{
	 public:

	  /// Constructor
	  BufferPool(int maxbuffers=10);

	  /// Allocate a new buffer or reuse a free one.
	  /// If strict is true and the number of buffer reached maxBuffer no new allocation will occur.
	  BufferWrite alloc(Allocator* allocator, size_t size, bool strict=false);

	  /// Returns the current buffer size
	  size_t getCurrentBufferSize() const
	  {
		return currentBufferSize;
	  }

	  /// Return the number of buffers currently in the pool

	  size_t getPoolSize() const
	  {
		ipc::ScopedMTLock locker(globalLock,"getPoolSize");
		return buffers.size()+freeBuffers.size();
	  }

	  /// Returns the current max number of buffers
	  int getMaxBuffer() const
	  {
		return maxBuffers;
	  }

	  /// Change the max number of buffers
	  void setMaxBuffer(int maxb)
	  {
		maxBuffers = maxb;
	  }

	  int getNbRequests() const { return nbrequests; }
	  int getNbAllocs() const { return nballocs; }

	 private:

	  int nbrequests; ///< Number of buffers requested (for stats)
	  int nballocs;   ///< Number of buffers allocated (for stats)


	  mutable ipc::MTLock globalLock;
	  size_t currentBufferSize; ///< Size of buffers currently in the pool
	  typedef std::list<BufferWrite> BufferList;
	  BufferList buffers; ///< Pool of allocated (but maybe still in use) buffers
	  BufferList freeBuffers; ///< Pool of free buffers
	  size_t     maxBuffers; ///< max size of buffer pool

	}; // class BufferPool

	/**
	 * @brief Allocator for buffer pools
	 *
	 * The BufferPoolAllocator uses a buffer pool to suffice memory requests.
	 */
	class BufferPoolAllocator : public Allocator {
	public:
	  /**
	   * Constructor.
	   *
	   * @param allocator the real memory allocator to use for the pool
	   * @param maxbuffers the maximum number of buffers to re-use
	   *        (is passed to BufferPool)
	   */
	  BufferPoolAllocator(flowvr::Allocator *allocator, int maxbuffers=10);


	  virtual BufferWrite alloc(size_t size);
	  virtual bool realloc(BufferWrite &b, size_t size, bool amortized=false);

	private:
	  flowvr::BufferPool bufferPool; ///< BufferPool used for allocations
	  flowvr::Allocator *allocator;  ///< Allocator
	};

} // namespace flowvr

#endif
