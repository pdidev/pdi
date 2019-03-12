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
* File: include/flowvr/mem/memorybuffer.h                         *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_MEM_MEMORYBUFFER_H
#define FLOWVR_MEM_MEMORYBUFFER_H

#include "flowvr/buffer.h"
#include "flowvr/ipc/atomic.h"

namespace flowvr
{

namespace mem
{

/// Buffer Implementation on Standard Memory (stdlib).
/// allocates a dedicated amount of memory in bytes.
class MemoryBuffer : public BufferImp
{
public:
  MemoryBuffer(size_t size, int nref = 0);
  virtual ~MemoryBuffer();
  virtual size_t open();
  virtual void claim();
  virtual void release();
  virtual void close();

  /**
   * @return the number of references due to calls to open()
             or claim() or close()
   */
  virtual int getCount() const;
  virtual const ubyte* readAccess() const;
  virtual ubyte* writeAccess() const;

  /**
   * resizing of the buffer is done when
   * - the new size (minSize) is larger than the current size (getSize()).
       The reason for this is to avoid costly resizing operations.
   * - a buffer that is to expand only will refuse to shrink. The physical size
   *   of the buffer will remain unchanged then.
   * - a buffer needs to be uniqueOwner() (opened just by one instance!)
       for a resize to work. Otherwise, other existing buffer instances might
       be confused.
   *
   * The algorithm:
   *  -# it is first tried to allocate the preferredSize in bytes,
   *  -# when this fails, the minSize in bytes is tried.
   *  -# If that fails to, the routine returns false.
   *
   * Note that a buffer size is sufficient, if its current size it at least
   * the minSize in bytes.
   *
   * @return true when the size changed or was large enough to
   *         fit the new requirements, false else
   * @param minSize the minimum number of bytes to allocate (must be smaller
   *        than preferredSize)
   * @param preferredSize the number of bytes to use for a 'first-try' allocation.
   *        If that failes, the realloc is tried again with minSize
   * @param expandOnly flag to indicate whether the buffer should only grow
   *        (i.e. it will not change the buffer size when the newly requested
   *        size is smaller than the old one)
   *
   * @see uniqueOwner()
   * @see open()
   */
  virtual bool   resize(size_t minSize, size_t preferredSize, bool expandOnly);
  virtual size_t getSize() const;

  virtual Allocator* getAllocator() const;

  static MemoryBuffer emptyBuffer;

protected:
  size_t size;
  void *data;
  ipc::MTAtomicInt nbRef;
};

} // namespace mem

} // namespace flowvr

#endif // FLOWVR_MEM_MEMORYBUFFER_H
