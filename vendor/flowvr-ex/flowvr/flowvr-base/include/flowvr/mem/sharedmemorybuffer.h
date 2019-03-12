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
* File: include/flowvr/sharedmem/sharedmemorybuffer.h             *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_SHAREDMEM_SHAREDMEMORYBUFFER_H
#define FLOWVR_SHAREDMEM_SHAREDMEMORYBUFFER_H

#include "flowvr/buffer.h"
#include "flowvr/ipc/atomic.h"

namespace flowvr
{



namespace mem
{
	class SharedMemoryArea;

/// Buffer Implementation on Shared Memory Areas
class SharedMemoryBuffer : public BufferImp
{
public:
  static SharedMemoryBuffer* create(SharedMemoryArea* area, size_t offset, size_t size);


  SharedMemoryBuffer(SharedMemoryArea* area, size_t offset, size_t size);
  virtual ~SharedMemoryBuffer();

  virtual size_t  open();

  virtual void claim();
  virtual void release();
  virtual void close();

  /**
   * @brief enhanced ownership test
   *
   * @return
   *    - true: only one owner (this) and we have an shm-area pointer and the
   *            number of references on that shared memory segment is 1 as well.
   *    - false: else
   */
  virtual bool uniqueOwner() const;

  /** give read pointer to shared memory buffer */
  virtual const ubyte* readAccess() const;

  /** give write pointer to shared memory buffer */
  virtual ubyte* writeAccess() const;

  /** try to resize a shared memory buffer */
  virtual bool resize(size_t minSize, size_t preferredSize, bool expandOnly);


  virtual size_t getSize() const;
  virtual int    getCount() const;

  virtual Allocator* getAllocator() const;

  SharedMemoryArea *getArea() const;
  size_t getOffset() const;

  static SharedMemoryBuffer emptyBuffer; // 0 sized buffer
protected:

  /// Empty buffer constructor
  SharedMemoryBuffer();
  SharedMemoryArea* area;
  size_t offset;
  size_t size;
  ipc::MTAtomicInt nbRefLocal;
}; // class SharedMemoryBuffer

} // namespace mem

} // namespace flowvr
#endif
