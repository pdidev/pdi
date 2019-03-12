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
* File: include/flowvr/mem/memorymanager.h                        *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_MEM_MEMORYMANAGER_H
#define FLOWVR_MEM_MEMORYMANAGER_H


#include "flowvr/allocator.h"
#include "flowvr/buffer.h"

namespace flowvr
{

namespace mem
{

class MemoryManager : public Allocator
{
public:
  
  /// Alloc method.
  BufferWrite alloc(size_t size);
  
  /// Realloc method.
  bool realloc(BufferWrite &buffer, size_t size, bool amortized=false)
  {
    if (buffer.valid()) 
      return buffer.resize(size, amortized);
    else {
      buffer = alloc(size);
      return buffer.valid();
    } 
  }

  static MemoryManager* instance();
};

} // namespace mem

} // namespace flowvr

#endif // FLOWVR_MEM_MEMORYMANAGER_H
