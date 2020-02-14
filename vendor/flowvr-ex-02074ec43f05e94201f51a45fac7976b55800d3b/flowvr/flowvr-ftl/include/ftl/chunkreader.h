/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Clement Menier.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: ./include/flowvr/chunkreader.h                            *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_CHUNKREADER_H
#define FLOWVR_CHUNKREADER_H

#include "chunk.h"

#include <flowvr/message.h>

namespace ftl
{

/// Note: A class deriving from ChunkReader must use a UserData class
class ChunkReaderDefaultUserData
{
public:
};

class ChunkReaderDefaultBase
{
public:
};


//template <class UserData = ChunkReaderDefaultUserData, class Base = ChunkReaderDefaultBase >
class ChunkReader// : public Base
{
public:

  int iteration;

  ChunkReader() : iteration(-1)
  {
  }

  virtual ~ChunkReader() {}

  /// global init
  /// @return false if error
  virtual bool init();

  virtual bool process(flowvr::Message msg, int it=-1);

protected:

  virtual bool processChunk(const MsgChunk<Chunk>& data);

};



} // namespace ftl

#endif
