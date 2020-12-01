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
* File: ./src/chunkreader.cpp                           *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <ftl/chunkreader.h>

namespace ftl
{

#define ERRMSG(msg) (std::cerr << __FILE__ << ":" << __LINE__ << "(" << __PRETTY_FUNCTION__ <<"): " << msg << std::endl, false)

//template <class UserData, class Base>
//bool ChunkReader<UserData,Base>::init()
bool ChunkReader::init()
{
  return true;
}

//template <class UserData, class Base>
//bool ChunkReader<UserData,Base>::process(flowvr::Message msg, int iter)
bool ChunkReader::process(flowvr::Message msg, int iter)
{
  if (iter<0) ++iteration;
  else iteration = iter;
  ChunkIterator it = chunkBegin(msg);
  ChunkIterator end = chunkEnd(msg);
  bool res = true;
  while (it != end)
  {
    if (!processChunk((MsgChunk<Chunk>)it))
      res = false;
    it++;
  }
  return res;
}

//template <class UserData, class Base>
//bool ChunkReader<UserData,Base>::processChunk(const MsgChunk<Chunk>& data)
bool ChunkReader::processChunk(const MsgChunk<Chunk>& data)
{

  if (data.getSize()==0) return ERRMSG("Empty chunk");

  switch (data->type)
  {

  case Chunk::INVALID:
  {
    return ERRMSG("Unknown chunk type "<<data->type);

  }

  default:
    MsgChunk<Chunk> c(data);  // discover what shoul be here
    return processChunk(c);
  }

}

#undef ERRMSG

} // namespace ftl
