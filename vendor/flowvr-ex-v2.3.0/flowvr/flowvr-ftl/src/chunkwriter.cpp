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
* File: ./src/chunkwriter.cpp                           *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <ftl/chunkwriter.h>

#include <string>
#include <algorithm>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>


namespace ftl
{

/// Send chunk messages to a specific destination port
/// this routine prepares the chunks in a standard message to be transmitted
flowvr::Message ChunkWriter::put(OutputPort* port, flowvr::StampsWrite stamps)
{
	///@todo check why this is still here? it is not used?
  if (chunkBufferSize != 0)
  {
    ChunkIterator it = chunkBegin(Buffer(chunkBuffer, 0, chunkBufferSize));
    ChunkIterator end = chunkEnd(Buffer(chunkBuffer, 0, chunkBufferSize));
  }

  flowvr::MessageWrite m = dump(port->getModule());
  m.stamps = stamps;
  // This is probably an old resource
  //m.stamps.write(port->stampScratch,scratch?1:0);

  ///sending the message as a normal port, port is the communication port and m is the message to be transmited
  port->getModule()->put(port,m);

  return m;
}

} // namespace ftl
