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
* File: ./include/flowvr/chunkwriter.h                            *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_CHUNKWRITER_H
#define FLOWVR_CHUNKWRITER_H

#include "chunk.h"
#include <cstring>
#include <flowvr/moduleapi.h>

namespace ftl
{

/// Helper class to construct messages containing graphical primitives.
class ChunkWriter
{
public:

  /// Constructor of the class ChunkWriter
  ChunkWriter()
  : chunkBufferMaxSize(0),
    chunkBufferSize(0),
    exceptionalBigBuffer(false)
  {
    // nothing else
  }

  /// Chunks ready to send
  std::vector<const Chunk*> chunks;

  /// Discard all the chunks.
  /// note that the memory of all embedded chunk is free'ed
  /// as well as the clearing of the chunks member
  void clear()
  {

    for (unsigned int i=0;i<chunks.size();i++)
    {
      free((void*)(chunks[i]));			/// de-allocating memory for each chunk
						// this function can be moved to the cpp file otherwise free crashes
    }

    chunks.clear();				/// clearing the chunks vector
    chunkBufferSize = 0;				/// reseting the chunk counter
  }

  bool isDirty() const
  {
	  // we are using chunks?
	  if( !chunks.empty() )
		  return true;

	  // chunks are empty, but possibly we stored something in
	  // the buffers of the buffer pool?
	  if(chunkBufferSize > 0 )
		  return true;

	  return false; // no... no chunks, no buffer write...
  }

  void setExceptionalBigBuffer(bool a = true)
  {
    exceptionalBigBuffer = a;
  }

  /// Buffer pool
  BufferPool bufferPool;

  int chunkBufferMaxSize;		/// usually it is going be initialized with 0
  int chunkBufferSize;			/// usually it is going be initialized with 0
  flowvr::BufferWrite chunkBuffer;	/// Buffer for the chunk, it is a typical message
  bool exceptionalBigBuffer;

  /// Add a chunk to send.
  template <class T>
  T* addChunk(int addsize = 0)
  {
    int padsize = padSize(sizeof(T)+addsize);
    T* p;

    /// If the chunk vector has no element and the buffersize and pad is smaller that the max
    if ((chunks.size() == 0) && (chunkBufferSize + padsize <= chunkBufferMaxSize))
    {
      p = chunkBuffer.template getWrite<T>(chunkBufferSize);
      chunkBufferSize += padsize;
    }
    else
    {
      p = (T*) calloc(padsize, 1);	/// alloc memory for the new chunk
      chunks.push_back(p);		/// Inserts a new element at the vector's end.
    }

    p->size = sizeof(T)+addsize-sizeof(Chunk);
    p->type = T::TYPE;
    return p;
  }

  /// Add a chunk to send. The given chunk is copied into the ChunkWriter.
  Chunk * addChunk(const Chunk *c)
  {
    Chunk *p;
    if ((chunks.size() == 0) && (chunkBufferSize + c->totalSize() <= chunkBufferMaxSize))
    {
      p = chunkBuffer.getWrite<Chunk>(chunkBufferSize);
      chunkBufferSize += c->totalSize();
    }
    else
    {
      p = (Chunk *) malloc(c->totalSize());
      chunks.push_back(p);
    }
    memcpy(p, c, c->totalSize());
    return p;
  }

  /// Send the created chunk to a port.
  flowvr::Message put(OutputPort* port, flowvr::StampsWrite stamps = flowvr::StampsWrite());

  /// Dump the created chunk to a message.
  /// at the end of dump, the chunk vector is cleared
  /// this means that the memory is free'd, too!
  template<class T>
  flowvr::MessageWrite dump(T* api)
  {
    flowvr::MessageWrite m; // container

    if (chunks.empty()) // no chunks in chunk vector?
    {
      // no...
      if (chunkBufferSize == 0) // maybe a static size buffer?
        m.data = api->alloc(0); // get a shallow pointer
      else
        m.data = BufferWrite(chunkBuffer, 0, chunkBufferSize); // create a properly sized buffer
    }
    else
    {
      // determine the total size of the message to send
      // start off with the chunkBuffer and its size
      int totalsize = chunkBufferSize; // add chunk buffer size
                                       // unconditional of elements in chunks
      for (unsigned i=0;i<chunks.size();++i)
          totalsize += chunks[i]->totalSize(); // add up size of the single chunks

      m.data = api->alloc(totalsize); // allocate memory large enough
      if( !m.data.valid() )
      {
    	  std::cerr << "could not allocate a buffer of size: "
    	            << totalsize
    	            << std::endl;
      }
      else
      {
		  if (chunkBufferSize) // prefix message with content in chunkBuffer
			memcpy(m.data.template getWrite<char>(0),
				   chunkBuffer.template getRead<char>(0),
				   chunkBufferSize);


          // mark the beginning of the spot to copy the
          // memory composed in the chunks to
		  int writeOffset = chunkBufferSize;
		  for (unsigned i=0;i<chunks.size();++i)
		  {
			  memcpy(m.data.template getWrite<char>(writeOffset),
					 chunks[i],
					 chunks[i]->totalSize()); // copy piece-wise to memory buffer
			  writeOffset += chunks[i]->totalSize(); // note the new offset in the output buffer
		  }
      }
    }

    clear(); // Discard all the chunks. this calls free() on the content!
    return m;
  }

};

} // namespace ftl

#endif

