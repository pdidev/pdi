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
* File: ./include/flowvr/chunk.h                                  *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_CHUNK_H
#define FLOWVR_CHUNK_H

#include <flowvr/buffer.h>
#include <flowvr/message.h>

namespace ftl
{
using namespace flowvr;

/// This assure that the messages have the short size as possible and no alignment
#if defined _MSC_VER
# pragma pack(push)
# pragma pack (1)
#else
# pragma pack(push,1)
#endif

/// Return the pad size
extern __inline__ int padSize(int size, int padding=4)
{
  return (size+3)&-padding;  /// binary and that retrieves the last digits (standard= 2 last)
}

/// Just the base struct to hold size and type
struct Chunk
{
  // HEADER
  int size; 			/// Data size (not including header and padding).
  unsigned short int type; 	/// See Type enum.

  /// Data is stored in the subclasses
  enum Type
  {
    INVALID    = 0x00,		/// Invalid, mean something wrong happened
    EVENT      = 0x10,		/// Used for IO events, like keyboard, mouse, etc
  };


  /// Returns the total chunk size
  int totalSize() const
  {
    return padSize(sizeof(Chunk)+size);  /// header plus data
  }

  /// Print chunk info to stdout
  void print() const;
};

#pragma pack(pop)

/// Hold a buffer containing one data chunk
/// MsgChunk is based in the Buffer class
template <class ChunkType=Chunk>
class MsgChunk : public Buffer
{
protected:

  /// Returns the MsgChunk size
  static int calcSize(const Buffer& buf, int offset=0, int size=-1)
  {
    if (size==-1 || buf.getSize() < offset+size)
    	size=buf.getSize()-offset;
    if (size>=(int)sizeof(Chunk))
    {
      int s2 = buf.getRead<Chunk>(offset)->totalSize();
      if (s2 < size)
    	  size=s2;
    }

    /// getRead and getSize are member functions that return the data position and size respectively

    if (size<=0)
    	return 0;  /// means there is nothing
    else
    	return size;
  }

public:
  MsgChunk() {}   /// empty constructor
  MsgChunk(const Buffer& buf, int offset=0, int size=-1)  /// complete constructor
  : Buffer(buf,offset,calcSize(buf,offset,size))
  {
  }

  bool isTruncated()
  {
	  size_t size = getSize();
    if (size<sizeof(Chunk))
    	return true;
    return (size < getRead<ChunkType>(0)->size + sizeof(Chunk));
  }

  const ChunkType* operator*() const
  {
    return getRead<ChunkType>(0);
  }

  const ChunkType* operator->() const
  {
    return getRead<ChunkType>(0);
  }

  operator const ChunkType*() const
  {
    return getRead<ChunkType>(0);
  }

  /// Returns the MsgChunk data as a Buffer
  Buffer data() const
  {
    return Buffer(this,
                  ((const char*)getRead<ChunkType>(0)->data()) - getRead<char>(0),
                  getRead<ChunkType>(0)->dataSize());
  }

  /// Just print chunk information
  void print() const
  {
    getRead<Chunk>(0)->print();
  }

};

/// Iterate thru all chunks in a buffer
/// One buffer can hold several chunks
class ChunkIterator
{
protected:

  Buffer buf;
  int offset;

public:

  /// Constructor
  ChunkIterator(const Buffer& b, int base=0)
  : buf(b), offset(base)
  {
    if (buf.getSize() < offset+(int)sizeof(Chunk))
      offset = -1;
  }

  /// Operator
  const Chunk& operator*() const
  {
    static Chunk invalidChunk={0,Chunk::INVALID};
    if (offset==-1)
      return invalidChunk;
    else
      return *buf.getRead<Chunk>(offset);
  }

  /// Operator
  const Chunk* operator->() const
  {
    if (offset==-1)
      return NULL;
    else
      return buf.getRead<Chunk>(offset);
  }

  /// Operator
  operator const Chunk*() const
  {
    return operator->();
  }

  /// Operator
  bool operator==(const ChunkIterator& it) const
  {
    return buf == it.buf && offset == it.offset;
  }

  /// Operator
  bool operator!=(const ChunkIterator& it) const
  {
    return !operator==(it);
  }


  /// Operator
  operator bool() const
  {
    return offset!=-1;
  }

  /// Operator
  bool operator!() const
  {
    return offset==-1;
  }


  /// Operator
  void operator++()
  {
    const Chunk* c = operator->();
    if (c == NULL) return;
    offset += c->totalSize();
    if (offset+(int)sizeof(Chunk) > buf.getSize())
      offset = -1;
  }

  /// Operator
  void operator++(int)
  {
    operator++();
  }

  /// Operator
  template <class ChunkType>
  operator MsgChunk<ChunkType>() const
  {
    return MsgChunk<ChunkType>(buf,offset);
  }

};

extern __inline__ ChunkIterator chunkBegin(const Buffer& buf)
{
  return ChunkIterator(buf);
}

extern __inline__ ChunkIterator chunkEnd(const Buffer& buf)
{
  return ChunkIterator(buf,-1);
}

extern __inline__ ChunkIterator chunkBegin(const Message& buf)
{
  return ChunkIterator(buf.data);
}

extern __inline__ ChunkIterator chunkEnd(const Message& buf)
{
  return ChunkIterator(buf.data,-1);
}

} // namespace ftl

#endif
