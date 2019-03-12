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
* File: include/flowvr/sharedmem/sharedmemoryarea.h               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_SHAREDMEM_SHAREDMEMORYAREA_H
#define FLOWVR_SHAREDMEM_SHAREDMEMORYAREA_H

#include "flowvr/ipc/mplock.h"
#include "flowvr/ipc/atomic.h"

namespace flowvr
{

namespace mem
{

/// Shared memory between modules and daemon.
/// This class manage the buffers inside the area.
/// It contains a multi-process safe malloc/free implementation.
///
/// This class is a wrapper allowing to manipulate the area it-self which lives,
///  well, in shared memory. Areas are created/opened by the SharedMemoryManager
///  using the static function \c isFree, \c create and \c open then detroyed
///	 with \c detach and \c release. The created instances can be freely copied
///	 without any armful consequence.
///
/// Note: The position 0 will never be returned as a valid chunk
///
/// Implementation notes:
///
/// The memory area is divided in allocated or free chunks.
///
/// The format of <b>allocated chunks</b> is as follow:
/// - length : size_t
/// - number_of_refering_threads/processes : int
///
/// The format of <b>free chunks</b> is as follows:
/// - length : size_t
/// - prev_free_chunk_relative_pos : int (negative)
/// - next_free_chunk_relative_pos : int (areasize-pos for the last chunk)
///
/// The allocation algorithm is as follows:
/// -# Search the list of free chunk for the fittest one.
/// -# Once the chunk is found,
///   -# if its length is more than the requested length + 1 alloc header + 1 free header:
///     -# Set its length to the requested length+1 alloc header size.
///     -# Create a new free chunk with the remaining part of the chunk.
///     -# Update the adjacent free chunks information of the new free chunk and the adjacent free chunks
///     -# set the number of references of the allocated chunk to 0
///   -# else:
///    -# Update the adjacent free chunk information of the adjacent free chunks
///    -# set the number of references of the allocated chunk to 0
///
/// The freeing algorithm is as follows:
/// -# Search the list of free chunk for the free chunks surrounding the to be freed chunk
/// -# If the previous chunk can be merged
///   - update the length of the previous chunk
/// -# else
///   - set the next free chunk of the previous chunk to this chunk and the previous chunk of this chunk to the previous chunk.
/// -# If the next chunk can be merged,
///   - update the length of the chunk and update the links with the next chunk's next
/// -#  else
///   - set the previous free chunk of the next chunk to this chunk and the next chunk of this chunk to the next chunk
///
/// Multi-Process support:
///
/// There is currently one big lock to be used for all callers from all threads/processes
/// during allocation and deallocation.
///
/// You should not use the very low-level API searchFreeBuffers() and searchFreeBuffersLocked()
/// to allocate memory, instead use allocBuffer() instead. Note that the pos returned by
/// allocBuffer() is not reference-counted. Users have to do that / can do that manually.
/// The scheme for allocation thus is:
/// - claim new (unreferenced buffer) by allocBuffer(), and store in "pos"
/// - call addRef(pos)
/// - obtain a write pointer on the area by a call to getWrite(pos) and store your items
/// - when done with the memory block, call freeRef(pos)
/// - forget pos
///

	class SharedMemoryArea
	{
	 public:

	  /// Constructor
	  SharedMemoryArea(int id, size_t size, ubyte *mapwrite, const ubyte *mapread=NULL);
	  
	  /// check if a given ID is free and can be used to create a new area
	  static bool isFree( int ID );
	  /// those 4 function truly create and destroy the mapping itself.
	  /// They are not thread-safe!
	  static SharedMemoryArea* create( int ID, size_t size, size_t headersize = 0, int verbose = 0 );
	  static SharedMemoryArea* open( int ID, int verbose = 0 );
	  static int detach( SharedMemoryArea* );
	  static int release( SharedMemoryArea* );

	  /// Read this area daemon header information.
	  /// Must be called before using the area if the header was not initialized by this instance
	  /// \return the position of this header.
	  size_t readHeader() const;// readHeader() const;
	  

	  /// Allocate a buffer with the specified size. If pos is specified then it must
	  /// be a value obtained by searchFreeBuffer with the same size and no other
	  /// operation done since.
	  ///
	  /// @param size the number of bytes to allocate as a continuous block
	  /// @param pos the offset from area-base in units of size_t.
	  ///        When pos is 0, a new block is searched. Setting pos to a value
	  ///        unequal to 0 is an advanced operation, so do not do this.
	  /// @return 0 if the call failed (not enough memory or invalid position given by pos).
	  size_t allocBuffer(size_t size, size_t pos=0);

	  /**
	   * Add a reference to a buffer.
	   * Modifies the header-structure given at pos - sizeof(HeaderAlloc).
	   * @param pos the position of the user-data chunk to modify
	   */
	  void addRef(size_t pos);

	  /// Release a reference to a buffer, possibly destroying it.
	  void freeRef(size_t pos);
      
	  /**
	   * Get the numebr of references of a buffer
	   * @return the number of references of the buffer at pos
	   */
	  int getNbRef(size_t pos) const;

	  /**
	   * Test if a ref is unique.
	   * @return true if the refcount is 1, false else
	   */
	  bool uniqueRef(size_t pos) const { return getNbRef( pos ) == 1; }

	  /**
	   *  Check if a buffer is still valid.
	   *  @warning Note that this function can incorrectly return true.
	   *           It just checks whether the nbRef field is greater or equal to 0.
	   *  @param pos the position into the user-chunk data to check
	   */
	  bool isValidRef(size_t pos) const;

	  /**
	   * Return the size of a buffer.
	   * Note that no check is done to ensure the position corresponds to a valid buffer.
	   * @param pos the position into the user data partition of this block
	   */
	  size_t getBufferSize(size_t pos) const;

	  ubyte*      getMappingWrite() const { return mappingWrite; }
	  const ubyte* getMappingRead() const { return mappingRead; }

	  /**
	   * Convenience method to have typed access on the internals.
	   * get a write pointer into the write area at the given offset in <b>bytes</b>
	   * @warning remember that the offset is in bytes and not in units of sizeof(T)
	   */
	  template<class T>
	  T* getWrite(size_t pos)
	  {
		return (T*)(mappingWrite+pos);
	  }

	  template<class T>
	  const T* getRead(size_t pos) const
	  {
		return (const T*)(mappingRead+pos);
	  }

	  int getAreaId() const
	  {
		return areaId;
	  }

	  void dumpDebugInfo() const;

	  size_t getSize()     const { return areaSize; }
	  size_t getFreeSize() const { return getRead<AreaHeader>(0)->freeSize; }


	  /// Access the list of attached process ids

	  /**
	   * @return the number of attached processes to this shared memory arena
	   */
	  int attachSize() const;

	  /**
	   * claim the PID of the process that attached at the index given
	   * @param index the 'attached' index
	   * @return
	   *         - the PID of the process that was attached after 'index' attaches
	   *         - 0 if the index is invalid (out of bounds or < 0)
	   */
	  int getAttachPID(int index) const;


    protected:
	  
	  /// Initialize the newly created area.
	  /// Return the position of this area's header with the requested size.
	  size_t initialize(size_t headersize);
	  
	  /// Search for an empty area. Return its position or 0 if none found.
	  size_t searchFreeBuffer(size_t size, size_t *bestFit);
	  
	  /// Declare a new attached process. Return the position in attach list
	  int attachPID(int pid);
	  /// Remove a previously attached process, given the pid
	  bool detachPID(int pid);
	  
	  
	private:

	  /// Search for an empty area. Return its position or 0 if none found.
	  /// Assume the caller already own the lock.
	  size_t searchFreeBufferLocked(size_t size, size_t *bestFit) ;


	  int       areaId;                ///< Id of the area
	  size_t areaSize;        ///< Total size
	  ubyte*       mappingWrite; ///< writable mapping of the area
	  const ubyte* mappingRead;  ///< readable mapping of the area

	  ipc::MTAtomicInt m_areaRef;
	  
	  int m_attachPos;
	  
	  
    public:
	  
	  struct AreaHeader
	  {
		int ID;
		uword versionMaj;
		uword versionMin;

		size_t headerPos;
		size_t firstFreePos;
		size_t freeSize;

		ipc::MPLock allocLock;
		ipc::MPAtomicInt attached[0]; /**< start of block of attached PIDs, grows during execution! */
	  };

	  struct HeaderAlloc
	  {
		size_t length;
		flowvr::ipc::MPAtomicIntBw nbref;
	  };

	  struct HeaderFree
	  {
		size_t length;
		ipc::MPAtomicIntBw prevDP;
		ipc::MPAtomicIntBw nextDP;
	  };

	  enum { MinAllocSize = 8 };
	  enum { Alignment    = 8 };

	  inline static size_t alignUp  (size_t i) { return (i+Alignment-1)&~(Alignment-1); }
	  inline static size_t alignDown(size_t i) { return (i)&~(Alignment-1); }

	};


} // namespace mem

} //namespace flowvr

#endif
