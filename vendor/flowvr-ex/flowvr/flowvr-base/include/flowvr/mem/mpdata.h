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
* File: include/flowvr/mpdata.h                                   *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_MPDATA_H
#define FLOWVR_MPDATA_H



#include <string>

namespace flowvr
{

	class Buffer;
	class BufferWrite;

	namespace mem
	{
		/**
		 * @brief memory layout structure for multi-process / shared memory data exchange
		 *
		 * An MPBuffer is never created as given here. It is just meant to be a memory layout
		 * of memory blobs in the shared memory space between the modules and the daemon.
		 * At the same time, the methods provided with this structure ensure a proper
		 * management of the reference count directly on the shared memory block.
		 *
		 * Its structure mimics the ShareMemoryBuffer to some extent. All attributes should
		 * be considered read-only. Writing the values to a proper state is ensured by the
		 * API provided with the MPBuffer (e.g., init(), clear() and so on...)
		 *
		 * It is a central data structure for communication. Conversion is done on assignment
		 * from a Buffer and back by casting to Buffer or BufferWrite.
		 */
		class MPBuffer
		{
		public:
		  int    shmID;     /**< the shared memory ID to attach to (determined by buffer upon assign) */
		  size_t offset;    /**< offset in the shared memory segment */
		  size_t suboffset; /**< sub-offset in the shared memory segment */
		  size_t subsize;   /**< sub-size in the shared memory segment */

		  size_t segments,
		         num_segments;

		  /**
		   * @brief set all members to 0
		   *
		   * Use this method instead of a constructor call.
		   * Use only once. If the MPBuffer was already attached to a shared memory segment,
		   * use clear() to detach it and re-use the buffer.
		   */
		  void init();

		  /**
		   * @brief detach from an attached shared memory segment (area)
		   *
		   * Detached from a shm area, if it was attached to one, decreases the
		   * ref-count on the area. After this call, valid() will return false.
		   * The only way to attach it again is by a call to a conversion operator
		   * given a Buffer / BufferWrite (it will be attached to the shm-area
		   * that is used by the Buffer / BufferWrite.
		   */
		  void clear();

		  /**
		   * @brief test whether this MPBuffer is attached to a sharem memory segment
		   *
		   * @return true if this MPBuffer is representing data in a shm area, false else
		   */
		  bool valid() const { return shmID>0; }

		  /**
		   * @brief test whether the MPBuffer is not valid or represents a block of size 0
		   *
		   * Note: Invalid buffers are considered empty.
		   *
		   * @return
		   *        - true either valid() is false or subsize is 0
		   *        - false else
		   */
		  bool empty() const { return !valid() || num_segments==0; }

		  /**
		   * @brief assignment from a BufferObject
		   *
		   * It is assumed that the implementation used by the buffer object
		   * is a SharedMemoryBuffer.
		   */
		  void operator=(const Buffer& buf);

		  /**
		   * @brief cast operator from MPBuffer to Buffer
		   *
		   * Converts this MPBuffer back to a normal Buffer.
		   * This operator works on the memory-block-head (by creating a SharedMemoryBuffer).
		   */
		  operator Buffer() const;

		  /**
		   * @brief cast operator from MPBuffer to BufferWrite
		   *
		   * Converts this MPBuffer back to a normal BufferWrite.
		   * This operator works on the memory-block head (by creating a SharedMemoryBuffer).
		   */
		  operator BufferWrite() const;
		private:
		  MPBuffer()  {} ///< An MP data structure can't be constructed in ordinary memory
		  MPBuffer( const MPBuffer & ) {} ///< An MP data structure can not be copied

		  ~MPBuffer() {} ///< An MP data structure can't be explicitly deleted
		};

		/**
		 * @brief Multi-Process String utility class
		 *
		 * This structure provides some utility functions to copy strings to an MPBuffer and
		 * read a string back from one.
		 */
		struct MPString : public MPBuffer
		{

		  /**
		   * @brief copy a string from str to this buffer given its length
		   *
		   * @param str the start of the memory block to copy from.
		   *        str can be NULL which will clear this MPString
		   * @param length the number of bytes to copy. can be ~0,
		   *        indicating that the length is to be determined
		   *        by strlen(str).
		   */
		  void copy(const char* str, size_t length=~0);

		  /**
		   * @brief copy from begin to end
		   *
		   * It is assumed that end > begin.
		   * Reduces to copy(begin, length)
		   */
		  void copy(const char* begin, const char* end)
		  {
			copy(begin,end-begin);
		  }


		  /**
		   * @brief assignment operator from const char*
		   *
		   * Reduced to copy(str)
		   *
		   * @param str the string to copy to this MPString (can be NULL)
		   */
		  void operator=(const char* str)
		  {
			copy(str);
		  }

		  /**
		   * @brief assignment operator from std::string
		   *
		   * @param str the string to copy to this MPString
		   */

		  void operator=(const std::string& str)
		  {
			copy(str.c_str(),str.size());
		  }

		  /**
		   * @brief assignment operator from Buffer
		   *
		   * Needed to assure that we call MPBuffer::operator=() here,
		   * otherwise an assignment from string will end up in an endless loop,
		   * as Buffer defines a cast operator to std::string.
		   */
		  void operator=(const Buffer& buf)
		  {
			MPBuffer::operator=(buf);
		  }


		  /**
		   * @brief conversion operator to std::string of this MPString
		   *
		   * Copies the content of this MPString to a std::string object in return.
		   * @returns a std::string with the contents of this MPString
		   *          (can be empty for invalid or empty MPString or if the SharedMemory
		   *          was not initialized)
		   */
		  operator std::string() const;
		};

	} // namespace mem

} // namespace flowvr

#endif
