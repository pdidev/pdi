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
* File: include/flowvr/sharedmem/sharedmemorymanager.h            *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_SHAREDMEM_SHAREDMEMORYMANAGER_H
#define FLOWVR_SHAREDMEM_SHAREDMEMORYMANAGER_H

#include "flowvr/common.h"
#include "flowvr/allocator.h"

#include "flowvr/mem/MPkMTvSparseVector.h"

#include <string>
#include <vector>


namespace flowvr
{

class BufferWrite;
class BufferImp;

namespace mem
{

class MPBuffer;
class SharedMemoryArea;

/**
 * @brief The SharedMemoryManager class provide an allocator supplying buffers
 *      of shared memory.
 *
 * This class is inherits of the singleton class 'Allocator'. After calling the
 *  constructor, the 'attach' method must be called to make the instance visible
 *  to the whole process. The initialization of the instance is performed during
 *  this call.
 */

class SharedMemoryManager : public Allocator
{
public:
    
  ////////////////////////////////////////////////////////////////////////////
  //    Public methods
  ////////////////////////////////////////////////////////////////////////////

  /// constructor
  SharedMemoryManager( unsigned int mainMemId = flowvr::DefaultMemId );
  /// destructor
  virtual ~SharedMemoryManager();

  /// constructor
  static SharedMemoryManager* instance();

  /// re-implementation of this virtual method. Attaching the allocator will
  /// perform a call to the virtual 'init' method at the same time.
  virtual int attach();

  /// @brief init initialize the instance, it's MP interface and get the shared
  ///   list of shared memory area from the daemon.
  /// @return \c -1 if it fails, \c 0 otherwise
  virtual int init();
  
  
  /// inherited virtual allocation method.
  virtual BufferWrite alloc( size_t size );// using any shared memory area, eventually allocating a new area in the process
  /// inherited virtual (re)allocation method.
  virtual bool realloc( BufferWrite &buffer, size_t size, bool amortized=false );

  /// Allocate a \c BufferWrite
  BufferWrite allocBuffer( size_t size );// using any shared memory area, eventually allocating a new area in the process
  /// Allocate a \c BufferWrite copy of a string
  BufferWrite allocString( const std::string& str );
  
  
  /// Open a shared memory area previously created
  SharedMemoryArea* openMemoryArea( int ID, int verbose=0 );

  /// @brief return the main shared memory area (must have been opened previously).
  /// \return the main shared memory area (must have been opened previously).
  inline SharedMemoryArea* getMainMemoryArea() const { return *m_shmAreas->getMapped( m_nMainMemId ); }  
  
  /// @brief return the shared memory area currently used for allocations (must have been opened previously).
  /// \return the shared memory area currently used for allocations (must have been opened previously).
  inline SharedMemoryArea* getCurrentMemoryArea() const { return *m_shmAreas->getMapped( m_nLastAreaID ); }  
  
  /**
   * @brief Asks the daemon to create a new area.
   *
   * This method returns after receiveing an answer from the daemon.
   *
   * @return \c -1 if the daemon refused, \c the new area id otherwise.
   */
  virtual int getNewAreaFromDaemon();
  
 protected:
  
  typedef MPkMTvSparseVector< int, SharedMemoryArea* > AreaMap;
  
  ////////////////////////////////////////////////////////////////////////////
  //    Protected members
  ////////////////////////////////////////////////////////////////////////////

  /// @brief m_nMainMemId store the ID of the main shared memory area
  int  m_nMainMemId;
  
  /// @brief m_nLastAreaID stores the ID of the last area used to allocate a
  ///   buffer. This value can then be used to allocate the next one.
  ipc::MTAtomicInt m_nLastAreaID;
  
  /// @brief contains the MP interface used to get informations from the daemon
  BufferWrite   *m_interfaceBuf;
  
  /// @brief m_shmAreas contains a MP list of shared memory areas allocated by
  ///   the daemon, and the corresponding 'SharedMemoryArea' instances which are
  ///   local to the process.
  AreaMap       *m_shmAreas;
  
  ////////////////////////////////////////////////////////////////////////////
  //    Protected methods
  ////////////////////////////////////////////////////////////////////////////

  BufferWrite allocBuffer( size_t size, SharedMemoryArea * );
  
  BufferImp* allocBufferImp( size_t size );
  BufferImp* allocBufferImp( size_t size, SharedMemoryArea *area );
  BufferImp* allocBufferImp( size_t size, const AreaMap * ); // parameter for overloading purpose only
  
  /**
   * @brief initMPInterface create and initialize the MP interface to receive
   *    an answer from the daemon.
   * @param mainArea the main area in which livez the daemon
   * @return \c -1 in case of failure, \c 0 otherwise
   */
  int initMPInterface( SharedMemoryArea * mainArea );
  
  /**
   * @brief getAreaListFromDaemon get the shared list of created shared areas
   * @param mainArea The main memory in which lives the daemon
   * @return \c -1 in case of failure, \c 0 otherwise.
   */
  virtual int getAreaListFromDaemon( SharedMemoryArea * mainArea );
  

}; // class SharedMemoryManager


} // namespace mem

} // namespace flowvr
#endif
