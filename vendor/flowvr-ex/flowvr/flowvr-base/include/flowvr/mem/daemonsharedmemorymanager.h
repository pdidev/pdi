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
#ifndef FLOWVR_SHAREDMEM_DAEMONSHAREDMEMORYMANAGER_H
#define FLOWVR_SHAREDMEM_DAEMONSHAREDMEMORYMANAGER_H

#include "flowvr/common.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/MPkMTvSparseVector.h"

#include <string>
#include <vector>


namespace flowvr
{

namespace mem
{

class DaemonSharedMemoryManager : public SharedMemoryManager
{
public:
    
    /// Constructor
    DaemonSharedMemoryManager( size_t areaSize, unsigned maxAreaNbr, unsigned int mainMemId = flowvr::DefaultMemId, int cmdtablesize = 64 );
    /// virtual Destructor
    virtual ~DaemonSharedMemoryManager();
    
    /// Try to release the memory areas.
    /// Each one will be removed once all processes using it will be closed.
    void releaseMemory();

    static DaemonSharedMemoryManager* instance();
    
    SharedMemoryArea* createNewMemoryArea();

    /**
     * @brief processCmdShmList process a \c SHMLIST command from the \c
     *      SharedMemoryManager of another process.
     * @param arg the \c MPshmManagerInterface of the requesting instance
     */
    static void processCmdShmList( mem::MPBuffer& arg );/// Send the MP area-list 

    /**
     * @brief processCmdShmList process a \c SHMNEW command from the \c
     *      SharedMemoryManager of another process.
     * @param arg the \c MPshmManagerInterface of the requesting instance
     */
    static void processCmdShmNew( mem::MPBuffer& arg ); /// Create new area
    
    std::vector< std::string > status() const;
   
protected:
    
    ////////////////////////////////////////////////////////////////////////////
    //    Protected members
    ////////////////////////////////////////////////////////////////////////////
    
    size_t m_shmAreaSize; ///< Size of the areas to allocate
    unsigned m_shmMaxAreaNbr; ///< Maximum number of areas we can allocate
    unsigned m_shmAreaNbr; ///< Number of allocated shared memory areas 
    
    ////////////////////////////////////////////////////////////////////////////
    //    Protected methods
    ////////////////////////////////////////////////////////////////////////////
    
    /// \brief connect to the main memory Area and get the shares-areas list
    virtual int init(); ///< \return \c -1 if it fails, \c 0 otherwise
        
    /// Compute a new and free memory ID, or -1
    int getNewAreaID() const;
    
    /// This method try to allocate a new area (not the main one!) and returns
    ///  the new Id if the operation is successful
    virtual int getNewAreaFromDaemon();
    
    /// Get the MP shared list to the daemon
    virtual int getAreaListFromDaemon( SharedMemoryArea * mainArea );    
    
private:
    
    ////////////////////////////////////////////////////////////////////////////
    //    Private members
    ////////////////////////////////////////////////////////////////////////////
    
    int m_cmdTableSize;
    
    ////////////////////////////////////////////////////////////////////////////
    //    Private methods
    ////////////////////////////////////////////////////////////////////////////
        
    
}; // class DaemonSharedMemoryManager


        
} // namespace mem

} // namespace flowvr
#endif
