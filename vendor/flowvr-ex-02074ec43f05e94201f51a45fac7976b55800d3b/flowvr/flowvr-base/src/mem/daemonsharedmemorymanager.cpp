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
* File: src/sharedmem/sharedmemorymanager.cpp                     *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/mem/daemonsharedmemorymanager.h"
#include "flowvr/mem/MPkMTvSparseVector.inl"

#include "flowvr/daemondata.h"
#include "flowvr/utils/size.h"

#include <cstdio>

#include <unistd.h>


namespace flowvr
{

namespace mem
{

static MPshmManagerInterface* getMPInterface( const BufferWrite &buf )
{
    return buf.getWrite< MPshmManagerInterface >( 0 );
}

DaemonSharedMemoryManager::DaemonSharedMemoryManager( size_t areaSize, unsigned maxAreaNbr, unsigned int mainMemId , int cmdtablesize )
    : SharedMemoryManager( mainMemId )
    , m_shmAreaSize( areaSize )
    , m_shmMaxAreaNbr( maxAreaNbr )
    , m_shmAreaNbr( 0 )
    , m_cmdTableSize( cmdtablesize )
{
}

DaemonSharedMemoryManager::~DaemonSharedMemoryManager()
{
}

DaemonSharedMemoryManager* DaemonSharedMemoryManager::instance()
{
	return dynamic_cast<DaemonSharedMemoryManager*>( Allocator::the() );
}

int DaemonSharedMemoryManager::init()
{
    // create main memory area
    SharedMemoryArea* mainArea = SharedMemoryArea::create(
            m_nMainMemId, m_shmAreaSize, MPDaemonHeader::size( m_cmdTableSize )
    );
    if ( NULL != mainArea )
    {
        // update the size member according to the size allocated by the system
        m_shmAreaSize = mainArea->getSize();
        // keep the count of total allocated memory
        m_shmAreaNbr++;
    
        if ( -1 != initMPInterface( mainArea ) )
        {
             if ( -1 != getAreaListFromDaemon( mainArea ) )
                return 0;
             
             // An error occured
             m_interfaceBuf->clear();
        }
        
        // An error occured
        // SharedMemoryArea::release( m_nMainMemId ); // keep for debug 
        SharedMemoryArea::detach( mainArea );        
    }
        
    return -1;
}


SharedMemoryArea* DaemonSharedMemoryManager::createNewMemoryArea()
{
    ipc::ScopedMPLock locker( m_shmAreas->getInsertLock() );
    // check you don't exceed the limit (after acquiring the lock)
    if ( m_shmAreaNbr < m_shmMaxAreaNbr )
    {
        // get a new ID
        int newAreaID = getNewAreaID();
        if ( -1 != newAreaID )
        {
            // Create the new area, with no deamon header
            SharedMemoryArea *area =
                    SharedMemoryArea::create( newAreaID, m_shmAreaSize );
            if ( area != NULL )
            {
                m_shmAreaNbr++;
                m_shmAreas->insert( newAreaID, area );
                return area;
            }
        }
    }
    return NULL;
}



void DaemonSharedMemoryManager::releaseMemory()
{
    for ( AreaMap::const_iterator it = m_shmAreas->begin()
        ;  it != m_shmAreas->end()  ;  ++it )
    {
        SharedMemoryArea::release( *it );
    }
}


void DaemonSharedMemoryManager::processCmdShmNew( MPBuffer &arg )
{
    // get the daemon manager
    DaemonSharedMemoryManager* shmman = DaemonSharedMemoryManager::instance();
    // get the MP interface of the requesting manager to send him the anwser
    MPshmManagerInterface* MPInterface = getMPInterface( arg );

    int newArea = -1;
    if ( NULL != shmman ) {
        newArea = shmman->getNewAreaFromDaemon();
        if ( -1 != newArea ) {
            // the new area as been created (It's already in the shared list)
            const int index = MPInterface->infoChan.beginSend();
            MPInterface->infoTable[index].id = newArea;
            MPInterface->infoChan.endSend();
            return;
        }
    }
    // The creation of a new memory area failed at some point, probably
    // because the maximum has been reached. Inform the requesting manager
    int index = MPInterface->infoChan.beginSend();
    MPInterface->infoTable[index].id = MPshmManagerInterface::Info::SHMNOPE;
    MPInterface->infoTable[index].arg.clear();
    MPInterface->infoChan.endSend();  
}


void DaemonSharedMemoryManager::processCmdShmList( MPBuffer &arg )
{
    DaemonSharedMemoryManager *shmman = DaemonSharedMemoryManager::instance();
    // get the MP interface of the requesting manager
    BufferWrite shmmanBuf = arg;
    MPshmManagerInterface *MPInterface = getMPInterface( shmmanBuf );
    if ( NULL == shmman ) // shouldn't happen
    {
        int index = MPInterface->infoChan.beginSend();
        MPInterface->infoTable[index].id = MPshmManagerInterface::Info::SHMNOPE;
        MPInterface->infoTable[index].arg.clear();
        MPInterface->infoChan.endSend();  
    }
    else
    {
        // Send MPBuffer storing the shared list of shmArea IDs
        int index = MPInterface->infoChan.beginSend();
        MPInterface->infoTable[index].id = MPshmManagerInterface::Info::SHMLIST;
        MPInterface->infoTable[index].arg = *shmman->m_shmAreas;
        MPInterface->infoChan.endSend();
    }
}


int DaemonSharedMemoryManager::getNewAreaID() const
{
    int lastAreaID = m_shmAreas->rbegin().key();
    // SharedMemoryArea::isFree() may be costly. you try with keys following the
    // last that were created, which are way more likely to be free
    for ( int k = 1 + lastAreaID ; k < m_shmAreas->keyRange_end() ; k++ )
    {
        if ( m_shmAreas->end() == m_shmAreas->find( k )
             && SharedMemoryArea::isFree( k ) )
            return k;
    }
    // in case of failure, try with those preceeding the lastly created
    for ( int k = m_shmAreas->keyRange_begin() ; k < lastAreaID ; k++ )
    {
        if ( m_shmAreas->end() == m_shmAreas->find( k )
             && SharedMemoryArea::isFree( k ) )
            return k;
    }
    // the whole range is already in use.
    return -1;
}





int DaemonSharedMemoryManager::getNewAreaFromDaemon()
{  // we ARE the daemon... we have to create it...
    
    SharedMemoryArea *area = createNewMemoryArea();
    if ( NULL != area )
        return area->getAreaId();
    return -1;
}

int DaemonSharedMemoryManager::getAreaListFromDaemon( SharedMemoryArea * mainArea )
{   // we ARE the daemon... we have to create it...
    
    // allocate the shared memory for the shared-Areas-list
    const size_t MPDataSize = AreaMap::MPData::size( m_shmMaxAreaNbr );
    BufferWrite shmAreasBuf = allocBuffer( MPDataSize, mainArea );
    
    if ( shmAreasBuf.valid() )
    {
        // init the shared-data of the area-list
        shmAreasBuf.getWrite< AreaMap::MPData >()->init( m_shmMaxAreaNbr, m_nMainMemId, m_nMainMemId+100 );
        // create the local instance
        m_shmAreas = new AreaMap( shmAreasBuf );
        // insert the main memory area. No lock this time since we are alone
        m_shmAreas->insert( m_nMainMemId, mainArea );
        return 0;
    }

    return -1;
}


std::vector< std::string > DaemonSharedMemoryManager::status() const
{
    utils::Size usedTotal = size_t(0);
    utils::Size sizeTotal = size_t(0);
    
    std::vector< std::string > result( 1 + m_shmAreas->size() );
    
    // generate output for each area
    for ( AreaMap::const_iterator it( m_shmAreas->begin() )
          ;  it != m_shmAreas->end()  ;  ++it )
    {
        if ( ! it.isMapping() )
            // the (last) key is inserted but the value is still beeing mapped
            break;
            
        // resize the vector in case a key has been inserted concurrently
        result.resize( 1 + m_shmAreas->size() );
        
        // get area values
        const SharedMemoryArea * shm = it.value();
        utils::Size areaUsed = shm->getSize() - shm->getFreeSize();
        utils::Size areaSize = shm->getSize();
        // update total values
        usedTotal = usedTotal + areaUsed;
        sizeTotal = sizeTotal + areaSize;
        
        // round area values
        areaSize.round();
        areaUsed.round();
        // generate output
        char c_str[1024];
        snprintf( c_str, sizeof(c_str), "  shm  #%2d  (id %5d) %6zd%c / %zd%c",
                 it.index(), it.key(),
                 areaUsed.getCount(), areaUsed.getUnit(),
                 areaSize.getCount(), areaSize.getUnit()
               );
        result[ 1 + it.index() ] = c_str;
    }

    sizeTotal.round();
    usedTotal.round();

    // generate output for the total shared memory
    char c_str[1024];
    snprintf( c_str, sizeof(c_str), "  shm total:           %6zd%c / %zd%c  ",
             usedTotal.getCount(), usedTotal.getUnit(),
             sizeTotal.getCount(), sizeTotal.getUnit()
           );
    result[ 0 ] = c_str;
    
    return result;
}


} // namespace mem

} // namespace flowvr
