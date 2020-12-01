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
#include "flowvr/mem/sharedmemorybuffer.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/MPkMTvSparseVector.inl"

#include "flowvr/buffer.h"
#include "flowvr/bufferimp.h"
#include "flowvr/daemondata.h"

#include <unistd.h>

#include <iostream>
#include <sstream>

namespace flowvr
{

namespace mem
{

static MPshmManagerInterface* getMPInterface( const BufferWrite &buf )
{
    return buf.getWrite< MPshmManagerInterface >( 0 );
}

SharedMemoryManager* SharedMemoryManager::instance()
{
	return dynamic_cast<SharedMemoryManager*>(Allocator::the());
}


BufferImp* SharedMemoryManager::allocBufferImp( size_t size, SharedMemoryArea *area )
{
  if ( size == 0 )
	return &SharedMemoryBuffer::emptyBuffer;

  size_t pos = area->allocBuffer(size);
  if (pos==0)
	  return NULL;
  else
  {
      if ( area->getAreaId() != m_nLastAreaID )
          m_nLastAreaID.set( area->getAreaId() );
      return SharedMemoryBuffer::create(area,pos,size);
  }
}


BufferImp* SharedMemoryManager::allocBufferImp( size_t size )
{
    if ( size == 0 )
        return &SharedMemoryBuffer::emptyBuffer;
    
    // try to allocate using the last used area (known to be already mapped)
    BufferImp* imp = allocBufferImp( size, m_shmAreas->getMapped( m_nLastAreaID ).value() );
    if ( imp != NULL )
        return imp;

    // try to allocate using already created areas
    imp = allocBufferImp( size, m_shmAreas );
    if ( imp != NULL )
        return imp;
    
    // ask the daemon to create a new area
    getNewAreaFromDaemon();
    
    // try again to allocate even if the daemon refused our request
    if ( true ) // he may have just created an area for another process/thread
    {
        imp = allocBufferImp( size, m_shmAreas );
        if ( imp != NULL )
            return imp;
    }
    
    // Couldn't get a block of the requested size
    return NULL; // NULL / invalid buffer
}


BufferImp* SharedMemoryManager::allocBufferImp( size_t size, const AreaMap* )
{
    // Ask a new buffer, to each area until we effectively get one
    for ( AreaMap::const_iterator it = m_shmAreas->begin() ; it != m_shmAreas->end() ; ++it )
    {
        // ensure the area is effectively open
        SharedMemoryArea *area = openMemoryArea(it.key());
        if ( NULL != area ) // not tested in 'allocBufferImp' (unlikely to fail)
        {
            // try allocation
            BufferImp* imp = allocBufferImp( size, area );
            if ( imp != NULL ) // buffer allocation is successful
              return imp;
        }
    }
    // Couldn't get a block of the requested size
    return NULL; // NULL / invalid buffer
}


int SharedMemoryManager::getNewAreaFromDaemon()
{
    if ( NULL == m_interfaceBuf && !m_interfaceBuf->valid() )
        return -1;
    
    // Ask the daemon to create a new area
    SharedMemoryArea *mainArea = getMainMemoryArea();
    int headpos = mainArea->readHeader();
    MPDaemonHeader *daemonHeader = mainArea->getWrite<MPDaemonHeader>(headpos);
    int cmdnum = daemonHeader->cmdChan.beginSend();
    daemonHeader->cmdTable[cmdnum].id = MPDaemonHeader::Cmd::SHMNEW;
    daemonHeader->cmdTable[cmdnum].arg = *m_interfaceBuf;
    daemonHeader->cmdChan.endSend();
    
    // read the answer
    MPshmManagerInterface *MPInterface = getMPInterface( *m_interfaceBuf );
    int infoNum = MPInterface->infoChan.beginRead();
    int id = MPInterface->infoTable[infoNum].id;
    MPInterface->infoChan.endRead();

    if ( id != MPshmManagerInterface::Info::SHMNOPE ) {
        // use the new area for ourselves
        openMemoryArea( id );
        m_nLastAreaID.set( id );
        return id; // new id is automaticaly visible in our AreaMap for others
    }

    return -1;
}


int SharedMemoryManager::getAreaListFromDaemon( SharedMemoryArea * mainArea )
{
    // Ask the daemon to send us the MP list of SharedMemoryArea IDs
    int headpos = mainArea->readHeader();
    MPDaemonHeader *daemonHeader = mainArea->getWrite<MPDaemonHeader>( headpos );
    int cmdnum = daemonHeader->cmdChan.beginSend();
    daemonHeader->cmdTable[cmdnum].id = MPDaemonHeader::Cmd::SHMLIST;
    // \note It is possible to cast since the daemon cleared the previous arg
    daemonHeader->cmdTable[cmdnum].arg = *m_interfaceBuf;
    daemonHeader->cmdChan.endSend();
   
    // we need the buffer containing the MP data of the shared list
    BufferWrite shmAreasBuf;
    
    // read the answer
    MPshmManagerInterface *MPInterface = getMPInterface( *m_interfaceBuf );
    int index = MPInterface->infoChan.beginRead();
    int answerID = MPInterface->infoTable[index].id;
    MPBuffer & b = MPInterface->infoTable[index].arg;
    int shmID = b.shmID;
    if ( MPshmManagerInterface::Info::SHMLIST == answerID // valid answer
         && m_nMainMemId == shmID )
    {
        // Note: Can't use the statement below (-->). The cast would fail when
        //   calling 'openMemoryArea()' method on an uninitialized allocator.
        //   -->     shmAreasBuf = MPInterface->infoTable[index].arg;
        //   We cast manually assuming (knowing) the buffer is contiguous.
        shmAreasBuf = BufferWrite( SharedMemoryBuffer::create(mainArea, b.offset, b.suboffset+b.subsize ), b.suboffset, b.subsize, true );
    }
    MPInterface->infoChan.endRead();
    
    // Use the received buffer to create the local instance of the MP map
    if ( shmAreasBuf.valid() )
    {
        // create our local map
        m_shmAreas = new AreaMap( shmAreasBuf );
        // store the already-mapped main area. No MT lock: we are still alone.
        m_shmAreas->map( m_nMainMemId, mainArea );
        // clear the received MPBuffer. (ok since no message is being sent)
        MPInterface->infoTable[index].arg.clear();
        return 0;
    }

    // error messages
    std::cerr << "Error initializing memory manager: couldn't get the shared area list." << std::endl;
    if ( MPshmManagerInterface::Info::SHMLIST != answerID )
        std::cerr << " Daemon answered " << answerID
                  << " instead of " << MPshmManagerInterface::Info::SHMLIST << " (SHMLIST)"
                  << std::endl;
    else if ( m_nMainMemId != shmID )
        std::cerr << " Given shared-area-list lives in memory " << shmID
                  << " instead of the chosen main memory " << m_nMainMemId
                  << std::endl;

    return -1;
}




BufferWrite SharedMemoryManager::alloc( size_t size )
{
    return allocBuffer( size );
}

bool SharedMemoryManager::realloc(BufferWrite &buffer, size_t size, bool amortized)
{
  if (buffer.valid())
    return buffer.resize(size, amortized);
  else
  {
    buffer = alloc(size);
    return buffer.valid();
  }
}


BufferWrite SharedMemoryManager::allocBuffer(size_t size, SharedMemoryArea *area )
{
  BufferImp* imp = allocBufferImp( size, area );
  if (imp==NULL)
    return BufferWrite(); // NULL / invalid buffer
  else
    return BufferWrite(imp, 0, size, true);
}

BufferWrite SharedMemoryManager::allocString( const std::string& str )
{
  BufferWrite buf = allocBuffer( str.size() );
  if (str.size()>0 && buf.writeAccess()!=NULL)
    memcpy(buf.writeAccess(),str.c_str(),str.size());
  return buf;
}

BufferWrite SharedMemoryManager::allocBuffer(size_t size )
{
    BufferImp* imp = allocBufferImp( size );
    if ( imp != NULL ) // buffer allocation is successful
        return BufferWrite( imp, 0, size, true );
    else // Couldn't get a block of the requested size
        return BufferWrite(); // NULL / invalid buffer
}


SharedMemoryArea* SharedMemoryManager::openMemoryArea( int ID, int verbose )
{
  AreaMap::const_iterator it = m_shmAreas->getMapped( ID );
  // check if already opened/mapped
  if ( it != m_shmAreas->end() )
      return *it;
  
  // not opened/mapped. Acquire lock to get the right to open it.
  ipc::ScopedMTLock locker( m_shmAreas->getMappingLock( ID ) );
  // double-check
  it = m_shmAreas->getMapped( ID );
  if ( it != m_shmAreas->end() )
      return *it;
 
  // This peculiar area haven't been opened/mapped yet by this instance. Do it.
  SharedMemoryArea *area = SharedMemoryArea::open( ID, verbose );
  if ( NULL != area )
      m_shmAreas->map( ID, area );
  
  return area;
}

SharedMemoryManager::SharedMemoryManager( unsigned int nMainMemId )
: m_nMainMemId( nMainMemId )
, m_nLastAreaID( nMainMemId )
, m_interfaceBuf( new BufferWrite )
, m_shmAreas( NULL )
{
}

SharedMemoryManager::~SharedMemoryManager()
{
    // buffers must be deleted before the shared memory is detached
    delete m_interfaceBuf;
    
    if ( NULL != m_shmAreas )
    {
        // copy the list of opened areas
        std::list< AreaMap::value_type > areas;
        for ( AreaMap::iterator it = m_shmAreas->begin()
              ; it != m_shmAreas->end() ; ++it)
        {
            if ( it.isMapping() )
                areas.push_front( AreaMap::value_type(*it) );
        }

        // delete the MP list which uses shared memory
        delete m_shmAreas;      

        // detach the shared memory areas
        for ( std::list< AreaMap::value_type >::const_iterator it = areas.begin()
              ; it != areas.end() ; ++it )
        {
            SharedMemoryArea::detach( *it );
        }
    }
}

int SharedMemoryManager::attach()
{
    int nAttachments = Allocator::attach();
    // the manager is beeing attached for the first time
    if ( nAttachments == 1 )
    {
        if ( -1 == this->init() )
        {
            this->detach();
            --nAttachments;
        }
    }
    return nAttachments;
}


int SharedMemoryManager::init()
{
    SharedMemoryArea* mainArea = SharedMemoryArea::open( m_nMainMemId );
    // Once the main memory is open, we need to allocate a communication channel
    // in order to get the shared-areas-list from the daemon.
    if ( mainArea )
    {
        // get the true area list
        if ( -1 != initMPInterface( mainArea ) )
        {
            if ( -1 != getAreaListFromDaemon( mainArea ) )
                return 0;

            // An error occured
            m_interfaceBuf->clear();
        }
        
        // An error occured
        SharedMemoryArea::detach( mainArea );
    }
    return -1;
}


int SharedMemoryManager::initMPInterface( SharedMemoryArea * mainArea )
{
    // The daemon will send only one info at a time through this channel.
    // Furthermore, size==1 helps avoiding some (minor) memory leak when
    //  initializing the memorry manager
    const size_t tableSize = 1;
    
    // alloc the  buffer for the MP interface
    const size_t size = MPshmManagerInterface::size( tableSize );
    BufferWrite buf = this->allocBuffer( size, mainArea );

    if ( buf.valid() )
    {
        // get the pid to help naming the lock of the communication channel
        std::ostringstream spid;
        const int pid = getpid();
        spid << "pid" << pid;
        
        // store the allocated buffer after initializing the MP interface
        getMPInterface( buf )->init( spid.str(), tableSize );
        *m_interfaceBuf = buf;
        return 0;
    }
    
    return -1;
}


} // namespace mem

} // namespace flowvr
