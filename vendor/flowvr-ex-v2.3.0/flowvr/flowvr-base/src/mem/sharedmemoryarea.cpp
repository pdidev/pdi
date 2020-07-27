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
 * File: src/sharedmem/sharedmemoryarea.cpp                        *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/mem/sharedmemoryarea.h"
#include <iostream>

#include <flowvr/utils/backtrace.h>

#include <sys/shm.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#include <iostream>

namespace flowvr
{

namespace mem
{

enum
{
	NB_PIDS = 1024
};

/// Constructor
SharedMemoryArea::SharedMemoryArea(int id, size_t size, ubyte* mapwrite, const ubyte* mapread)
: areaId(id)
, areaSize(size)
, mappingWrite(mapwrite)
, mappingRead(mapread == NULL ? mapwrite : mapread)
, m_areaRef(0)
, m_attachPos(0)
{
}

/// Initialize the newly created area.
/// Return the position of this area's header with the requested size.
size_t SharedMemoryArea::initialize(size_t headersize)
{
	// set the header of the area at the begining of the shared segment
	AreaHeader* head = getWrite<AreaHeader> (0);
	head->ID = *(const int*) "flowvr";
	head->versionMaj = 0;
	head->versionMin = 0;
	head->headerPos = alignUp(sizeof(AreaHeader) + NB_PIDS * sizeof(ipc::MPAtomicInt));
	head->allocLock.init(NULL);

	// Reserve a chunk for the daemon header
	HeaderAlloc *allochead = getWrite<HeaderAlloc> (head->headerPos);
	allochead->length = alignUp(sizeof(HeaderAlloc) + headersize);
	allochead->nbref = 0;
	head->firstFreePos = head->headerPos + allochead->length;
	
	// Leave an empty chunk storing the position of the first free chunk
	HeaderFree* freehead = getWrite<HeaderFree> (head->firstFreePos);
	freehead->length = alignUp(sizeof(HeaderFree));
	int nextfreepos = head->firstFreePos + freehead->length;
	freehead->length = 0; // never merge this chunk with any adjacent chunk
	freehead->prevDP = 0; // this indicates a dummy free chunk
	freehead->nextDP = nextfreepos - head->firstFreePos;
	
	// Set a single free chunck from which we'll be able to allocate
	HeaderFree* nextfree = getWrite<HeaderFree> (nextfreepos);
	nextfree->length = areaSize - nextfreepos;
	nextfree->prevDP = head->firstFreePos - nextfreepos;
	nextfree->nextDP = nextfree->length;
	head->freeSize = nextfree->length - sizeof(HeaderAlloc);
	return head->headerPos + sizeof(HeaderAlloc);
}

size_t SharedMemoryArea::readHeader() const
{
	const AreaHeader* head = getRead<AreaHeader> (0);
	if (head->ID != *(const int*) "flowvr")
	{
		std::cerr << "FLOWVR: Bad shared memory area ID: "
				  << head->ID
				  << std::endl;
		return 0;
	}

	return head->headerPos + sizeof(HeaderAlloc);
}

size_t SharedMemoryArea::searchFreeBuffer(size_t size, size_t *bestFit)
{
	ipc::ScopedMPLock locker(getWrite<AreaHeader> (0)->allocLock,
			                 "SharedMemoryArea::searchFreeBuffer");
	return searchFreeBufferLocked(size, bestFit);
}

/// Search for an empty area. Return its position or 0 if none found.
/// Assume the caller already own the lock.
size_t SharedMemoryArea::searchFreeBufferLocked(size_t size, size_t* bestFit)
{
	size_t bestfitsize = (bestFit == NULL) ? areaSize : *bestFit;
	size_t bestfitpos = 0;

	if (size < MinAllocSize)
		size = MinAllocSize;

	// align the size and add header length
	size = alignUp(size + sizeof(HeaderAlloc));

	size_t pos = getRead<AreaHeader> (0)->firstFreePos;
	while (pos < areaSize && bestfitsize > size)
	{
		const HeaderFree* head = getRead<HeaderFree> (pos);
		if (head->length >= size && head->length < bestfitsize)
		{ // this is a new candidate
			bestfitpos  = pos + sizeof(HeaderAlloc);
			bestfitsize = head->length;
		}
		pos += head->nextDP;
	}

	if (bestFit != NULL && bestfitpos != 0)
		*bestFit = bestfitsize;

	return bestfitpos;
}

size_t SharedMemoryArea::allocBuffer(size_t size, size_t pos)
{
	ipc::ScopedMPLock locker(getWrite<AreaHeader> (0)->allocLock,
			                 "SharedMemoryArea::allocBuffer");
	if (pos == 0)
		pos = searchFreeBufferLocked(size, NULL);

	if (pos == 0)
	{
		std::cerr << "allocBuffer search: DID NOT FIND [" << size
				<< "] bytes, when starting at pos [" << pos << "]" << std::endl;

		flowvr::utils::stacktrace();
		return 0; // no free chunk of this size
	}

	if (size < MinAllocSize)
		size = MinAllocSize;

	// align the size and add header length
	size = alignUp(size + sizeof(HeaderAlloc));
	pos -= sizeof(HeaderAlloc);

	AreaHeader* head     = getWrite<AreaHeader> (0);
	HeaderFree* freehead = getWrite<HeaderFree> (pos);

	if (freehead->length < size || freehead->prevDP >= 0)
	{
		std::cerr << "allocBuffer search: DID NOT FIND [" << size << "] bytes."
				<< " Header at pos [" << pos << "] is of size ["
				<< freehead->length << "]" << std::endl;

		flowvr::utils::stacktrace();
		return 0; // bad chunk
	}

	if (freehead->length <= size + (int) sizeof(HeaderAlloc) + MinAllocSize)
	{   // no room left
		// just remove this chunk from the free list
		if (freehead->prevDP < 0)
			getWrite<HeaderFree> (pos + freehead->prevDP)->nextDP += freehead->nextDP;

		if ((unsigned) (pos + freehead->nextDP) < (unsigned) areaSize)
			getWrite<HeaderFree> (pos + freehead->nextDP)->prevDP += freehead->prevDP;

		head->freeSize -= freehead->length - sizeof(HeaderAlloc);
	}
	else
	{ // split the chunk
		HeaderFree* newfree = getWrite<HeaderFree> (pos + size);
		newfree->length = freehead->length - size;
		newfree->nextDP = freehead->nextDP - size;
		newfree->prevDP = freehead->prevDP - size;
		freehead->length = size;
		if (freehead->prevDP < 0)
			getWrite<HeaderFree> (pos + freehead->prevDP)->nextDP += freehead->length;
		if (pos + freehead->nextDP < areaSize)
			getWrite<HeaderFree> (pos + freehead->nextDP)->prevDP += freehead->length;

		head->freeSize -= size;
	}

	HeaderAlloc* allochead = getWrite<HeaderAlloc> (pos);
	allochead->nbref = 0; // this block is now allocated

	return pos + sizeof(HeaderAlloc);
}

void SharedMemoryArea::addRef(size_t pos)
{
	if (pos >= areaSize)
		return;

	// pos is expected to indicate the beginning of the user-data block
	// so navigate back to the header
	pos -= sizeof(HeaderAlloc);

	// interpret that byte as header
	HeaderAlloc* header = getWrite<HeaderAlloc> (pos);
#ifdef SHMDEBUG
	std::cout<<"addRef("<<(void*)(pos+sizeof(HeaderAlloc))<<"): "<<(int)header->nbref<<"+1"<<std::endl;
#endif
	++(header->nbref);
}

bool SharedMemoryArea::isValidRef(size_t pos) const
{
	pos -= sizeof(HeaderAlloc);
	const HeaderAlloc* header = getRead<HeaderAlloc> (pos);
#ifdef SHMDEBUG
	std::cout<<"isValid("<<(void*)(pos+sizeof(HeaderAlloc))<<"): "<<(int)header->nbref<<std::endl;
#endif
	return ((int) header->nbref) >= 0;
}

size_t SharedMemoryArea::getBufferSize(size_t pos) const
{
	pos -= sizeof(HeaderAlloc);
	const HeaderAlloc* header = getRead<HeaderAlloc> (pos);
	return header->length - sizeof(HeaderAlloc);
}

int SharedMemoryArea::getNbRef(size_t pos) const
{
	pos -= sizeof(HeaderAlloc);
	const HeaderAlloc* header = getRead<HeaderAlloc> (pos);
	const int nbref = header->nbref;
	return nbref;
}

/// Release a reference to a buffer, possibly destroying it.
void SharedMemoryArea::freeRef(size_t pos)
{
	if (pos >= areaSize)
		return;

	pos -= sizeof(HeaderAlloc);

	{ // scope for loacl variables
		HeaderAlloc* header = getWrite<HeaderAlloc> (pos);
#ifdef SHMDEBUG
		std::cout<<"freeRef("<<(void*)(pos+sizeof(HeaderAlloc))<<"): "<<(int)header->nbref<<"-1"<<std::endl;
#endif
		if (!header->nbref.dec_and_test_null()) // ATOMIC
		{
			return; // still some reference(s)
		}
	}

	// last reference: free the buffer
	{
		ipc::ScopedMPLock locker(getWrite<AreaHeader> (0)->allocLock,
				                 "SharedMemoryArea::freeRef");
#ifdef DEBUG
		int lastFreeSize = getWrite<AreaHeader>(0)->freeSize;
		std::cout<<"Free buffer @ "<<(void*)(pos+sizeof(HeaderAlloc));
#endif

		// search next and prev free chunks
		size_t prevpos = getRead<AreaHeader> (0)->firstFreePos;
		size_t nextpos;

		while ((nextpos = getRead<HeaderFree> (prevpos)->nextDP + prevpos) < pos)
			prevpos = nextpos;

		HeaderFree* head = getWrite<HeaderFree> (pos);
		HeaderFree* prev = getWrite<HeaderFree> (prevpos);
		if (prevpos + prev->length == pos)
		{ // merge with the previous chunk
			getWrite<AreaHeader> (0)->freeSize += head->length; // we have the whole chunk available
			prev->length += head->length;
			// this is now our working chunk
			pos = prevpos;
			head = prev;
		}
		else
		{ // set the next free chunk of the previous chunk to this chunk and the previous chunk of this chunk to the previous chunk.
			getWrite<AreaHeader> (0)->freeSize += head->length - sizeof(HeaderAlloc);
			// we have the chunk available minus one header
			head->prevDP = prevpos - pos;
			head->nextDP = prevpos + prev->nextDP - pos;
			prev->nextDP = pos - prevpos;
		}

		if (nextpos < areaSize)
		{
			HeaderFree* next = getWrite<HeaderFree> (nextpos);
			if (pos + head->length == nextpos)
			{ // merge with the next chunk
				getWrite<AreaHeader> (0)->freeSize += sizeof(HeaderAlloc); // we won the size of one header
				head->length += next->length;
				head->nextDP += next->nextDP;
				if (pos + head->nextDP < areaSize)
					getWrite<HeaderFree> (pos + head->nextDP)->prevDP = -head->nextDP;
			}
			else
			{ //set the previous free chunk of the next chunk to this chunk and the next chunk of this chunk to the next chunk
				head->nextDP = nextpos - pos;
				next->prevDP = pos - nextpos;
			}
		}
#ifdef DEBUG
		std::cout<<" remaining: "<<getWrite<AreaHeader>(0)->freeSize<<"(+"<<getWrite<AreaHeader>(0)->freeSize-lastFreeSize<<")"<<std::endl;
#endif
	}
}

void SharedMemoryArea::dumpDebugInfo() const
{
	const AreaHeader* header = getRead<AreaHeader> (0);
	std::cout << "Daemon header at 0x"
			  << std::hex << header->headerPos
			  << std::dec << std::endl;
	std::cout << "Head bloc at 0x"
			  << std::hex << header->firstFreePos
			  << std::dec << std::endl;


	size_t freesize = 0;
	size_t fpos     = header->firstFreePos;
	size_t flength  = alignUp(sizeof(HeaderFree));

	const HeaderFree* fhead = getRead<HeaderFree> (fpos);
	while (fpos < areaSize && fhead->nextDP > 0)
	{
		size_t fnext = fpos + fhead->nextDP;
		if (fnext > areaSize)
		{
			std::cerr << "  Bad next free bloc pointer: 0x" << std::hex << fpos
					  << "+0x"
					  << (int) fhead->nextDP << "=0x" << fnext
					  << " > 0x" << areaSize << std::dec << std::endl;
			fnext = areaSize;
		}

		if (flength > 0)
		{
			size_t pos = fpos + flength;
			while (pos < fnext)
			{
				const HeaderAlloc* h = getRead<HeaderAlloc> (pos);
				std::cout << "  Buffer pos=0x" << std::hex << pos << std::dec
						  << " length=0x" << std::hex << h->length << std::dec
						  << " nbref=" << (int) h->nbref << std::endl;
				if ((int) h->nbref < 0)
					std::cerr << "    NEGATIVE NBREF!" << std::endl;

				if (h->length < (size_t) sizeof(HeaderAlloc) + MinAllocSize || pos + h->length > fnext)
				{
					std::cerr << "    INVALID LENGTH!" << std::endl;
					break;
				}
				pos += h->length;
			}
		}

		if (fnext < areaSize)
		{
			fhead = getRead<HeaderFree> (fnext);
			flength = fhead->length;
			std::cout << "Bloc pos=0x" << std::hex << fnext << " length=0x"
					  << fhead->length << " prevDP=-0x" << -(int) fhead->prevDP
					  << " nextDP=0x" << (int) fhead->nextDP << std::dec
					  << std::endl;
			freesize += flength - sizeof(HeaderAlloc);
			if (fhead->prevDP != fpos - fnext)
				std::cerr << "  INVALID PREV POINTER! (should be -0x"
						  << std::hex << (fnext - fpos) << std::dec << ")"
						  << std::endl;
			fpos = fnext;
			fnext = fpos + fhead->nextDP;
			if (fhead->nextDP <= 0 || fnext > areaSize)
			{
				std::cerr << "  INVALID NEXT POINTER!" << std::endl;
				fnext = areaSize;
			}
			if (flength < (int) sizeof(HeaderAlloc) + MinAllocSize || fpos + flength > fnext)
			{
				std::cerr << "    INVALID LENGTH!" << std::endl;
				flength = 0;
			}
		}
		else
			break;
	}
	std::cout << "Free memory " << header->freeSize << ", found " << freesize
			  << " (" << (freesize) / (areaSize / 100) << "%)" << std::endl;
}

int SharedMemoryArea::attachSize() const
{
	const AreaHeader* head = getRead<AreaHeader> (0);
	return (head->headerPos - sizeof(AreaHeader)) / sizeof(ipc::MPAtomicInt);
}

int SharedMemoryArea::getAttachPID(int i) const
{
	if (i < 0 || i >= attachSize())
		return 0;
	const AreaHeader* head = getRead<AreaHeader> (0);
	return head->attached[i];
}

int SharedMemoryArea::attachPID(int pid)
{
#ifdef SHMDEBUG
    std::cout << "\tArea " << this->getAreaId() << " attach : pid == " << pid
              << "\t\t m_areaRef == " << m_areaRef
              << std::endl;
#endif
	++m_areaRef;
	int n = attachSize();
	AreaHeader* head = getWrite<AreaHeader> (0);
	for (int i = 0; i < n; i++)
		if (!head->attached[i])
		{
            if ( head->attached[i].compare_and_swap( 0, pid ) )
				return m_attachPos = i;
		}
#ifdef SHMDEBUG
std::cout << "\t\t Unable to add PID"
              << std::endl;
#endif
	return -1;
}

bool SharedMemoryArea::detachPID(int pid)
{
#ifdef SHMDEBUG
    std::cerr << "\tArea " << this->getAreaId() << " detach : pid == " << pid
              << "\t\t m_areaRef == " << (m_areaRef-1)
              << std::endl;
#endif
	AreaHeader* head = getWrite<AreaHeader> (0);
	if ( m_attachPos >= 0 && m_attachPos < attachSize() )
        head->attached[m_attachPos].compare_and_swap( pid, 0 );

    return m_areaRef.dec_and_test_null();
}










bool SharedMemoryArea::isFree( int ID )
{
    // The new ID shall not be associed to any existing segment.
    // Hence, we need the 'shmget' call to fail.
    if ( shmget( ID, 0, 0 ) == -1 )
    {
        int err = errno;
        // the error code must confirm that the segment is nonexistent
        if ( err == ENOENT )
            return true;
    }
    return false;
}

SharedMemoryArea* SharedMemoryArea::create( int ID, size_t size, size_t headersize, int verbose )
{
    if (verbose >= 1)
    std::cout << "Creating shared memory area (ID="
                << ID
                << ", size="
                << size
                << ")"
                << std::endl;
  
    int shmid=-1;
    // Create the memory area
    shmid = shmget(ID,size, 0666|IPC_CREAT|IPC_EXCL);
    if (shmid==-1 && errno==EEXIST)
    {
      if (verbose >= 1)
      std::cout<<"Shared memory " << ID << " already exists, trying to recreate it." << std::endl;
      shmid = shmget(ID,0,0);
      if (shmid!=-1) // can access it
      {
        shmctl(shmid,IPC_RMID,NULL);
        // we hope it is now removed
        shmid = shmget(ID,size, 0666|IPC_CREAT|IPC_EXCL);
      }
    }
    if (shmid==-1)
    {
      int err = errno;
      const char* errmsg = strerror(err);
      std::cerr<<"Error :"<<err<<": "<<errmsg<<std::endl;
      switch (err)
      {
      case EACCES: std::cerr << "Shared memory "<<ID<<" creation denied"<<std::endl; break;
      case EEXIST: std::cerr << "Shared memory "<<ID<<" already exists"<<std::endl; break;
      case EINVAL: std::cerr << "Shared memory "<<ID<<" invalid size: "<<size<<std::endl; break;
      case ENOSPC: std::cerr << "Shared memory "<<ID<<" creation failed (no enough space for "<<size<<" bytes)"<<std::endl; break;
      case ENOMEM: std::cerr << "Shared memory "<<ID<<" creation failed (no enough memory for "<<size<<" bytes)"<<std::endl; break;
      default:     std::cerr << "Shared memory "<<ID<<" creation failed (size="<<size<<")"<<std::endl; break;
      }
      return NULL;
    }
    
    // get the final size
    shmid_ds shmattr;
    if( -1 != shmctl(shmid,IPC_STAT,&shmattr) )
        size = shmattr.shm_segsz;
    else // Should absolutly never happen since we just created this segment
    {
        std::cerr << "error on call to shmctl:" << std::endl;
        int err = errno;
        const char *errstr = strerror(err);
        std::cerr << "Error :" << err << ": " << errstr << std::endl;
    }
    
    // Map the area into local space
    void* mapping = shmat(shmid,NULL,0);
    // Error should absolutly never happen since we just created this segment
    if (mapping==NULL || mapping==(void*)-1)
    {
      const char* errmsg=strerror(errno);
      std::cerr << "Failed to map shared memory area (ID=" << ID << ", internal id=" << shmid << ") : " << errmsg << std::endl;
      return NULL;
    }
  
    // the requested new area
    SharedMemoryArea *sharedArea = new SharedMemoryArea(ID, size, (ubyte*)mapping);
    sharedArea->initialize( headersize );
    sharedArea->attachPID((int)getpid());
    
    if (verbose >= 3)
    std::cout << "Shared memory area mapped at "
              << (const void*)sharedArea->getMappingRead()
              << std::endl;
  
    return sharedArea;
}



SharedMemoryArea* SharedMemoryArea::open( int ID, int verbose )
{
      int shmid = shmget(ID,0,0);
      if ( -1 == shmget(ID,0,0) )
      {
        int err = errno;
        const char* errmsg = strerror(err);
        std::cerr<<"Error :"<<err<<": "<<errmsg<<std::endl;
        switch (err)
        {
        case EACCES: std::cerr << "Shared memory "<<ID<<" access denied"<<std::endl; break;
        case EIDRM:  std::cerr << "Shared memory "<<ID<<" already deleted"<<std::endl; break;
        case EINVAL: std::cerr << "Shared memory "<<ID<<" not found"<<std::endl; break;
        case ENOMEM: std::cerr << "Shared memory "<<ID<<" access failed (no enough memory)"<<std::endl; break;
        default:     std::cerr << "Shared memory "<<ID<<" access failed"<<std::endl; break;
        }
        return NULL;
      }
      // get the size
      shmid_ds shmattr;
      if( -1 == shmctl(shmid,IPC_STAT,&shmattr) )
      {
          std::cerr << "error on call to shmctl:" << std::endl;
          int err = errno;
          std::cerr << "Error :" << err << ": " << strerror(err) << std::endl;
      }
      else
      {
          if (verbose >= 1)
          std::cout<<"Opened shared memory area with ID="<<ID
                 <<": internal id = " << shmid
                 << " size = " << shmattr.shm_segsz
                 << " attached " << shmattr.shm_nattch
                 << " time(s)" << std::endl;
      }
      // and finally map it
      void* mapping = shmat(shmid,NULL,0);
      if (mapping==NULL || mapping==(void*)-1)
      {
        const char* errmsg=strerror(errno);
        std::cerr << "Failed to map shared memory area (ID="
                  << ID
                  << ", internal id="
                  << shmid
                  << ") : "
                  << errmsg << std::endl;
    
        return NULL;
      }
      
      if (verbose >= 3)
        std::cout << "Mapped shared memory area with ID="
                  << ID
                  << " at "
                  << mapping
                  << std::endl;
    
      // opening succesful
      SharedMemoryArea *newarea = new SharedMemoryArea(ID, shmattr.shm_segsz, (ubyte*)mapping);
      newarea->attachPID((int)getpid());
              
      return newarea;
}



int SharedMemoryArea::detach( SharedMemoryArea *area )
{
    // detach PID
    area->detachPID( getpid() );
    
    // release the mapped pointer
    if ( -1 == shmdt( area->mappingWrite ) )
    {
        int err = errno;
        const char* errmsg = strerror( err );
        std::cerr << "Releasing shared memory " << area->getAreaId() << " FAILED." << std::endl;
        std::cerr << "\t shmget Error :" << err << ": " << errmsg << std::endl;
        return -1;
    }
    
    // delete the area itself
    delete area;
    return 0;
}

int SharedMemoryArea::release( SharedMemoryArea *area )
{
    int ID = area->getAreaId();
    int shmid = shmget( ID, 0, 0 );
    if ( -1 == shmid )
    {
        int err = errno;
        const char* errmsg = strerror( err );
        std::cerr << "Releasing shared memory " << ID << " FAILED." << std::endl;
        std::cerr << "\t shmget Error :" << err << ": " << errmsg << std::endl;
        return -1;
    }
    else if ( -1 == shmctl( shmid, IPC_RMID, NULL ) )
    {
        int err = errno;
        const char* errmsg = strerror( err );
        std::cerr << "Releasing shared memory " << ID << " FAILED." << std::endl;
        std::cerr << "\t shmctl Error :" << err << ": " << errmsg << std::endl;
        return -1;
    }
    return 0;
}



} // namespace mem

} //namespace flowvr
