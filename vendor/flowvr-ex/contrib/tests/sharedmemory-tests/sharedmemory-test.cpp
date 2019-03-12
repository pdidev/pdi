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
*                                                                 * 
*******************************************************************
*                                                                 *
* File: sharedmemory-test.cpp                                     *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include <flowvr/utils/cmdline.h>
#include <flowvr/mem/daemonsharedmemorymanager.h>
#include <flowvr/mem/sharedmemoryarea.h>
#include <flowvr/ipc/atomic.h>
#include <flowvr/utils/backtrace.h>
#include <signal.h>

#include <flowvr/buffer.h>

flowvr::utils::Option<unsigned long long> shmSize("shm-size", 's', "Shared memory size", 33554432U, false);//6400000, false);
flowvr::utils::Option<int> shmMemId("shm-id", 'i', "Shared memory ID", 57776, false);


using namespace flowvr::mem;
using namespace flowvr;

void state( const char *pcName, bool bCompleted )
{
	std::cout << pcName << (bCompleted ? ": OK" : ": FAILED") << std::endl;
}

bool testSharedMemoryCreation();

bool testZeroAllocation();
bool testHalfAllocation();
bool testFullAllocation();
bool testMoreThanFullAllocation();
bool testMoreThan32bitAllocation();

bool testCauseNewArea();
bool testMPBufferFromUnopenedArea();


// global vars to be accessible for signal handler
// and gentle shoutdown of the test-suite
DaemonSharedMemoryManager *g_dshmman = NULL;
SharedMemoryManager *g_shmman = NULL;
SharedMemoryArea    *g_shm    = NULL;
int pos    = 0;
int mem_id = 0;
bool delShmMan = false;


void segvhandler(int)
{
	flowvr::utils::stacktrace();

	if(g_shm)
	{
		// create a local copy of the pointer
		SharedMemoryArea *lc = g_shm;
		g_shm = NULL; // set this one to NULL (in case the detach will cause a segfault,
		            // we do not have an endless cycle here...
		SharedMemoryArea::detach( lc );
		SharedMemoryArea::release( lc );
		delete lc; // kill memory
	}

	signal(SIGSEGV, SIG_DFL); // proceed with default handling
}


int main(int argc, char **argv)
{
	flowvr::utils::CmdLine line( std::string(argv[0]) + std::string(": some tests on the shared-memory low level routines"));

	bool bError = false;
	if( line.parse(argc, argv, &bError ) == false )
	{
		if(bError)
			std::cerr << "Error during command line parsing. Options are:" << std::endl;

		std::cout << line.help() << std::endl;
		return 0;
	}


	(void) signal(SIGSEGV, segvhandler);

	std::cout << "Using MPAtomicIntBw Implementation: " << flowvr::ipc::MPAtomicIntBw::getImplName() << std::endl;

	mem_id           = shmMemId.value();
	size_t  mem_size = size_t(shmSize.value());

	
	
	std::cout << "//////////////////////////////////////" << std::endl;
	std::cout << "//    DaemonSharedMemoryManager (size = " << shmSize.value() << ")" << std::endl << std::endl;
	
	DaemonSharedMemoryManager *dshmman =
			new DaemonSharedMemoryManager( shmSize, 2, shmMemId);
	Allocator::setAllocator(dshmman);
	Allocator::the()->attach();
	g_dshmman = DaemonSharedMemoryManager::instance();
	if(g_dshmman == NULL)
	{
		std::cerr << "could not create DaemonSharedMemoryManager with main memory area of size [" << mem_size << "]" << std::endl;
		return -1;
	}
	g_shm = g_dshmman->getMainMemoryArea();
	delShmMan = true;

	state("testSharedMemoryCreation", testSharedMemoryCreation());
	state("testZeroAllocation", testZeroAllocation());
	state("testHalfAllocation", testHalfAllocation());
	state("testFullAllocation", testFullAllocation());
	state("testMoreThanFullAllocation", testMoreThanFullAllocation());
	state("testMoreThan32bitAllocation", testMoreThan32bitAllocation());
	
	state("testCauseNewArea", testCauseNewArea());

	if( delShmMan )
	{
		delShmMan = false;
		dshmman = DaemonSharedMemoryManager::instance();
		dshmman->releaseMemory();
		Allocator::the()->detach();
	}

	std::cout << std::endl;
	
	
	
	
	std::cout << std::endl;
	std::cout << "//////////////////////////////////////" << std::endl;
	std::cout << "//	SharedMemoryManager" << std::endl << std::endl;
	
	shmMemId.operator =( flowvr::DefaultMemId );
	SharedMemoryManager *shmman = new SharedMemoryManager( shmMemId );
	Allocator::setAllocator(shmman);
	Allocator::the()->attach();
	g_shmman = SharedMemoryManager::instance();
	if(g_shmman == NULL)
	{
		std::cerr << "could not create SharedMemoryManager with main ID " << shmMemId << "]" << std::endl;
		return -1;
	}
	g_shm = g_shmman->getMainMemoryArea();
	delShmMan = true;

	// state("testSharedMemoryCreation", testSharedMemoryCreation()); // irrelevant as the daemon does the allocation here 
	state("testZeroAllocation", testZeroAllocation());
	state("testHalfAllocation", testHalfAllocation());
	state("testFullAllocation", testFullAllocation()); // fails if memory fragmented
	state("testMoreThanFullAllocation", testMoreThanFullAllocation());
	state("testMoreThan32bitAllocation", testMoreThan32bitAllocation());
	
	state("testCauseNewArea", testCauseNewArea());
	state("testMPBufferFromUnopenedArea", testMPBufferFromUnopenedArea());
	
	if( delShmMan )
	{
		delShmMan = false;
		Allocator::the()->detach();
	}

	std::cout << std::endl;


	
	return 0;
}


size_t doAlloc( size_t size )
{
	SharedMemoryManager *shmman = SharedMemoryManager::instance();
	int        mem_id   = shmMemId.value();
	size_t  mem_size = size_t(shmSize.value());

	SharedMemoryArea *shm = shmman->openMemoryArea( mem_id, 1 );

	return shm->allocBuffer( size, 0 );
}

bool testSharedMemoryCreation()
{
	SharedMemoryManager *shmman = SharedMemoryManager::instance();
	int        mem_id   = shmMemId.value();
	size_t     mem_size = size_t(shmSize.value());

	SharedMemoryArea *shm = shmman->openMemoryArea( mem_id, 1 );


	bool bRet = false;
	if( shm->getFreeSize() <= mem_size )
		bRet = true;
	else
		std::cout << "shm->getFreeSize() returned ["
		          << shm->getFreeSize()
		          << "] ; expected at least: ["
		          << mem_size
		          << "]" << std::endl;

	return bRet;
}


bool testZeroAllocation()
{
	int        mem_id   = shmMemId.value();

	SharedMemoryManager *shmman = SharedMemoryManager::instance();
	SharedMemoryArea *shm = shmman->openMemoryArea( mem_id, 1 );

	size_t freeSize = shm->getFreeSize();
	size_t pos      = doAlloc(0);

	if(pos)
	{
		shm->addRef(pos);
		shm->freeRef(pos);
	}
	else
		return false;

	return ( shm->getFreeSize() == freeSize );
}

bool testHalfAllocation()
{
	SharedMemoryManager *shmman = SharedMemoryManager::instance();
	int        mem_id   = shmMemId.value();
	size_t  mem_size = size_t(shmSize.value());

	SharedMemoryArea *shm = shmman->openMemoryArea( mem_id, 1 );

	size_t freeSize = shm->getFreeSize();

	size_t pos = doAlloc( freeSize/2 );

	if(pos)
	{
		shm->addRef(pos);
		shm->freeRef(pos);
	}
	else
		return false;

	return ( shm->getFreeSize() == freeSize );
}

bool testFullAllocation()
{
	SharedMemoryManager *shmman = SharedMemoryManager::instance();
	int        mem_id   = shmMemId.value();
	size_t  mem_size = size_t(shmSize.value());

	SharedMemoryArea *shm = shmman->openMemoryArea( mem_id, 1 );

	size_t freeSize = shm->getFreeSize();

	size_t pos = shm->allocBuffer( freeSize, 0 );

	if(pos)
	{
		shm->addRef(pos);
		shm->freeRef(pos);
	}
	else
		return false;

	return ( shm->getFreeSize() == freeSize );
}

bool testMoreThanFullAllocation()
{
	SharedMemoryManager *shmman = SharedMemoryManager::instance();
	int        mem_id   = shmMemId.value();
	size_t  mem_size = size_t(shmSize.value());

	SharedMemoryArea *shm = shmman->openMemoryArea( mem_id, 1 );

	size_t freeSize = shm->getFreeSize();


	size_t pos = shm->allocBuffer( freeSize+1, 0 );

	if(pos)
	{
		shm->addRef(pos);
		shm->freeRef(pos);
	}
	else
		return true;

	return ( shm->getFreeSize() == freeSize );
}


bool testMoreThan32bitAllocation()
{
	SharedMemoryManager *shmman = SharedMemoryManager::instance();
	int        mem_id   = shmMemId.value();
	size_t  mem_size = size_t(shmSize.value());

	SharedMemoryArea *shm = shmman->openMemoryArea( mem_id, 1 );

	size_t freeSize = shm->getFreeSize();

	size_t desired = (size_t(0x7FFFFFFF) + size_t(1000) + size_t((2*4096)));

	if( freeSize < desired )
	{
		std::cerr << "shmseg must be greater than [" << desired << "] ; [is: " << freeSize << "]" << std::endl;
		return false;
	}


	size_t pos0 = shm->allocBuffer( 4096, 0 );
	if(pos0)
		shm->addRef(pos0);

	size_t posA = shm->allocBuffer( size_t(0x7FFFFFFF) + size_t(1000), 0 ); // alloc 2GB

	if( posA )
	{
		shm->addRef( posA );
	}

	size_t posB = shm->allocBuffer( 4096, 0 );

	if( posB )
	{
		shm->addRef(posB);
	}


	if( posA )
		shm->freeRef(posA);

	if( posB )
		shm->freeRef(posB);

	if( pos0 )
		shm->freeRef( pos0 );

	return ( shm->getFreeSize() == freeSize );
}




bool testCauseNewArea()
{
	
	flowvr::mem::SharedMemoryManager *shmman = flowvr::mem::SharedMemoryManager::instance();
	if ( NULL != shmman )
	{
		std::vector< flowvr::BufferWrite > bVec;
		size_t areaSize = shmman->getMainMemoryArea()->getSize();
		size_t freeSize = shmman->getMainMemoryArea()->getFreeSize();
		for ( int i = 0 ; i <= freeSize / ( 1 << 20 ) ; i++ )
		{
			bVec.push_back( shmman->alloc( 1 << 20 ) ); // 1 Mo
		}
		if ( bVec.back().valid() )
			return true;
	}
	return false;
}

bool testMPBufferFromUnopenedArea()
{
	bool success = false;
	
	// allocate memory to store the MPBuffer (no public constructor)
	MPBuffer *mpBuf = (MPBuffer*) operator new( sizeof(MPBuffer) );
	mpBuf->init();
	// BufferWrite to store a buffer from another area than the main one.
	BufferWrite Buf;
	
	// create a buffer from another area than the main one.
	flowvr::mem::SharedMemoryManager *shmman = flowvr::mem::SharedMemoryManager::instance();
	if ( NULL != shmman )
	{
		std::vector< flowvr::BufferWrite > bVec;
		size_t freeSize = shmman->getMainMemoryArea()->getFreeSize();
		for ( int i = 0 ; i <= freeSize / ( 1 << 20 ) ; i++ )
		{
			bVec.push_back( shmman->alloc( 1 << 20 ) ); // 1 Mo
		}
		if ( bVec.back().valid() )
		{
			Buf = bVec.back();	  // keep a copy to avoid its deallocation
			*mpBuf = bVec.back();   // cast it to MPBuffer
		}
		else
			return false;
	}
	
	// open it with an allocator that didn't open another area than the main one
	{
		SharedMemoryManager *oldShmMan = SharedMemoryManager::instance();
	
		// create temporary manager
		SharedMemoryManager *tmpShmMan = new SharedMemoryManager( shmMemId );
		Allocator::setAllocator( tmpShmMan );
		tmpShmMan->init();
		
		// perform the cast from MPBuffer cast
		{
			BufferWrite buf = *mpBuf;
			success = buf.valid();
			mpBuf->clear(); // clear MPBuff to avoid memory leak
		}
		
		// destroy temp allocator
		delete tmpShmMan;
		Allocator::setAllocator( oldShmMan );
	}
	
	operator delete( mpBuf );
	return success;
}




