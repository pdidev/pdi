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
* File: src/thread.cpp                                            *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/thread.h"
#include <signal.h>
#include <string.h>

#include "flowvr/topo.h"

#if FLOWVR_HAVE_CPU_NUMBER == 1
#include <sys/types.h>
#include <machine/cpu.h>
#endif


namespace flowvr
{

void Thread::resetState( void *arg )
{
	Thread* t = (Thread*) arg;
	if(t->detached)
	{
		t->tID = 0;
		t->detached = false;
	}
}

void* Thread::threadFunc( void *arg )
{
	 Thread* t = (Thread*) arg;

	 // that is a race, somehow...
	 if(t->sys_topology.getIsValid()) // user set topo at least once
		 t->setAffinity(t->thread_cpu_mask); // call with invalid topo
	                                         // will then use the system topology

	 // call run
	 void *ret = (void*)(long)t->run();

	 // note: this ** IS ** a race...
	 // but apparently there is no real clean way to do this
	 // pthread_cleanup_push does not work on this machine,
	 // the macros seem broken, and anyhow... that would be a
	 // race, too...
	 t->resetState(t);

	 // return exit-value on stack
	 pthread_exit(ret);
}
// class Thread: Thread utility class


Thread::Thread()
: tID(0)          // invalid thread handle
, detached(false) // not-detached by default
, sys_topology(false)  // create (invalid) topo
{
    thread_cpu_mask = hwloc_bitmap_alloc();
	// clear mask properly
    hwloc_bitmap_zero(thread_cpu_mask );
}

Thread::~Thread()
{
    hwloc_bitmap_free(thread_cpu_mask);
}

/// Start the thread
bool Thread::start()
{
  if(tID)
	  return true;
  return (pthread_create(&tID, NULL, &threadFunc, this)==0);
}

/// Wait for termination of this thread
int Thread::wait()
{
  if(detached)
	  return -1; // no join on a detached thread

  void* res = NULL;
  pthread_join(tID,&res);
  tID = 0;
  return (int)(long)res;
}

/// Kill this thread
void Thread::kill()
{
	if(tID)
		pthread_kill(tID,SIGKILL);
}

void Thread::detach()
{
	if(tID)
		(detached = (pthread_detach(tID)==0));
}


void Thread::terminate()
{
	if(tID)
		pthread_kill(tID,SIGTERM);
}


bool Thread::isDetached() const
{
	return detached;
}

bool Thread::equals( const Thread &oOther ) const
{
	return (pthread_equal(tID, oOther.tID) != 0) ;
}

Thread::eAffinityError Thread::setAffinity( hwloc_cpuset_t mask, const Topo &topo )
{
	// the passing of topo enables to re-use a topology
	// for a number of calls to setAffinity or start.
	// the creation of a topology is expensive, and we do not
	// want the thread to build one upon each start
	if(tID)
	{
		// this is a race ONLY when setAffinity() is called
		// VERY briefly after the thread was started. hmm...

		// anyhow, we do only copy the topology when it is
		// valid, otherwise we ignore it.
		if(topo.getIsValid())
			sys_topology = topo; // copy over

		// still have to test ;)
		if(sys_topology.getIsValid())
		{
			// ok, now we can bind
                    if(hwloc_set_cpubind(sys_topology.topology.t_topology,  mask, HWLOC_CPUBIND_THREAD ) == 0)
			{
				// save the current value of the mask set by the user
				// to match our internal state (for getAffinity()).
				memcpy(&thread_cpu_mask,&mask, sizeof(hwloc_cpuset_t));
				return ERR_OK; // that worked
			}
			else
				return ERR_CPUBIND_FAILED;
		}
		else
			return ERR_INVALID_TOPOLOGY;
	}
	else
	{
		// thread not created yet, copy the topo argument over
		// (yes, we expect it is valid ;)
		// and mark the topo-mask as set by the user
		sys_topology = topo;
		memcpy(&thread_cpu_mask,&mask, sizeof(hwloc_cpuset_t));
		return ERR_OK;
	}
}

hwloc_cpuset_t Thread::getAffinity() const
{
	return thread_cpu_mask;
}

int  Thread::getCurrentCPU()
{
#if FLOWVR_SCHED_HAVE_GETCPU == 1 // explicitly test for the value being 1
	/// @todo check: is there a pthreads call for this?
	return sched_getcpu(); // define me in subclass
#elif FLOWVR_HAVE_CPU_NUMBER == 1
	return cpu_number();
#else
	return -1;
#endif // FLOWVR_SCHED_HAVE_GETCPU
}

void Thread::yield()
{
#ifdef __APPLE__
  pthread_yield_np();
#else
  pthread_yield();
#endif /* __APPLE__ */
}

} // namespace flowvr
